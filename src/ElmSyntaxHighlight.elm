module ElmSyntaxHighlight exposing (SyntaxKind(..), for)

import ElmParserLenient
import ElmSyntax
import ParserFast
import RangeDict exposing (RangeDict)


type SyntaxKind
    = Type
    | TypeVariable
    | Variant
    | Field
    | ModuleNameOrAlias
    | Variable
    | Flow
    | Comment
    | String
    | Number
    | VariableDeclaration
    | KeySymbol


locationAddColumn : Int -> ElmSyntax.Location -> ElmSyntax.Location
locationAddColumn columnPlus location =
    { location | column = location.column + columnPlus }


qualifiedSyntaxKindMap :
    SyntaxKind
    ->
        ElmSyntax.Node
            { qualification : ElmSyntax.ModuleName
            , name : String
            }
    -> RangeDict SyntaxKind
qualifiedSyntaxKindMap kind qualified =
    case qualified.value.qualification of
        [] ->
            RangeDict.singleton qualified.range kind

        _ :: _ ->
            RangeDict.empty
                |> RangeDict.insert
                    { start = qualified.range.start
                    , end = qualified.range.end |> locationAddColumn (-(qualified.value.name |> String.length) - 1)
                    }
                    ModuleNameOrAlias
                |> RangeDict.insert
                    { start = qualified.range.end |> locationAddColumn (-(qualified.value.name |> String.length) - 1)
                    , end = qualified.range.end |> locationAddColumn -(qualified.value.name |> String.length)
                    }
                    KeySymbol
                |> RangeDict.insert
                    { start = qualified.range.end |> locationAddColumn -(qualified.value.name |> String.length)
                    , end = qualified.range.end
                    }
                    kind


patternSyntaxKindMap : ElmSyntax.Node ElmSyntax.Pattern -> RangeDict SyntaxKind
patternSyntaxKindMap patternNode =
    case patternNode.value of
        ElmSyntax.PatternIgnored ->
            RangeDict.singleton patternNode.range KeySymbol

        ElmSyntax.PatternVariable _ ->
            RangeDict.singleton patternNode.range Variable

        ElmSyntax.PatternUnit ->
            RangeDict.singleton patternNode.range Variant

        ElmSyntax.PatternChar _ ->
            RangeDict.singleton patternNode.range String

        ElmSyntax.PatternString _ ->
            RangeDict.singleton
                (-- TODO strip quotes
                 patternNode.range
                )
                String

        ElmSyntax.PatternInt _ ->
            RangeDict.singleton patternNode.range Number

        ElmSyntax.PatternHex _ ->
            RangeDict.singleton patternNode.range Number

        ElmSyntax.PatternTuple parts ->
            parts.part0
                |> patternSyntaxKindMap
                |> RangeDict.union
                    (parts.part1 |> patternSyntaxKindMap)

        ElmSyntax.PatternTriple parts ->
            parts.part0
                |> patternSyntaxKindMap
                |> RangeDict.union
                    (parts.part1 |> patternSyntaxKindMap)
                |> RangeDict.union
                    (parts.part2 |> patternSyntaxKindMap)

        ElmSyntax.PatternRecord fieldVariables ->
            fieldVariables
                |> RangeDict.mapFromList (\variable -> ( variable.range, Variable ))

        ElmSyntax.PatternListCons listCons ->
            RangeDict.union
                (listCons.head |> patternSyntaxKindMap)
                (listCons.tail |> patternSyntaxKindMap)
                |> RangeDict.insert
                    (-- TODO add to AST
                     { start = listCons.head.range |> .end |> locationAddColumn 1
                     , end = listCons.tail.range |> .start |> locationAddColumn -1
                     }
                    )
                    Variant

        ElmSyntax.PatternListExact elements ->
            elements |> RangeDict.unionFromListMap patternSyntaxKindMap

        ElmSyntax.PatternVariant patternVariant ->
            let
                qualified : { qualification : ElmSyntax.ModuleName, name : String }
                qualified =
                    { qualification = patternVariant.qualification
                    , name = patternVariant.name
                    }
            in
            RangeDict.union
                (ElmSyntax.Node
                    { start = patternNode.range.start
                    , end = patternNode.range.start |> locationAddColumn (qualified |> qualifiedRangeLength)
                    }
                    qualified
                    |> qualifiedSyntaxKindMap Variant
                )
                (patternVariant.values
                    |> RangeDict.unionFromListMap patternSyntaxKindMap
                )

        ElmSyntax.PatternAs patternAs ->
            patternAs.pattern
                |> patternSyntaxKindMap
                |> RangeDict.insert
                    (-- TODO do not rely on elm-format-ed code
                     { start = patternAs.pattern.range.end |> locationAddColumn 1
                     , end = patternAs.variable.range.start |> locationAddColumn -1
                     }
                    )
                    KeySymbol
                |> RangeDict.insert patternAs.variable.range Variable

        ElmSyntax.PatternParenthesized inner ->
            inner |> patternSyntaxKindMap


qualifiedRangeLength : { qualification : ElmSyntax.ModuleName, name : String } -> Int
qualifiedRangeLength qualified =
    case qualified.qualification of
        [] ->
            qualified.name |> String.length

        moduleNamePart0 :: moduleNamePart1Up ->
            List.foldl (\part soFar -> (part |> String.length) + 1 + soFar)
                ((moduleNamePart0 |> String.length) + 1 + (qualified.name |> String.length))
                moduleNamePart1Up


expressionSyntaxKindMap :
    { rawSourceCode : List String, commentRanges : List ElmSyntax.Range }
    -> ElmSyntax.Node ElmSyntax.Expression
    -> RangeDict SyntaxKind
expressionSyntaxKindMap context expressionNode =
    case expressionNode.value of
        ElmSyntax.ExpressionUnit ->
            RangeDict.singleton expressionNode.range Variant

        ElmSyntax.ExpressionCall applicationParts ->
            applicationParts
                |> RangeDict.unionFromListMap
                    (\part -> part |> expressionSyntaxKindMap context)

        ElmSyntax.ExpressionInfixOperation infixOperation ->
            let
                leftRightSyntaxKindMap : RangeDict SyntaxKind
                leftRightSyntaxKindMap =
                    RangeDict.union
                        (infixOperation.left |> expressionSyntaxKindMap context)
                        (infixOperation.right |> expressionSyntaxKindMap context)

                operatorRange : ElmSyntax.Range
                operatorRange =
                    -- TODO do not rely on elm-format-ed code
                    { start = infixOperation.right.range.start |> locationAddColumn -2
                    , end = infixOperation.right.range.start |> locationAddColumn -1
                    }
            in
            case infixOperation.operator of
                "|>" ->
                    leftRightSyntaxKindMap
                        |> RangeDict.insert operatorRange Flow

                "<|" ->
                    leftRightSyntaxKindMap
                        |> RangeDict.insert operatorRange Flow

                _ ->
                    leftRightSyntaxKindMap
                        |> -- hmm
                           RangeDict.insert operatorRange KeySymbol

        ElmSyntax.ExpressionReference reference ->
            { range = expressionNode.range, value = reference }
                |> qualifiedSyntaxKindMap
                    (if nameIsUppercase reference.name then
                        Variant

                     else
                        Variable
                    )

        ElmSyntax.ExpressionIfThenElse ifThenElse ->
            ifThenElse.condition
                |> expressionSyntaxKindMap context
                |> RangeDict.union (ifThenElse.onTrue |> expressionSyntaxKindMap context)
                |> RangeDict.union (ifThenElse.onFalse |> expressionSyntaxKindMap context)
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = { line = expressionNode.range.start.line, column = expressionNode.range.start.column + 2 }
                    }
                    Flow
                |> RangeDict.insert
                    (-- TODO add to syntax tree
                     "then"
                        |> tokenFindRangeIn
                            { start = ifThenElse.condition.range.end
                            , end = ifThenElse.onTrue.range.start
                            }
                            context
                        |> -- if there is a bug
                           Maybe.withDefault ElmSyntax.rangeEmpty
                    )
                    Flow
                |> RangeDict.insert
                    (-- TODO add to syntax tree
                     "else"
                        |> tokenFindRangeIn
                            { start = ifThenElse.onTrue.range.end
                            , end = ifThenElse.onFalse.range.start
                            }
                            context
                        |> -- if there is a bug
                           Maybe.withDefault ElmSyntax.rangeEmpty
                    )
                    Flow

        ElmSyntax.ExpressionOperatorFunction _ ->
            RangeDict.singleton expressionNode.range Variable

        ElmSyntax.ExpressionInteger _ ->
            RangeDict.singleton expressionNode.range Number

        ElmSyntax.ExpressionHex _ ->
            RangeDict.singleton expressionNode.range Number

        ElmSyntax.ExpressionFloat _ ->
            RangeDict.singleton expressionNode.range Number

        ElmSyntax.ExpressionNegation inner ->
            inner
                |> expressionSyntaxKindMap context
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = expressionNode.range.start |> locationAddColumn 1
                    }
                    -- hmm
                    KeySymbol

        ElmSyntax.ExpressionString _ ->
            -- TODO respect """
            RangeDict.singleton
                { start = expressionNode.range.start |> locationAddColumn 1
                , end = expressionNode.range.end |> locationAddColumn -1
                }
                String

        ElmSyntax.ExpressionChar _ ->
            RangeDict.singleton
                { start = expressionNode.range.start |> locationAddColumn 1
                , end = expressionNode.range.end |> locationAddColumn -1
                }
                String

        ElmSyntax.ExpressionTuple parts ->
            parts.part0
                |> expressionSyntaxKindMap context
                |> RangeDict.union (parts.part1 |> expressionSyntaxKindMap context)

        ElmSyntax.ExpressionTriple parts ->
            parts.part0
                |> expressionSyntaxKindMap context
                |> RangeDict.union (parts.part1 |> expressionSyntaxKindMap context)
                |> RangeDict.union (parts.part2 |> expressionSyntaxKindMap context)

        ElmSyntax.ExpressionParenthesized inner ->
            inner |> expressionSyntaxKindMap context

        ElmSyntax.ExpressionLetIn letIn ->
            RangeDict.union
                (letIn.declarations
                    |> RangeDict.unionFromListMap (letDeclarationSyntaxKindMap context)
                )
                (letIn.result |> expressionSyntaxKindMap context)
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = { line = expressionNode.range.start.line, column = expressionNode.range.start.column + 3 }
                    }
                    KeySymbol
                |> RangeDict.insert
                    (case letIn.declarations |> listLast of
                        Nothing ->
                            -- invalid syntax
                            ElmSyntax.rangeEmpty

                        Just lastDeclarationNode ->
                            -- TODO store in syntax tree
                            "in"
                                |> tokenFindRangeIn
                                    { start = lastDeclarationNode.range.end
                                    , end = letIn.result.range.start
                                    }
                                    context
                                |> -- if there is a bug
                                   Maybe.withDefault ElmSyntax.rangeEmpty
                    )
                    KeySymbol

        ElmSyntax.ExpressionCaseOf caseOf ->
            RangeDict.union
                (caseOf.expression |> expressionSyntaxKindMap context)
                (caseOf.cases
                    |> RangeDict.unionFromListMap
                        (\syntaxCase ->
                            RangeDict.union
                                (syntaxCase.pattern |> patternSyntaxKindMap)
                                (syntaxCase.result |> expressionSyntaxKindMap context)
                                |> RangeDict.insert
                                    (-- TODO do not rely on elm-format-ed code
                                     { start =
                                        { column = syntaxCase.pattern.range.end.column + 1
                                        , line = syntaxCase.pattern.range.end.line
                                        }
                                     , end =
                                        { column = syntaxCase.pattern.range.end.column + 3
                                        , line = syntaxCase.pattern.range.end.line
                                        }
                                     }
                                    )
                                    Flow
                        )
                )
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = { line = expressionNode.range.start.line, column = expressionNode.range.start.column + 4 }
                    }
                    Flow
                |> RangeDict.insert
                    (case caseOf.cases of
                        [] ->
                            -- invalid syntax
                            ElmSyntax.rangeEmpty

                        firstCase :: _ ->
                            -- TODO store in syntax tree
                            "of"
                                |> tokenFindRangeIn
                                    { start = caseOf.expression.range.end
                                    , end = firstCase.pattern.range.start
                                    }
                                    context
                                |> -- if there is a bug
                                   Maybe.withDefault ElmSyntax.rangeEmpty
                    )
                    Flow

        ElmSyntax.ExpressionLambda lambda ->
            RangeDict.union
                (lambda.parameters
                    |> RangeDict.unionFromListMap patternSyntaxKindMap
                )
                (lambda.result |> expressionSyntaxKindMap context)
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = { line = expressionNode.range.start.line, column = expressionNode.range.start.column + 1 }
                    }
                    Flow
                |> RangeDict.insert
                    (case lambda.parameters |> listLast of
                        Just lastParameter ->
                            -- TODO do not rely on elm-format-ed code
                            { start =
                                { column = lastParameter.range.end.column + 1
                                , line = lastParameter.range.end.line
                                }
                            , end =
                                { column = lastParameter.range.end.column + 3
                                , line = lastParameter.range.end.line
                                }
                            }

                        Nothing ->
                            -- invalid syntax
                            ElmSyntax.rangeEmpty
                    )
                    Flow

        ElmSyntax.ExpressionRecord fields ->
            fields
                |> RangeDict.unionFromListMap
                    (\field ->
                        field.value
                            |> expressionSyntaxKindMap context
                            |> RangeDict.insert field.name.range Field
                            |> RangeDict.insert
                                (-- TODO store in syntax tree
                                 { start = field.name.range.end |> locationAddColumn 1
                                 , end = field.name.range.end |> locationAddColumn 2
                                 }
                                )
                                KeySymbol
                    )

        ElmSyntax.ExpressionList elements ->
            elements
                |> RangeDict.unionFromListMap
                    (\element ->
                        element |> expressionSyntaxKindMap context
                    )

        ElmSyntax.ExpressionRecordAccess recordAccess ->
            recordAccess.record
                |> expressionSyntaxKindMap context
                |> RangeDict.insert
                    { start = recordAccess.field.range.start |> locationAddColumn -1
                    , end = recordAccess.field.range.start
                    }
                    KeySymbol
                |> RangeDict.insert recordAccess.field.range Field

        ElmSyntax.ExpressionRecordAccessFunction _ ->
            RangeDict.singleton
                { start = expressionNode.range.start |> locationAddColumn 1
                , end = expressionNode.range.end
                }
                Field
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = expressionNode.range.start |> locationAddColumn 1
                    }
                    KeySymbol

        ElmSyntax.ExpressionRecordUpdate recordUpdate ->
            recordUpdate.fields
                |> RangeDict.unionFromListMap
                    (\field ->
                        field.value
                            |> expressionSyntaxKindMap context
                            |> RangeDict.insert field.name.range Field
                            |> RangeDict.insert
                                (-- TODO store in syntax tree
                                 { start = field.name.range.end |> locationAddColumn 1
                                 , end = field.name.range.end |> locationAddColumn 2
                                 }
                                )
                                KeySymbol
                    )
                |> RangeDict.insert recordUpdate.recordVariable.range Variable


nameIsUppercase : String -> Bool
nameIsUppercase string =
    case string |> String.uncons of
        Just ( firstChar, _ ) ->
            firstChar |> Char.isUpper

        Nothing ->
            False


tokenFindRangeIn :
    ElmSyntax.Range
    ->
        { rawSourceCode : List String
        , commentRanges : List ElmSyntax.Range
        }
    -> String
    -> Maybe ElmSyntax.Range
tokenFindRangeIn searchRange context token =
    let
        searchLines : List String
        searchLines =
            context.rawSourceCode
                |> stringLinesSlice searchRange
                |> String.lines

        operatorStartLocationFound : Maybe ElmSyntax.Location
        operatorStartLocationFound =
            searchLines
                |> List.indexedMap Tuple.pair
                |> listFindMap
                    (\( searchLineIndex, searchLine ) ->
                        String.indexes token searchLine
                            |> listFindMap
                                (\operatorOffset ->
                                    let
                                        operatorStartLocation : ElmSyntax.Location
                                        operatorStartLocation =
                                            case searchLineIndex of
                                                0 ->
                                                    { line = searchRange.start.line
                                                    , column = searchRange.start.column + operatorOffset
                                                    }

                                                searchLineAfterFirstIndex ->
                                                    { line = searchRange.start.line + searchLineAfterFirstIndex
                                                    , column = operatorOffset + 1
                                                    }

                                        isPartOfComment : Bool
                                        isPartOfComment =
                                            List.any
                                                (\commentRange ->
                                                    rangeContainsLocation operatorStartLocation commentRange
                                                )
                                                context.commentRanges
                                    in
                                    if isPartOfComment then
                                        Nothing

                                    else
                                        Just operatorStartLocation
                                )
                    )
    in
    case operatorStartLocationFound of
        Just operatorStartLocation ->
            Just
                { start = operatorStartLocation
                , end =
                    { line = operatorStartLocation.line
                    , column = operatorStartLocation.column + String.length token
                    }
                }

        -- there's a bug somewhere
        Nothing ->
            Nothing


rangeContainsLocation : ElmSyntax.Location -> ElmSyntax.Range -> Bool
rangeContainsLocation location range =
    case ElmSyntax.locationCompare range.start location of
        GT ->
            False

        EQ ->
            False

        LT ->
            case ElmSyntax.locationCompare range.end location of
                GT ->
                    True

                LT ->
                    False

                EQ ->
                    False


letDeclarationSyntaxKindMap :
    { rawSourceCode : List String, commentRanges : List ElmSyntax.Range }
    -> ElmSyntax.Node ElmSyntax.LetDeclaration
    -> RangeDict SyntaxKind
letDeclarationSyntaxKindMap context letDeclarationNode =
    case letDeclarationNode.value of
        ElmSyntax.LetDestructuring letDestructuring ->
            RangeDict.union
                (letDestructuring.pattern |> patternSyntaxKindMap)
                (letDestructuring.expression |> expressionSyntaxKindMap context)
                |> RangeDict.insert
                    (let
                        tokenBeforeEqualsEnd : ElmSyntax.Location
                        tokenBeforeEqualsEnd =
                            letDestructuring.pattern.range.end
                     in
                     { start = { column = tokenBeforeEqualsEnd.column + 1, line = tokenBeforeEqualsEnd.line }
                     , end = { column = tokenBeforeEqualsEnd.column + 2, line = tokenBeforeEqualsEnd.line }
                     }
                    )
                    KeySymbol

        ElmSyntax.LetValueOrFunctionDeclaration fnDeclaration ->
            RangeDict.union
                (case fnDeclaration.signature of
                    Just functionSignature ->
                        functionSignature |> signatureSyntaxKindMap

                    Nothing ->
                        RangeDict.empty
                )
                (RangeDict.union
                    (fnDeclaration.result |> expressionSyntaxKindMap context)
                    (fnDeclaration.parameters |> RangeDict.unionFromListMap patternSyntaxKindMap)
                    |> RangeDict.insert fnDeclaration.implementationNameRange
                        Variable
                    |> RangeDict.insert
                        (let
                            tokenBeforeEqualsEnd : ElmSyntax.Location
                            tokenBeforeEqualsEnd =
                                case fnDeclaration.parameters |> listLast of
                                    Just lastParameter ->
                                        lastParameter.range.end

                                    Nothing ->
                                        fnDeclaration.implementationNameRange.end
                         in
                         { start = { column = tokenBeforeEqualsEnd.column + 1, line = tokenBeforeEqualsEnd.line }
                         , end = { column = tokenBeforeEqualsEnd.column + 2, line = tokenBeforeEqualsEnd.line }
                         }
                        )
                        KeySymbol
                )


stringLinesSlice : ElmSyntax.Range -> List String -> String
stringLinesSlice rangeToExtract lines =
    case List.drop (rangeToExtract.start.line - 1) lines of
        [] ->
            ""

        firstLine :: rest ->
            if rangeToExtract.start.line == rangeToExtract.end.line then
                String.slice
                    (rangeToExtract.start.column - 1)
                    (rangeToExtract.end.column - 1)
                    firstLine

            else
                let
                    restLinesTaken : { linesTaken : String, lastLine : Maybe String }
                    restLinesTaken =
                        linesTake (rangeToExtract.end.line - rangeToExtract.start.line - 1) rest ""
                in
                String.dropLeft (rangeToExtract.start.column - 1) firstLine
                    ++ restLinesTaken.linesTaken
                    ++ (case restLinesTaken.lastLine of
                            Just lastLine ->
                                "\n" ++ String.left (rangeToExtract.end.column - 1) lastLine

                            Nothing ->
                                ""
                       )


linesTake : Int -> List String -> String -> { linesTaken : String, lastLine : Maybe String }
linesTake n lines linesTaken =
    if n <= 0 then
        { linesTaken = linesTaken, lastLine = List.head lines }

    else
        case lines of
            [] ->
                { linesTaken = linesTaken, lastLine = Nothing }

            line :: rest ->
                linesTake (n - 1)
                    rest
                    (linesTaken ++ "\n" ++ line)


typeAnnotationSyntaxKindMap : ElmSyntax.Node ElmSyntax.TypeAnnotation -> RangeDict SyntaxKind
typeAnnotationSyntaxKindMap typeNode =
    -- IGNORE TCO
    case typeNode.value of
        ElmSyntax.TypeAnnotationUnit ->
            RangeDict.singleton typeNode.range Type

        ElmSyntax.TypeAnnotationVariable _ ->
            RangeDict.singleton typeNode.range TypeVariable

        ElmSyntax.TypeAnnotationParenthesized inParens ->
            typeAnnotationSyntaxKindMap inParens

        ElmSyntax.TypeAnnotationTuple parts ->
            parts.part0
                |> typeAnnotationSyntaxKindMap
                |> RangeDict.union
                    (parts.part1 |> typeAnnotationSyntaxKindMap)

        ElmSyntax.TypeAnnotationTriple parts ->
            parts.part0
                |> typeAnnotationSyntaxKindMap
                |> RangeDict.union
                    (parts.part1 |> typeAnnotationSyntaxKindMap)
                |> RangeDict.union
                    (parts.part2 |> typeAnnotationSyntaxKindMap)

        ElmSyntax.TypeAnnotationConstruct typeConstruct ->
            (typeConstruct.reference |> qualifiedSyntaxKindMap Type)
                |> RangeDict.union
                    (typeConstruct.arguments
                        |> RangeDict.unionFromListMap typeAnnotationSyntaxKindMap
                    )

        ElmSyntax.TypeAnnotationRecord fields ->
            fields
                |> RangeDict.unionFromListMap
                    (\field ->
                        field.value.value
                            |> typeAnnotationSyntaxKindMap
                            |> RangeDict.insert field.value.name.range Field
                            |> RangeDict.insert
                                (-- TODO do not rely on elm-format-ed code
                                 { start = field.value.name.range.end |> locationAddColumn 1
                                 , end = field.value.name.range.end |> locationAddColumn 2
                                 }
                                )
                                KeySymbol
                    )

        ElmSyntax.TypeAnnotationRecordExtension typeRecordExtension ->
            case typeRecordExtension.fields.value of
                -- invalid syntax
                [] ->
                    RangeDict.singleton typeRecordExtension.recordVariable.range
                        Variable

                additionalField0 :: additionalField1Up ->
                    (additionalField0 :: additionalField1Up)
                        |> RangeDict.unionFromListMap (\field -> field.value.value |> typeAnnotationSyntaxKindMap)
                        |> RangeDict.insert typeRecordExtension.recordVariable.range
                            Variable
                        |> RangeDict.insert
                            (-- TODO do not rely on elm-format-ed code
                             { start = additionalField0.range.start |> locationAddColumn -2
                             , end = additionalField0.range.start |> locationAddColumn -1
                             }
                            )
                            KeySymbol

        ElmSyntax.TypeAnnotationFunction typeFunction ->
            RangeDict.union
                (typeFunction.input |> typeAnnotationSyntaxKindMap)
                (typeFunction.output |> typeAnnotationSyntaxKindMap)
                |> RangeDict.insert
                    (-- TODO do not rely on elm-format-ed code
                     { start = typeFunction.output.range.start |> locationAddColumn -2
                     , end = typeFunction.output.range.start |> locationAddColumn -1
                     }
                    )
                    KeySymbol


signatureSyntaxKindMap :
    { name : ElmSyntax.Node String
    , type_ : ElmSyntax.Node ElmSyntax.TypeAnnotation
    }
    -> RangeDict SyntaxKind
signatureSyntaxKindMap signature =
    signature.type_
        |> typeAnnotationSyntaxKindMap
        |> RangeDict.insert signature.name.range
            VariableDeclaration


declarationSyntaxKindMap :
    { rawSourceCode : List String, commentRanges : List ElmSyntax.Range }
    -> ElmSyntax.Node ElmSyntax.Declaration
    -> RangeDict SyntaxKind
declarationSyntaxKindMap context declarationNode =
    case declarationNode.value of
        ElmSyntax.DeclarationValueOrFunction fnDeclaration ->
            (case fnDeclaration.documentation of
                Nothing ->
                    RangeDict.empty

                Just documentationNode ->
                    commentSyntaxKindMap documentationNode
            )
                |> RangeDict.union
                    (case fnDeclaration.signature of
                        Just functionSignature ->
                            functionSignature |> signatureSyntaxKindMap

                        Nothing ->
                            RangeDict.empty
                    )
                |> RangeDict.union
                    (fnDeclaration.result |> expressionSyntaxKindMap context)
                |> RangeDict.union
                    (fnDeclaration.parameters |> RangeDict.unionFromListMap patternSyntaxKindMap)
                |> RangeDict.insert fnDeclaration.implementationNameRange
                    VariableDeclaration
                |> RangeDict.insert
                    (let
                        tokenBeforeEqualsEnd : ElmSyntax.Location
                        tokenBeforeEqualsEnd =
                            case fnDeclaration.parameters |> listLast of
                                Just lastParameterNode ->
                                    lastParameterNode.range.end

                                Nothing ->
                                    fnDeclaration.implementationNameRange.end
                     in
                     { start = { column = tokenBeforeEqualsEnd.column + 1, line = tokenBeforeEqualsEnd.line }
                     , end = { column = tokenBeforeEqualsEnd.column + 2, line = tokenBeforeEqualsEnd.line }
                     }
                    )
                    KeySymbol

        ElmSyntax.DeclarationTypeAlias aliasTypeDeclaration ->
            (case aliasTypeDeclaration.documentation of
                Nothing ->
                    RangeDict.empty

                Just documentationNode ->
                    commentSyntaxKindMap documentationNode
            )
                |> RangeDict.union
                    (aliasTypeDeclaration.type_ |> typeAnnotationSyntaxKindMap)
                |> RangeDict.union
                    (aliasTypeDeclaration.parameters
                        |> RangeDict.mapFromList
                            (\variableNode ->
                                ( variableNode.range, Variable )
                            )
                    )
                |> RangeDict.insert
                    aliasTypeDeclaration.name.range
                    Type
                |> RangeDict.insert
                    { start = declarationNode.range.start
                    , end = { line = declarationNode.range.start.line, column = declarationNode.range.start.column + 10 }
                    }
                    KeySymbol
                |> RangeDict.insert
                    (let
                        tokenBeforeEqualsEnd : ElmSyntax.Location
                        tokenBeforeEqualsEnd =
                            case aliasTypeDeclaration.parameters |> listLast of
                                Just lastParameter ->
                                    lastParameter.range.end

                                Nothing ->
                                    aliasTypeDeclaration.name.range.end
                     in
                     { start = { column = tokenBeforeEqualsEnd.column + 1, line = tokenBeforeEqualsEnd.line }
                     , end = { column = tokenBeforeEqualsEnd.column + 2, line = tokenBeforeEqualsEnd.line }
                     }
                    )
                    KeySymbol

        ElmSyntax.DeclarationChoiceType choiceTypeDeclaration ->
            (case choiceTypeDeclaration.documentation of
                Nothing ->
                    RangeDict.empty

                Just documentationNode ->
                    commentSyntaxKindMap documentationNode
            )
                |> RangeDict.union
                    (choiceTypeDeclaration.variants
                        |> RangeDict.unionFromListMap
                            (\variantNode ->
                                let
                                    variantNameRange : ElmSyntax.Range
                                    variantNameRange =
                                        variantNode.value.name.range
                                in
                                variantNode.value.values
                                    |> RangeDict.unionFromListMap typeAnnotationSyntaxKindMap
                                    |> RangeDict.insert variantNameRange Variant
                                    |> RangeDict.insert
                                        { start = { column = variantNameRange.start.column - 2, line = variantNameRange.start.line }
                                        , end = { column = variantNameRange.start.column - 1, line = variantNameRange.start.line }
                                        }
                                        KeySymbol
                            )
                    )
                |> RangeDict.union
                    (choiceTypeDeclaration.parameters
                        |> RangeDict.mapFromList
                            (\parameterNode ->
                                ( parameterNode.range, Variable )
                            )
                    )
                |> RangeDict.insert
                    choiceTypeDeclaration.name.range
                    Type
                |> RangeDict.insert
                    { start = declarationNode.range.start
                    , end =
                        { line = declarationNode.range.start.line
                        , column = declarationNode.range.start.column + 4
                        }
                    }
                    KeySymbol

        ElmSyntax.DeclarationPort signature ->
            signature
                |> signatureSyntaxKindMap
                |> RangeDict.insert
                    { start = declarationNode.range.start
                    , end =
                        { line = declarationNode.range.start.line
                        , column = declarationNode.range.start.column + 4
                        }
                    }
                    KeySymbol

        ElmSyntax.DeclarationOperator _ ->
            RangeDict.empty


{-| Assumes `elm-format`ed code
-}
for :
    { source : String, syntax : ElmSyntax.Module }
    ->
        List
            { range : ElmSyntax.Range
            , syntaxKind : SyntaxKind
            }
for elmModule =
    let
        rawSourceCodeLines : List String
        rawSourceCodeLines =
            elmModule.source |> String.lines

        segmentsReverse : List { range : ElmSyntax.Range, syntaxKind : SyntaxKind }
        segmentsReverse =
            RangeDict.unionFromListMap identity
                [ elmModule.syntax.moduleDefinition |> moduleHeaderSyntaxKindMap
                , elmModule.syntax.comments
                    |> RangeDict.unionFromListMap commentSyntaxKindMap
                , elmModule.syntax.imports
                    |> RangeDict.unionFromListMap importSyntaxKindMap
                , elmModule.syntax.declarations
                    |> RangeDict.unionFromListMap
                        (declarationSyntaxKindMap
                            { rawSourceCode = rawSourceCodeLines
                            , commentRanges =
                                elmModule.syntax.comments |> List.map .range
                            }
                        )
                ]
                |> RangeDict.foldl
                    (\range syntaxKind soFar ->
                        { range = range, syntaxKind = syntaxKind }
                            :: soFar
                    )
                    []
    in
    segmentsReverse
        |> listReverseAndMap
            (\segment ->
                { range = segment.range
                , syntaxKind = segment.syntaxKind
                }
            )


listReverseAndMap : (a -> b) -> List a -> List b
listReverseAndMap elementChange listReverse =
    listReverse
        |> List.foldl
            (\element soFar -> (element |> elementChange) :: soFar)
            []


moduleHeaderSyntaxKindMap : ElmSyntax.Node ElmSyntax.ModuleHeader -> RangeDict SyntaxKind
moduleHeaderSyntaxKindMap moduleHeaderNode =
    moduleHeaderNode.value.exposing_
        |> exposingSyntaxKindMap
        |> RangeDict.insert moduleHeaderNode.value.moduleName.range
            ModuleNameOrAlias
        |> RangeDict.union
            (case moduleHeaderNode.value.specific of
                Nothing ->
                    RangeDict.singleton
                        { start = moduleHeaderNode.range.start
                        , end =
                            { line = moduleHeaderNode.range.start.line
                            , column = moduleHeaderNode.range.start.column + 6
                            }
                        }
                        KeySymbol

                Just (ElmSyntax.ModuleHeaderSpecificPort portModuleHeaderSpecific) ->
                    RangeDict.singleton
                        { start = moduleHeaderNode.range.start
                        , end =
                            { line = moduleHeaderNode.range.start.line
                            , column = moduleHeaderNode.range.start.column + 4
                            }
                        }
                        KeySymbol
                        |> RangeDict.insert
                            portModuleHeaderSpecific.moduleKeywordRange
                            KeySymbol

                Just (ElmSyntax.ModuleHeaderSpecificEffect effectModuleHeaderSpecific) ->
                    RangeDict.singleton
                        { start = moduleHeaderNode.range.start
                        , end =
                            { line = moduleHeaderNode.range.start.line
                            , column = moduleHeaderNode.range.start.column + 6
                            }
                        }
                        KeySymbol
                        |> RangeDict.insert
                            effectModuleHeaderSpecific.moduleKeywordRange
                            KeySymbol
            )


commentSyntaxKindMap : ElmSyntax.Node String -> RangeDict SyntaxKind
commentSyntaxKindMap commentNode =
    if commentNode.value |> String.startsWith "--: " then
        case
            (commentNode.value |> String.dropLeft 4)
                |> ElmParserLenient.run ElmParserLenient.type_
        of
            Just parsedType ->
                parsedType.syntax
                    |> typeAnnotationSyntaxKindMap

            Nothing ->
                plainCommentSyntaxKindMap commentNode

    else
        plainCommentSyntaxKindMap commentNode


plainCommentSyntaxKindMap : ElmSyntax.Node String -> RangeDict SyntaxKind
plainCommentSyntaxKindMap commentNode =
    commentNode.value
        |> String.lines
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( innerRow, innerRowLine ) soFar ->
                let
                    line : Int
                    line =
                        commentNode.range.start.line + innerRow
                in
                soFar
                    |> RangeDict.insert
                        { start =
                            if innerRow == 0 then
                                commentNode.range.start

                            else
                                { line = line, column = 1 }
                        , end =
                            if line == commentNode.range.end.line then
                                commentNode.range.end

                            else
                                { line = line, column = 1 + (innerRowLine |> String.length) }
                        }
                        Comment
            )
            RangeDict.empty


importSyntaxKindMap : ElmSyntax.Node ElmSyntax.Import -> RangeDict SyntaxKind
importSyntaxKindMap importNode =
    RangeDict.union
        ((case importNode.value.exposing_ of
            Nothing ->
                RangeDict.empty

            Just exposing_ ->
                exposing_ |> exposingSyntaxKindMap
         )
            |> RangeDict.insert
                { start = importNode.range.start
                , end =
                    { line = importNode.range.start.line
                    , column = importNode.range.start.column + 6
                    }
                }
                KeySymbol
            |> RangeDict.insert importNode.value.moduleName.range ModuleNameOrAlias
        )
        (case importNode.value.moduleAlias of
            Nothing ->
                RangeDict.empty

            Just aliasNode ->
                RangeDict.singleton aliasNode.range ModuleNameOrAlias
                    |> RangeDict.insert
                        { start =
                            { column = aliasNode.range.start.column - 3
                            , line = aliasNode.range.start.line
                            }
                        , end =
                            { column = aliasNode.range.start.column - 1
                            , line = aliasNode.range.start.line
                            }
                        }
                        KeySymbol
        )


rangeAddRow : Int -> ElmSyntax.Range -> ElmSyntax.Range
rangeAddRow rowPlus range =
    { start = range.start |> locationAddRow rowPlus
    , end = range.end |> locationAddRow rowPlus
    }


locationAddRow : Int -> ElmSyntax.Location -> ElmSyntax.Location
locationAddRow rowPlus location =
    { line = location.line + rowPlus, column = location.column }


rangeAddColumn : Int -> ElmSyntax.Range -> ElmSyntax.Range
rangeAddColumn columnPlus range =
    { start = range.start |> locationAddColumn columnPlus
    , end = range.end |> locationAddColumn columnPlus
    }


exposingSyntaxKindMap : ElmSyntax.Node ElmSyntax.Exposing -> RangeDict SyntaxKind
exposingSyntaxKindMap exposingNode =
    let
        exposingKeywordRange : ElmSyntax.Range
        exposingKeywordRange =
            { start =
                { column = exposingNode.range.start.column
                , line = exposingNode.range.start.line
                }
            , end =
                { column = exposingNode.range.start.column + 8
                , line = exposingNode.range.start.line
                }
            }
    in
    case exposingNode.value of
        ElmSyntax.ExposingAll _ ->
            RangeDict.singleton exposingKeywordRange KeySymbol

        ElmSyntax.ExposingExplicit exposedMembers ->
            exposedMembers
                |> RangeDict.unionFromListMap
                    (\exposedMemberNode ->
                        case exposedMemberNode.value of
                            ElmSyntax.ExposeOperator _ ->
                                RangeDict.empty

                            ElmSyntax.ExposeVariable _ ->
                                RangeDict.singleton exposedMemberNode.range
                                    VariableDeclaration

                            ElmSyntax.ExposeTypeName _ ->
                                RangeDict.singleton exposedMemberNode.range
                                    Type

                            ElmSyntax.ExposeChoiceType exposedType ->
                                RangeDict.singleton
                                    { start = exposedMemberNode.range.start
                                    , end = exposedType.openRange.start
                                    }
                                    Type
                                    |> RangeDict.insert exposedType.openRange
                                        Variant
                    )
                |> RangeDict.insert exposingKeywordRange KeySymbol


listFindMap : (a -> Maybe found) -> List a -> Maybe found
listFindMap elementToMaybeFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case head |> elementToMaybeFound of
                Just found ->
                    Just found

                Nothing ->
                    listFindMap elementToMaybeFound tail


listLast : List a -> Maybe a
listLast list =
    case list of
        [] ->
            Nothing

        [ onlyElement ] ->
            Just onlyElement

        _ :: el1 :: el2Up ->
            listLast (el1 :: el2Up)
