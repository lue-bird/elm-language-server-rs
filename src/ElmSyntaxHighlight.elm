module ElmSyntaxHighlight exposing (SyntaxKind(..), for)

import ElmParserLenient
import ElmSyntax
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
    { line = location.line
    , column = location.column + columnPlus
    }


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
    -- IGNORE TCO
    case patternNode.value of
        ElmSyntax.PatternIgnored ->
            RangeDict.singleton patternNode.range KeySymbol

        ElmSyntax.PatternVariable _ ->
            RangeDict.singleton patternNode.range Variable

        ElmSyntax.PatternUnit ->
            RangeDict.singleton patternNode.range Variant

        ElmSyntax.PatternChar _ ->
            RangeDict.singleton
                { start =
                    patternNode.range.start
                        |> locationAddColumn 1
                , end =
                    patternNode.range.end
                        |> locationAddColumn -1
                }
                String

        ElmSyntax.PatternString patternString ->
            RangeDict.singleton
                (case patternString.quotingStyle of
                    ElmSyntax.StringSingleQuoted ->
                        { start =
                            patternNode.range.start
                                |> locationAddColumn 1
                        , end =
                            patternNode.range.end
                                |> locationAddColumn -1
                        }

                    ElmSyntax.StringTripleQuoted ->
                        { start =
                            patternNode.range.start
                                |> locationAddColumn 3
                        , end =
                            patternNode.range.end
                                |> locationAddColumn -3
                        }
                )
                String

        ElmSyntax.PatternInt _ ->
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
                |> RangeDict.insert listCons.consKeySymbolRange
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
                ({ range =
                    { start = patternNode.range.start
                    , end =
                        patternNode.range.start
                            |> locationAddColumn (qualified |> qualifiedRangeLength)
                    }
                 , value = qualified
                 }
                    |> qualifiedSyntaxKindMap Variant
                )
                (patternVariant.values
                    |> RangeDict.unionFromListMap patternSyntaxKindMap
                )

        ElmSyntax.PatternAs patternAs ->
            patternAs.pattern
                |> patternSyntaxKindMap
                |> RangeDict.insert patternAs.asKeywordRange
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
    ElmSyntax.Node ElmSyntax.Expression
    -> RangeDict SyntaxKind
expressionSyntaxKindMap expressionNode =
    -- IGNORE TCO
    case expressionNode.value of
        ElmSyntax.ExpressionUnit ->
            RangeDict.singleton expressionNode.range Variant

        ElmSyntax.ExpressionCall call ->
            (call.called |> expressionSyntaxKindMap)
                |> RangeDict.union
                    ((call.argument0 :: call.argument1Up)
                        |> RangeDict.unionFromListMap
                            (\part -> part |> expressionSyntaxKindMap)
                    )

        ElmSyntax.ExpressionInfixOperation infixOperation ->
            let
                leftRightSyntaxKindMap : RangeDict SyntaxKind
                leftRightSyntaxKindMap =
                    RangeDict.union
                        (infixOperation.left |> expressionSyntaxKindMap)
                        (infixOperation.right |> expressionSyntaxKindMap)
            in
            case infixOperation.operator.value of
                "|>" ->
                    leftRightSyntaxKindMap
                        |> RangeDict.insert infixOperation.operator.range Flow

                "<|" ->
                    leftRightSyntaxKindMap
                        |> RangeDict.insert infixOperation.operator.range Flow

                _ ->
                    leftRightSyntaxKindMap
                        |> -- hmm
                           RangeDict.insert infixOperation.operator.range KeySymbol

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
                |> expressionSyntaxKindMap
                |> RangeDict.union (ifThenElse.onTrue |> expressionSyntaxKindMap)
                |> RangeDict.union (ifThenElse.onFalse |> expressionSyntaxKindMap)
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end =
                        expressionNode.range.start
                            |> locationAddColumn 2
                    }
                    Flow
                |> RangeDict.insert
                    ifThenElse.thenKeywordRange
                    Flow
                |> RangeDict.insert
                    ifThenElse.elseKeywordRange
                    Flow

        ElmSyntax.ExpressionOperatorFunction _ ->
            RangeDict.singleton expressionNode.range Variable

        ElmSyntax.ExpressionInteger _ ->
            RangeDict.singleton expressionNode.range Number

        ElmSyntax.ExpressionFloat _ ->
            RangeDict.singleton expressionNode.range Number

        ElmSyntax.ExpressionNegation inner ->
            inner
                |> expressionSyntaxKindMap
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = expressionNode.range.start |> locationAddColumn 1
                    }
                    -- hmm
                    KeySymbol

        ElmSyntax.ExpressionString expressionString ->
            RangeDict.singleton
                (case expressionString.quotingStyle of
                    ElmSyntax.StringSingleQuoted ->
                        { start =
                            expressionNode.range.start
                                |> locationAddColumn 1
                        , end =
                            expressionNode.range.end
                                |> locationAddColumn -1
                        }

                    ElmSyntax.StringTripleQuoted ->
                        { start =
                            expressionNode.range.start
                                |> locationAddColumn 3
                        , end =
                            expressionNode.range.end
                                |> locationAddColumn -3
                        }
                )
                String

        ElmSyntax.ExpressionChar _ ->
            RangeDict.singleton
                { start = expressionNode.range.start |> locationAddColumn 1
                , end = expressionNode.range.end |> locationAddColumn -1
                }
                String

        ElmSyntax.ExpressionTuple parts ->
            parts.part0
                |> expressionSyntaxKindMap
                |> RangeDict.union (parts.part1 |> expressionSyntaxKindMap)

        ElmSyntax.ExpressionTriple parts ->
            parts.part0
                |> expressionSyntaxKindMap
                |> RangeDict.union (parts.part1 |> expressionSyntaxKindMap)
                |> RangeDict.union (parts.part2 |> expressionSyntaxKindMap)

        ElmSyntax.ExpressionParenthesized inner ->
            expressionSyntaxKindMap inner

        ElmSyntax.ExpressionLetIn letIn ->
            RangeDict.union
                ((letIn.declaration0 :: letIn.declaration1Up)
                    |> RangeDict.unionFromListMap letDeclarationSyntaxKindMap
                )
                (letIn.result |> expressionSyntaxKindMap)
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = expressionNode.range.start |> locationAddColumn 3
                    }
                    KeySymbol
                |> RangeDict.insert letIn.inKeywordRange
                    KeySymbol

        ElmSyntax.ExpressionCaseOf caseOf ->
            RangeDict.union
                (caseOf.matched |> expressionSyntaxKindMap)
                ((caseOf.case0 :: caseOf.case1Up)
                    |> RangeDict.unionFromListMap
                        (\syntaxCase ->
                            RangeDict.union
                                (syntaxCase.pattern |> patternSyntaxKindMap)
                                (syntaxCase.result |> expressionSyntaxKindMap)
                                |> RangeDict.insert
                                    syntaxCase.arrowKeySymbolRange
                                    Flow
                        )
                )
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end = expressionNode.range.start |> locationAddColumn 4
                    }
                    Flow
                |> RangeDict.insert caseOf.ofKeywordRange
                    Flow

        ElmSyntax.ExpressionLambda lambda ->
            RangeDict.union
                ((lambda.parameter0 :: lambda.parameter1Up)
                    |> RangeDict.unionFromListMap patternSyntaxKindMap
                )
                (lambda.result |> expressionSyntaxKindMap)
                |> RangeDict.insert
                    { start = expressionNode.range.start
                    , end =
                        expressionNode.range.start |> locationAddColumn 1
                    }
                    Flow
                |> RangeDict.insert
                    lambda.arrowKeySymbolRange
                    Flow

        ElmSyntax.ExpressionRecord fields ->
            fields
                |> RangeDict.unionFromListMap
                    (\field ->
                        field.value
                            |> expressionSyntaxKindMap
                            |> RangeDict.insert field.name.range Field
                            |> RangeDict.insert field.equalsKeySymbolRange
                                KeySymbol
                    )

        ElmSyntax.ExpressionList elements ->
            elements
                |> RangeDict.unionFromListMap
                    (\element ->
                        element |> expressionSyntaxKindMap
                    )

        ElmSyntax.ExpressionRecordAccess recordAccess ->
            recordAccess.record
                |> expressionSyntaxKindMap
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
            (recordUpdate.field0 :: recordUpdate.field1Up)
                |> RangeDict.unionFromListMap
                    (\field ->
                        field.value
                            |> expressionSyntaxKindMap
                            |> RangeDict.insert field.name.range Field
                            |> RangeDict.insert field.equalsKeySymbolRange
                                KeySymbol
                    )
                |> RangeDict.insert recordUpdate.recordVariable.range Variable
                |> RangeDict.insert recordUpdate.barKeySymbolRange KeySymbol


nameIsUppercase : String -> Bool
nameIsUppercase string =
    case string |> String.uncons of
        Just ( firstChar, _ ) ->
            firstChar |> Char.isUpper

        Nothing ->
            False


letDeclarationSyntaxKindMap :
    ElmSyntax.Node ElmSyntax.LetDeclaration
    -> RangeDict SyntaxKind
letDeclarationSyntaxKindMap letDeclarationNode =
    case letDeclarationNode.value of
        ElmSyntax.LetDestructuring letDestructuring ->
            RangeDict.union
                (letDestructuring.pattern |> patternSyntaxKindMap)
                (letDestructuring.expression |> expressionSyntaxKindMap)
                |> RangeDict.insert letDestructuring.equalsKeySymbolRange
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
                    (fnDeclaration.result |> expressionSyntaxKindMap)
                    (fnDeclaration.parameters |> RangeDict.unionFromListMap patternSyntaxKindMap)
                    |> RangeDict.insert fnDeclaration.implementationNameRange
                        Variable
                    |> RangeDict.insert fnDeclaration.equalsKeySymbolRange
                        KeySymbol
                )


typeAnnotationSyntaxKindMap : ElmSyntax.Node ElmSyntax.Type -> RangeDict SyntaxKind
typeAnnotationSyntaxKindMap typeNode =
    -- IGNORE TCO
    case typeNode.value of
        ElmSyntax.TypeUnit ->
            RangeDict.singleton typeNode.range Type

        ElmSyntax.TypeVariable _ ->
            RangeDict.singleton typeNode.range TypeVariable

        ElmSyntax.TypeParenthesized inParens ->
            typeAnnotationSyntaxKindMap inParens

        ElmSyntax.TypeTuple parts ->
            parts.part0
                |> typeAnnotationSyntaxKindMap
                |> RangeDict.union
                    (parts.part1 |> typeAnnotationSyntaxKindMap)

        ElmSyntax.TypeTriple parts ->
            parts.part0
                |> typeAnnotationSyntaxKindMap
                |> RangeDict.union
                    (parts.part1 |> typeAnnotationSyntaxKindMap)
                |> RangeDict.union
                    (parts.part2 |> typeAnnotationSyntaxKindMap)

        ElmSyntax.TypeConstruct typeConstruct ->
            (typeConstruct.reference |> qualifiedSyntaxKindMap Type)
                |> RangeDict.union
                    (typeConstruct.arguments
                        |> RangeDict.unionFromListMap typeAnnotationSyntaxKindMap
                    )

        ElmSyntax.TypeRecord fields ->
            fields
                |> RangeDict.unionFromListMap
                    (\field ->
                        field.value
                            |> typeAnnotationSyntaxKindMap
                            |> RangeDict.insert field.name.range Field
                            |> RangeDict.insert field.colonKeySymbolRange
                                KeySymbol
                    )

        ElmSyntax.TypeRecordExtension typeRecordExtension ->
            (typeRecordExtension.field0 :: typeRecordExtension.field1Up)
                |> RangeDict.unionFromListMap
                    (\field ->
                        field.value
                            |> typeAnnotationSyntaxKindMap
                            |> RangeDict.insert field.name.range Field
                            |> RangeDict.insert field.colonKeySymbolRange
                                KeySymbol
                    )
                |> RangeDict.insert typeRecordExtension.recordVariable.range
                    Variable
                |> RangeDict.insert typeRecordExtension.barKeySymbolRange
                    KeySymbol

        ElmSyntax.TypeFunction typeFunction ->
            RangeDict.union
                (typeFunction.input |> typeAnnotationSyntaxKindMap)
                (typeFunction.output |> typeAnnotationSyntaxKindMap)
                |> RangeDict.insert
                    typeFunction.arrowKeySymbolRange
                    KeySymbol


signatureSyntaxKindMap :
    { name : ElmSyntax.Node String
    , type_ : ElmSyntax.Node ElmSyntax.Type
    }
    -> RangeDict SyntaxKind
signatureSyntaxKindMap signature =
    signature.type_
        |> typeAnnotationSyntaxKindMap
        |> RangeDict.insert signature.name.range
            VariableDeclaration


declarationSyntaxKindMap :
    ElmSyntax.Node ElmSyntax.Declaration
    -> RangeDict SyntaxKind
declarationSyntaxKindMap declarationNode =
    case declarationNode.value of
        ElmSyntax.DeclarationValueOrFunction fnDeclaration ->
            (case fnDeclaration.signature of
                Just functionSignature ->
                    functionSignature |> signatureSyntaxKindMap

                Nothing ->
                    RangeDict.empty
            )
                |> RangeDict.union
                    (fnDeclaration.result |> expressionSyntaxKindMap)
                |> RangeDict.union
                    (fnDeclaration.parameters |> RangeDict.unionFromListMap patternSyntaxKindMap)
                |> RangeDict.insert fnDeclaration.implementationNameRange
                    VariableDeclaration
                |> RangeDict.insert fnDeclaration.equalsKeySymbolRange
                    KeySymbol

        ElmSyntax.DeclarationTypeAlias typeAliasDeclaration ->
            RangeDict.singleton
                { start = declarationNode.range.start
                , end = declarationNode.range.start |> locationAddColumn 4
                }
                KeySymbol
                |> RangeDict.insert typeAliasDeclaration.aliasKeywordRange
                    KeySymbol
                |> RangeDict.insert typeAliasDeclaration.name.range
                    Type
                |> RangeDict.union
                    (typeAliasDeclaration.parameters
                        |> RangeDict.mapFromList
                            (\variableNode ->
                                ( variableNode.range, TypeVariable )
                            )
                    )
                |> RangeDict.insert typeAliasDeclaration.equalsKeySymbolRange
                    KeySymbol
                |> RangeDict.union
                    (typeAliasDeclaration.type_ |> typeAnnotationSyntaxKindMap)

        ElmSyntax.DeclarationChoiceType choiceTypeDeclaration ->
            choiceTypeDeclaration.variant0.values
                |> RangeDict.unionFromListMap typeAnnotationSyntaxKindMap
                |> RangeDict.insert choiceTypeDeclaration.variant0.name.range
                    Variant
                |> RangeDict.union
                    (choiceTypeDeclaration.variant1Up
                        |> RangeDict.unionFromListMap
                            (\variant ->
                                variant.values
                                    |> RangeDict.unionFromListMap typeAnnotationSyntaxKindMap
                                    |> RangeDict.insert variant.name.range Variant
                                    |> RangeDict.insert variant.orKeySymbolRange
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
                |> RangeDict.insert choiceTypeDeclaration.equalsKeySymbolRange
                    KeySymbol
                |> RangeDict.insert
                    choiceTypeDeclaration.name.range
                    Type
                |> RangeDict.insert
                    { start = declarationNode.range.start
                    , end =
                        declarationNode.range.start
                            |> locationAddColumn 4
                    }
                    KeySymbol

        ElmSyntax.DeclarationPort signature ->
            RangeDict.singleton
                { start = declarationNode.range.start
                , end =
                    declarationNode.range.start
                        |> locationAddColumn 4
                }
                KeySymbol
                |> RangeDict.union
                    ({ name = signature.name
                     , type_ = signature.type_
                     }
                        |> signatureSyntaxKindMap
                    )

        ElmSyntax.DeclarationOperator _ ->
            RangeDict.singleton
                { start = declarationNode.range.start
                , end =
                    declarationNode.range.start
                        |> locationAddColumn 4
                }
                KeySymbol


{-| Assumes `elm-format`ed code
-}
for :
    ElmSyntax.Module
    ->
        List
            { range : ElmSyntax.Range
            , syntaxKind : SyntaxKind
            }
for elmModule =
    let
        segmentsReverse : List { range : ElmSyntax.Range, syntaxKind : SyntaxKind }
        segmentsReverse =
            RangeDict.unionFromListMap identity
                [ elmModule.header |> moduleHeaderSyntaxKindMap
                , elmModule.comments
                    |> RangeDict.unionFromListMap commentSyntaxKindMap
                , elmModule.imports
                    |> RangeDict.unionFromListMap importSyntaxKindMap
                , elmModule.declarations
                    |> RangeDict.unionFromListMap
                        (\declarationWithDocumentation ->
                            (case declarationWithDocumentation.documentation of
                                Nothing ->
                                    RangeDict.empty

                                Just documentationNode ->
                                    commentSyntaxKindMap documentationNode
                            )
                                |> RangeDict.union
                                    (declarationWithDocumentation.declaration |> declarationSyntaxKindMap)
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
                            moduleHeaderNode.range.start
                                |> locationAddColumn 6
                        }
                        KeySymbol

                Just (ElmSyntax.ModuleHeaderSpecificPort portModuleHeaderSpecific) ->
                    RangeDict.singleton
                        { start = moduleHeaderNode.range.start
                        , end =
                            moduleHeaderNode.range.start
                                |> locationAddColumn 4
                        }
                        KeySymbol
                        |> RangeDict.insert portModuleHeaderSpecific.moduleKeywordRange
                            KeySymbol

                Just (ElmSyntax.ModuleHeaderSpecificEffect effectModuleHeaderSpecific) ->
                    RangeDict.singleton
                        { start = moduleHeaderNode.range.start
                        , end =
                            moduleHeaderNode.range.start
                                |> locationAddColumn 6
                        }
                        KeySymbol
                        |> RangeDict.insert effectModuleHeaderSpecific.moduleKeywordRange
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
                    importNode.range.start
                        |> locationAddColumn 6
                }
                KeySymbol
            |> RangeDict.insert importNode.value.moduleName.range ModuleNameOrAlias
        )
        (case importNode.value.alias of
            Nothing ->
                RangeDict.empty

            Just importAlias ->
                RangeDict.singleton importAlias.name.range ModuleNameOrAlias
                    |> RangeDict.insert importAlias.asKeywordRange
                        KeySymbol
        )


exposingSyntaxKindMap : ElmSyntax.Node ElmSyntax.Exposing -> RangeDict SyntaxKind
exposingSyntaxKindMap exposingNode =
    let
        exposingKeywordRange : ElmSyntax.Range
        exposingKeywordRange =
            { start = exposingNode.range.start
            , end =
                exposingNode.range.start |> locationAddColumn 8
            }
    in
    case exposingNode.value of
        ElmSyntax.ExposingAll ellipsisKeySymbolRange ->
            RangeDict.singleton exposingKeywordRange KeySymbol
                |> RangeDict.insert ellipsisKeySymbolRange KeySymbol

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
