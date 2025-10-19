module ElmSyntaxHighlight exposing (SyntaxKind(..), for)

import Dict
import ElmParserLenient
import ElmSyntax
import RangeDict exposing (RangeDict)
import TextGrid


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


locationAddColumn : Int -> TextGrid.Location -> TextGrid.Location
locationAddColumn columnPlus location =
    { line = location.line
    , column = location.column + columnPlus
    }


qualifiedSyntaxKindMap :
    SyntaxKind
    ->
        ElmSyntax.Node
            { qualification : String
            , name : String
            }
    -> RangeDict SyntaxKind
qualifiedSyntaxKindMap kind qualified =
    case qualified.value.qualification of
        "" ->
            RangeDict.singleton qualified.range kind

        _ ->
            RangeDict.singleton
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
                |> Dict.union
                    (parts.part1 |> patternSyntaxKindMap)

        ElmSyntax.PatternTriple parts ->
            parts.part0
                |> patternSyntaxKindMap
                |> Dict.union
                    (parts.part1 |> patternSyntaxKindMap)
                |> Dict.union
                    (parts.part2 |> patternSyntaxKindMap)

        ElmSyntax.PatternRecord fieldVariables ->
            fieldVariables
                |> RangeDict.mapFromList
                    (\variable ->
                        ( variable.range
                        , -- both property and binding. Field punning sucks
                          Variable
                        )
                    )

        ElmSyntax.PatternListCons listCons ->
            listCons.head
                |> patternSyntaxKindMap
                |> Dict.union (listCons.tail |> patternSyntaxKindMap)
                |> RangeDict.insert listCons.consKeySymbolRange
                    Variant

        ElmSyntax.PatternListExact elements ->
            elements |> RangeDict.unionFromListMap patternSyntaxKindMap

        ElmSyntax.PatternVariant patternVariant ->
            Dict.union
                (patternVariant.reference
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
                |> Dict.union
                    ((call.argument0 :: call.argument1Up)
                        |> RangeDict.unionFromListMap
                            (\part -> part |> expressionSyntaxKindMap)
                    )

        ElmSyntax.ExpressionInfixOperation infixOperation ->
            let
                leftRightSyntaxKindMap : RangeDict SyntaxKind
                leftRightSyntaxKindMap =
                    Dict.union
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
                |> Dict.union (ifThenElse.onTrue |> expressionSyntaxKindMap)
                |> Dict.union (ifThenElse.onFalse |> expressionSyntaxKindMap)
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
                |> Dict.union (parts.part1 |> expressionSyntaxKindMap)

        ElmSyntax.ExpressionTriple parts ->
            parts.part0
                |> expressionSyntaxKindMap
                |> Dict.union (parts.part1 |> expressionSyntaxKindMap)
                |> Dict.union (parts.part2 |> expressionSyntaxKindMap)

        ElmSyntax.ExpressionParenthesized inner ->
            expressionSyntaxKindMap inner

        ElmSyntax.ExpressionLetIn letIn ->
            Dict.union
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
            Dict.union
                (caseOf.matched |> expressionSyntaxKindMap)
                ((caseOf.case0 :: caseOf.case1Up)
                    |> RangeDict.unionFromListMap
                        (\syntaxCase ->
                            Dict.union
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
            Dict.union
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
            Dict.union
                (letDestructuring.pattern |> patternSyntaxKindMap)
                (letDestructuring.expression |> expressionSyntaxKindMap)
                |> RangeDict.insert letDestructuring.equalsKeySymbolRange
                    KeySymbol

        ElmSyntax.LetValueOrFunctionDeclaration fnDeclaration ->
            let
                implementationSyntaxKindMap : RangeDict SyntaxKind
                implementationSyntaxKindMap =
                    fnDeclaration.parameters
                        |> RangeDict.unionFromListMap patternSyntaxKindMap
                        |> RangeDict.insert fnDeclaration.equalsKeySymbolRange
                            KeySymbol
                        |> Dict.union
                            (fnDeclaration.result |> expressionSyntaxKindMap)
                        |> RangeDict.insert fnDeclaration.implementationNameRange
                            Variable
            in
            case fnDeclaration.signature of
                Just functionSignature ->
                    functionSignature
                        |> signatureSyntaxKindMap
                        |> Dict.union implementationSyntaxKindMap

                Nothing ->
                    implementationSyntaxKindMap


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
                |> Dict.union
                    (parts.part1 |> typeAnnotationSyntaxKindMap)

        ElmSyntax.TypeTriple parts ->
            parts.part0
                |> typeAnnotationSyntaxKindMap
                |> Dict.union
                    (parts.part1 |> typeAnnotationSyntaxKindMap)
                |> Dict.union
                    (parts.part2 |> typeAnnotationSyntaxKindMap)

        ElmSyntax.TypeConstruct typeConstruct ->
            typeConstruct.reference
                |> qualifiedSyntaxKindMap Type
                |> Dict.union
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
            typeFunction.input
                |> typeAnnotationSyntaxKindMap
                |> RangeDict.insert
                    typeFunction.arrowKeySymbolRange
                    KeySymbol
                |> Dict.union
                    (typeFunction.output |> typeAnnotationSyntaxKindMap)


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
            let
                implementationSyntaxKindMap : RangeDict SyntaxKind
                implementationSyntaxKindMap =
                    fnDeclaration.parameters
                        |> RangeDict.unionFromListMap patternSyntaxKindMap
                        |> RangeDict.insert fnDeclaration.equalsKeySymbolRange
                            KeySymbol
                        |> Dict.union
                            (fnDeclaration.result |> expressionSyntaxKindMap)
                        |> RangeDict.insert fnDeclaration.implementationNameRange
                            VariableDeclaration
            in
            case fnDeclaration.signature of
                Just functionSignature ->
                    functionSignature
                        |> signatureSyntaxKindMap
                        |> Dict.union implementationSyntaxKindMap

                Nothing ->
                    implementationSyntaxKindMap

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
                |> Dict.union
                    (typeAliasDeclaration.parameters
                        |> RangeDict.mapFromList
                            (\variableNode ->
                                ( variableNode.range, TypeVariable )
                            )
                    )
                |> RangeDict.insert typeAliasDeclaration.equalsKeySymbolRange
                    KeySymbol
                |> Dict.union
                    (typeAliasDeclaration.type_ |> typeAnnotationSyntaxKindMap)

        ElmSyntax.DeclarationChoiceType choiceTypeDeclaration ->
            choiceTypeDeclaration.variant0.values
                |> RangeDict.unionFromListMap typeAnnotationSyntaxKindMap
                |> RangeDict.insert choiceTypeDeclaration.variant0.name.range
                    Variant
                |> Dict.union
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
                |> Dict.union
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
                |> Dict.union
                    (signature |> signatureSyntaxKindMap)

        ElmSyntax.DeclarationOperator operatorDeclaration ->
            RangeDict.singleton
                { start = declarationNode.range.start
                , end =
                    declarationNode.range.start
                        |> locationAddColumn 5
                }
                KeySymbol
                |> RangeDict.insert
                    operatorDeclaration.direction.range
                    KeySymbol
                |> RangeDict.insert
                    { start =
                        operatorDeclaration.operator.range.start
                            |> locationAddColumn 1
                    , end =
                        operatorDeclaration.operator.range.end
                            |> locationAddColumn -1
                    }
                    KeySymbol
                |> RangeDict.insert
                    operatorDeclaration.precedence.range
                    Number
                |> RangeDict.insert
                    operatorDeclaration.function.range
                    Variable


{-| Assumes `elm-format`ed code
-}
for :
    ElmSyntax.Module
    ->
        List
            { range : TextGrid.Range
            , syntaxKind : SyntaxKind
            }
for elmModule =
    elmModule.header
        |> moduleHeaderSyntaxKindMap
        |> Dict.union
            (elmModule.comments
                |> RangeDict.unionFromListMap commentSyntaxKindMap
            )
        |> Dict.union
            (elmModule.imports
                |> RangeDict.unionFromListMap importSyntaxKindMap
            )
        |> Dict.union
            (elmModule.declarations
                |> RangeDict.unionFromListMap
                    (\declarationWithDocumentation ->
                        let
                            undocumentedSyntaxKindMap : RangeDict SyntaxKind
                            undocumentedSyntaxKindMap =
                                declarationWithDocumentation.declaration |> declarationSyntaxKindMap
                        in
                        case declarationWithDocumentation.documentation of
                            Nothing ->
                                undocumentedSyntaxKindMap

                            Just documentationNode ->
                                commentSyntaxKindMap documentationNode
                                    |> Dict.union undocumentedSyntaxKindMap
                    )
            )
        |> RangeDict.toListMap
            (\range syntaxKind ->
                { range = range, syntaxKind = syntaxKind }
            )


moduleHeaderSyntaxKindMap : ElmSyntax.Node ElmSyntax.ModuleHeader -> RangeDict SyntaxKind
moduleHeaderSyntaxKindMap moduleHeaderNode =
    moduleHeaderNode.value.exposing_
        |> exposingSyntaxKindMap
        |> RangeDict.insert moduleHeaderNode.value.moduleName.range
            ModuleNameOrAlias
        |> Dict.union
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
            Dict.empty


importSyntaxKindMap : ElmSyntax.Node ElmSyntax.Import -> RangeDict SyntaxKind
importSyntaxKindMap importNode =
    Dict.union
        ((case importNode.value.exposing_ of
            Nothing ->
                Dict.empty

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
                Dict.empty

            Just importAlias ->
                RangeDict.singleton importAlias.name.range ModuleNameOrAlias
                    |> RangeDict.insert importAlias.asKeywordRange
                        KeySymbol
        )


exposingSyntaxKindMap : ElmSyntax.Node ElmSyntax.Exposing -> RangeDict SyntaxKind
exposingSyntaxKindMap exposingNode =
    let
        exposingKeywordRange : TextGrid.Range
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
                                RangeDict.singleton
                                    { start =
                                        exposedMemberNode.range.start
                                            |> locationAddColumn 1
                                    , end =
                                        exposedMemberNode.range.end
                                            |> locationAddColumn -1
                                    }
                                    KeySymbol

                            ElmSyntax.ExposeVariable _ ->
                                RangeDict.singleton exposedMemberNode.range
                                    VariableDeclaration

                            ElmSyntax.ExposeTypeName _ ->
                                RangeDict.singleton exposedMemberNode.range
                                    Type

                            ElmSyntax.ExposeChoiceTypeIncludingVariants exposedType ->
                                RangeDict.singleton
                                    { start = exposedMemberNode.range.start
                                    , end = exposedType.openRange.start
                                    }
                                    Type
                                    |> RangeDict.insert exposedType.openRange
                                        Variant
                    )
                |> RangeDict.insert exposingKeywordRange KeySymbol
