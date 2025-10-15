module ElmParserLenient exposing
    ( Parser, run, module_
    , Comments, commentsToList
    , expose, exposing_
    , moduleHeader, import_, declarations, declaration
    , type_, pattern, expression
    , multiLineComment, singleLineComment, whitespaceAndComments
    , moduleName, nameLowercase, nameUppercase
    , RopeFilled(..)
    )

{-| Like [`Elm.Parser`](https://elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Parser)
but able to parse badly indented code and similar somewhat incorrect syntax,
similar to elm-format.

Some additional lenient parsing:

  - `a != b`   → `a /= b`  

  - `\a => b` → `\a -> b`

  - merges consecutive `,` in record, list or explicit exposing

  - removes extra `,` before first record field, list element or expose

  - merges consecutive `|` in choice type declaration

  - removes remove extra `|` before first variant declaration

  - merges consecutive `->` in function type

  - `(...)` → `(..)` in exposing and type expose that includes its variants

  - removes empty `exposing ()` after import

  - expression record field name-value separators

    `{ name : value }` or `{ name value }`

    → `{ name = value }`

  - type record field name-value separators

    `{ name = value }` or `{ name value }`

    → `{ name : value }`

  - corrects names that collide with keywords

    `Html.Attributes.type` → `Html.Attributes.type_`

@docs Parser, run, module_

That's all you'll need most of the time.

Sometimes it's useful to parse only some part of the syntax,
to, say, display only an expression in an article
or reparse only the touched declarations on save.

@docs Comments, commentsToList
@docs expose, exposing_
@docs moduleHeader, import_, declarations, declaration
@docs type_, pattern, expression


### whitespace

@docs multiLineComment, singleLineComment, whitespaceAndComments


### low-level

@docs moduleName, nameLowercase, nameUppercase
@docs RopeFilled

-}

import ElmSyntax
import ParserFast
import TextGrid
import Unicode


{-| Can turn a String into syntax or Nothing.
See [`ElmParserLenient.run`](#run)

(This is not related to [`elm/parser`](https://elm-lang.org/packages/elm/parser/latest/).
[Open an issue](https://github.com/lue-bird/elm-format-unofficial/issues/new)
if you need a way to covert to that)

-}
type alias Parser a =
    ParserFast.Parser a


{-| Turn a given source String into `Just` the parsed syntax
or `Nothing` if any unrecognizable part is found.
-}
run : Parser a -> String -> Maybe a
run syntaxParser source =
    ParserFast.run syntaxParser source


{-| [`Parser`](#Parser) for an [`ElmSyntax.Module`](ElmSyntax#Module)
-}
module_ : Parser ElmSyntax.Module
module_ =
    ParserFast.map4
        (\moduleHeaderResult moduleComments importsResult declarationsResult ->
            { header =
                moduleHeaderResult.syntax
            , imports = importsResult.syntax
            , declarations =
                declarationsResult.syntax
                    |> List.map
                        (\declarationSyntax ->
                            { documentation = declarationSyntax.documentation
                            , declaration = declarationSyntax.declaration
                            }
                        )
            , comments =
                moduleHeaderResult.comments
                    |> ropePrependTo moduleComments
                    |> ropePrependTo importsResult.comments
                    |> ropePrependTo declarationsResult.comments
                    |> commentsToList
            }
        )
        (whitespaceAndCommentsEndsTopIndentedFollowedByWithComments
            moduleHeader
        )
        (whitespaceAndCommentsEndsTopIndentedFollowedByComments
            (ParserFast.map2OrSucceed
                (\moduleDocumentation commentsAfter ->
                    ropeOne moduleDocumentation |> ropeFilledPrependTo commentsAfter
                )
                documentationComment
                whitespaceAndCommentsEndsTopIndented
                ropeEmpty
            )
        )
        (manyWithComments importFollowedByWhitespaceAndComments)
        (manyWithComments
            (topIndentedFollowedBy
                (ParserFast.map2
                    (\declarationParsed commentsAfter ->
                        { comments =
                            declarationParsed.comments
                                |> ropePrependTo commentsAfter
                        , syntax =
                            { documentation = declarationParsed.documentation
                            , declaration = declarationParsed.declaration
                            }
                        }
                    )
                    declaration
                    whitespaceAndComments
                )
            )
        )


{-| [`Parser`](#Parser) for an [`ElmSyntax.ModuleName`](ElmSyntax#ModuleName)
-}
moduleName : Parser (ElmSyntax.Node ElmSyntax.ModuleName)
moduleName =
    ParserFast.map2WithRange
        (\range head tail ->
            { range = range, value = head :: tail }
        )
        nameUppercase
        (ParserFast.loopWhileSucceedsRightToLeftStackUnsafe
            (ParserFast.symbolFollowedBy "." nameUppercase)
            []
            (::)
        )


moduleHeaderExposing : Parser (WithComments (ElmSyntax.Node ElmSyntax.Exposing))
moduleHeaderExposing =
    ParserFast.map2WithRange
        (\range commentsAfterExposing exposingInnerResult ->
            { comments =
                commentsAfterExposing
                    |> ropePrependTo exposingInnerResult.comments
            , syntax =
                { range = range, value = exposingInnerResult.syntax }
            }
        )
        (ParserFast.symbolFollowedBy "exposing" whitespaceAndComments)
        exposing_


{-| [`Parser`](#Parser) for an [`ElmSyntax.Exposing`](ElmSyntax#Exposing)
(the stuff after `exposing` in an import or module)
-}
exposing_ : Parser { comments : Comments, syntax : ElmSyntax.Exposing }
exposing_ =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2
            (\commentsBefore inner ->
                { comments = commentsBefore |> ropePrependTo inner.comments
                , syntax = inner.syntax
                }
            )
            whitespaceAndComments
            (ParserFast.oneOf3
                (ParserFast.mapWithRange
                    (\range comments ->
                        { comments = comments
                        , syntax = ElmSyntax.ExposingAll range
                        }
                    )
                    (ParserFast.symbolFollowedBy "..." whitespaceAndComments)
                )
                (ParserFast.mapWithRange
                    (\range comments ->
                        { comments = comments
                        , syntax = ElmSyntax.ExposingAll range
                        }
                    )
                    (ParserFast.symbolFollowedBy ".." whitespaceAndComments)
                )
                (exposingWithinParensExplicitFollowedByWhitespaceAndCommentsMap identity)
            )
        )
        |> ParserFast.followedBySymbol ")"


exposingWithinParensExplicitFollowedByWhitespaceAndCommentsMap : (ElmSyntax.Exposing -> syntax) -> ParserFast.Parser (WithComments syntax)
exposingWithinParensExplicitFollowedByWhitespaceAndCommentsMap exposingToSyntax =
    ParserFast.map4
        (\commentsBeforeHeadElement headElement commentsAfterHeadElement tailElements ->
            { comments =
                commentsBeforeHeadElement
                    |> ropePrependTo headElement.comments
                    |> ropePrependTo commentsAfterHeadElement
                    |> ropePrependTo tailElements.comments
            , syntax =
                ElmSyntax.ExposingExplicit
                    (headElement.syntax :: tailElements.syntax)
                    |> exposingToSyntax
            }
        )
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "," whitespaceAndComments)
            ropeEmpty
        )
        expose
        whitespaceAndComments
        (manyWithComments
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map4
                    (\commentsBefore commentsWithExtraComma result commentsAfter ->
                        { comments =
                            commentsBefore
                                |> ropePrependTo commentsWithExtraComma
                                |> ropePrependTo result.comments
                                |> ropePrependTo commentsAfter
                        , syntax = result.syntax
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                        ropeEmpty
                    )
                    expose
                    whitespaceAndComments
                )
            )
        )


{-| [`Parser`](#Parser) for a single [`ElmSyntax.Expose`](ElmSyntax#Expose)
-}
expose : Parser { comments : Comments, syntax : ElmSyntax.Node ElmSyntax.Expose }
expose =
    ParserFast.oneOf3
        functionExpose
        typeExpose
        infixExpose


infixExpose : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expose))
infixExpose =
    ParserFast.map2WithRange
        (\range infixName () ->
            { comments = ropeEmpty
            , syntax = { range = range, value = ElmSyntax.ExposeOperator infixName }
            }
        )
        (ParserFast.symbolFollowedBy "("
            (ParserFast.ifFollowedByWhileWithoutLinebreak
                (\c ->
                    case c of
                        ')' ->
                            False

                        '\n' ->
                            False

                        ' ' ->
                            False

                        _ ->
                            True
                )
                (\c ->
                    case c of
                        ')' ->
                            False

                        '\n' ->
                            False

                        ' ' ->
                            False

                        _ ->
                            True
                )
            )
        )
        (ParserFast.symbol ")" ())


typeExpose : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expose))
typeExpose =
    ParserFast.map3
        (\typeExposeNameNode commentsBeforeMaybeOpen maybeOpen ->
            case maybeOpen of
                Nothing ->
                    { comments = commentsBeforeMaybeOpen
                    , syntax =
                        { range = typeExposeNameNode.range, value = ElmSyntax.ExposeTypeName typeExposeNameNode.value }
                    }

                Just open ->
                    { comments = commentsBeforeMaybeOpen |> ropePrependTo open.comments
                    , syntax =
                        { range = { start = typeExposeNameNode.range.start, end = open.syntax.end }
                        , value = ElmSyntax.ExposeChoiceType { name = typeExposeNameNode.value, openRange = open.syntax }
                        }
                    }
        )
        nameUppercaseNode
        whitespaceAndComments
        (ParserFast.map2WithRangeOrSucceed
            (\range left right ->
                Just { comments = left |> ropePrependTo right, syntax = range }
            )
            (ParserFast.symbolFollowedBy "(" whitespaceAndComments)
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "..." whitespaceAndComments)
                (ParserFast.symbolFollowedBy ".." whitespaceAndComments)
                |> ParserFast.followedBySymbol ")"
            )
            Nothing
        )


functionExpose : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expose))
functionExpose =
    nameLowercaseMapWithRange
        (\range name ->
            { comments = ropeEmpty
            , syntax =
                { range = range, value = ElmSyntax.ExposeVariable name }
            }
        )


{-| [`Parser`](#Parser) for an [`ElmSyntax.Module`](ElmSyntax#Module)
(confusingly, that's their name for only the `module X exposing (Y)` lines)
-}
moduleHeader : Parser { comments : Comments, syntax : ElmSyntax.Node ElmSyntax.ModuleHeader }
moduleHeader =
    ParserFast.oneOf3
        normalModuleHeader
        portModuleHeader
        effectModuleHeader


effectWhereClause : Parser (WithComments ( String, ElmSyntax.Node String ))
effectWhereClause =
    ParserFast.map4
        (\fnName commentsAfterFnName commentsAfterEqual fnTypeName ->
            { comments = commentsAfterFnName |> ropePrependTo commentsAfterEqual
            , syntax = ( fnName, fnTypeName )
            }
        )
        nameLowercaseUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
        nameUppercaseNode


whereBlock : Parser (WithComments { command : Maybe (ElmSyntax.Node String), subscription : Maybe (ElmSyntax.Node String) })
whereBlock =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map4
            (\commentsBeforeHead head commentsAfterHead tail ->
                let
                    pairs : List ( String, ElmSyntax.Node String )
                    pairs =
                        head.syntax :: tail.syntax
                in
                { comments =
                    commentsBeforeHead
                        |> ropePrependTo head.comments
                        |> ropePrependTo commentsAfterHead
                        |> ropePrependTo tail.comments
                , syntax =
                    { command =
                        pairs
                            |> listFirstWhere
                                (\( fnName, _ ) ->
                                    case fnName of
                                        "command" ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Maybe.map Tuple.second
                    , subscription =
                        pairs
                            |> listFirstWhere
                                (\( fnName, _ ) ->
                                    case fnName of
                                        "subscription" ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Maybe.map Tuple.second
                    }
                }
            )
            whitespaceAndComments
            effectWhereClause
            whitespaceAndComments
            (manyWithComments
                (ParserFast.symbolFollowedBy ","
                    (ParserFast.map3
                        (\commentsBefore v commentsAfter ->
                            { comments =
                                commentsBefore
                                    |> ropePrependTo v.comments
                                    |> ropePrependTo commentsAfter
                            , syntax = v.syntax
                            }
                        )
                        whitespaceAndComments
                        effectWhereClause
                        whitespaceAndComments
                    )
                )
            )
        )
        |> ParserFast.followedBySymbol "}"


listFirstWhere : (a -> Bool) -> List a -> Maybe a
listFirstWhere predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                listFirstWhere predicate xs


effectWhereClauses : Parser (WithComments { command : Maybe (ElmSyntax.Node String), subscription : Maybe (ElmSyntax.Node String) })
effectWhereClauses =
    ParserFast.map2
        (\commentsBefore whereResult ->
            { comments = commentsBefore |> ropePrependTo whereResult.comments
            , syntax = whereResult.syntax
            }
        )
        (ParserFast.keywordFollowedBy "where" whitespaceAndComments)
        whereBlock


effectModuleHeader : Parser (WithComments (ElmSyntax.Node ElmSyntax.ModuleHeader))
effectModuleHeader =
    ParserFast.map8WithStartLocation
        (\startLocation commentsAfterEffectKeyword moduleKeywordRange commentsAfterModuleKeyword name commentsAfterName whereClauses commentsAfterWhereClauses exposingResult ->
            { comments =
                commentsAfterEffectKeyword
                    |> ropePrependTo commentsAfterModuleKeyword
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo whereClauses.comments
                    |> ropePrependTo commentsAfterWhereClauses
                    |> ropePrependTo exposingResult.comments
            , syntax =
                { range =
                    { start = startLocation
                    , end = exposingResult.syntax.range.end
                    }
                , value =
                    { moduleName = name
                    , exposing_ = exposingResult.syntax
                    , specific =
                        Just
                            (ElmSyntax.ModuleHeaderSpecificEffect
                                { moduleKeywordRange = moduleKeywordRange
                                , command = whereClauses.syntax.command
                                , subscription = whereClauses.syntax.subscription
                                }
                            )
                    }
                }
            }
        )
        (ParserFast.keywordFollowedBy "effect" whitespaceAndComments)
        (ParserFast.keywordRange "module")
        whitespaceAndComments
        moduleName
        whitespaceAndComments
        effectWhereClauses
        whitespaceAndComments
        moduleHeaderExposing


normalModuleHeader : Parser (WithComments (ElmSyntax.Node ElmSyntax.ModuleHeader))
normalModuleHeader =
    ParserFast.map4WithRange
        (\range commentsAfterModule moduleNameNode commentsAfterModuleName exposingResult ->
            { comments =
                commentsAfterModule
                    |> ropePrependTo commentsAfterModuleName
                    |> ropePrependTo exposingResult.comments
            , syntax =
                { range = range
                , value =
                    { moduleName = moduleNameNode
                    , exposing_ = exposingResult.syntax
                    , specific = Nothing
                    }
                }
            }
        )
        (ParserFast.keywordFollowedBy "module" whitespaceAndComments)
        moduleName
        whitespaceAndComments
        moduleHeaderExposing


portModuleHeader : Parser (WithComments (ElmSyntax.Node ElmSyntax.ModuleHeader))
portModuleHeader =
    ParserFast.map6WithStartLocation
        (\startLocation commentsAfterPortKeyword moduleKeywordRange commentsAfterModuleKeyword moduleNameNode commentsAfterModuleName exposingResult ->
            { comments =
                commentsAfterPortKeyword
                    |> ropePrependTo commentsAfterModuleKeyword
                    |> ropePrependTo commentsAfterModuleName
                    |> ropePrependTo exposingResult.comments
            , syntax =
                { range =
                    { start = startLocation
                    , end = exposingResult.syntax.range.end
                    }
                , value =
                    { moduleName = moduleNameNode
                    , exposing_ = exposingResult.syntax
                    , specific =
                        Just
                            (ElmSyntax.ModuleHeaderSpecificPort
                                { moduleKeywordRange = moduleKeywordRange }
                            )
                    }
                }
            }
        )
        (ParserFast.keywordFollowedBy "port" whitespaceAndComments)
        (ParserFast.keywordRange "module")
        whitespaceAndComments
        moduleName
        whitespaceAndComments
        moduleHeaderExposing


{-| [`Parser`](#Parser) for a single [`ElmSyntax.Import`](ElmSyntax#Import)
-}
import_ : Parser { comments : Comments, syntax : ElmSyntax.Node ElmSyntax.Import }
import_ =
    importFollowedByWhitespaceAndComments


importFollowedByWhitespaceAndComments : Parser { comments : Comments, syntax : ElmSyntax.Node ElmSyntax.Import }
importFollowedByWhitespaceAndComments =
    ParserFast.map5WithStartLocation
        (\start commentsAfterImport moduleNameNode commentsAfterModuleName maybeAlias maybeExposingResult ->
            let
                commentsBeforeAlias : Comments
                commentsBeforeAlias =
                    commentsAfterImport
                        |> ropePrependTo commentsAfterModuleName
            in
            case maybeAlias of
                Nothing ->
                    case maybeExposingResult.syntax of
                        Nothing ->
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                { range = { start = start, end = moduleNameNode.range.end }
                                , value =
                                    { moduleName = moduleNameNode
                                    , alias = Nothing
                                    , exposing_ = Nothing
                                    }
                                }
                            }

                        Just exposingValue ->
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                { range = { start = start, end = exposingValue.range.end }
                                , value =
                                    { moduleName = moduleNameNode
                                    , alias = Nothing
                                    , exposing_ = Just exposingValue
                                    }
                                }
                            }

                Just aliasResult ->
                    case maybeExposingResult.syntax of
                        Nothing ->
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo aliasResult.comments
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                { range = { start = start, end = aliasResult.syntax.name.range.end }
                                , value =
                                    { moduleName = moduleNameNode
                                    , alias = Just aliasResult.syntax
                                    , exposing_ = Nothing
                                    }
                                }
                            }

                        Just exposingValue ->
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo aliasResult.comments
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                { range = { start = start, end = exposingValue.range.end }
                                , value =
                                    { moduleName = moduleNameNode
                                    , alias = Just aliasResult.syntax
                                    , exposing_ = Just exposingValue
                                    }
                                }
                            }
        )
        (ParserFast.keywordFollowedBy "import" whitespaceAndComments)
        moduleName
        whitespaceAndComments
        (ParserFast.map3WithStartLocationOrSucceed
            (\asKeywordStartLocation commentsBeforeAs nameNode commentsAfter ->
                Just
                    { comments = commentsBeforeAs |> ropePrependTo commentsAfter
                    , syntax =
                        { asKeywordRange =
                            { start = asKeywordStartLocation
                            , end =
                                asKeywordStartLocation
                                    |> locationAddColumn 2
                            }
                        , name = nameNode
                        }
                    }
            )
            (ParserFast.keywordFollowedBy "as" whitespaceAndComments)
            (nameUppercaseMapWithRange
                (\range moduleAlias ->
                    { range = range, value = moduleAlias }
                )
            )
            whitespaceAndComments
            Nothing
        )
        (ParserFast.map2OrSucceed
            (\exposingResult commentsAfter ->
                { comments = exposingResult.comments |> ropePrependTo commentsAfter
                , syntax = exposingResult.syntax
                }
            )
            (ParserFast.map2WithRange
                (\range commentsAfterExposing exposingInnerResult ->
                    { comments =
                        commentsAfterExposing
                            |> ropePrependTo exposingInnerResult.comments
                    , syntax =
                        case exposingInnerResult.syntax of
                            Nothing ->
                                Nothing

                            Just exposingInner ->
                                Just { range = range, value = exposingInner }
                    }
                )
                (ParserFast.symbolFollowedBy "exposing" whitespaceAndComments)
                (ParserFast.symbolFollowedBy "("
                    (ParserFast.map2
                        (\commentsBefore inner ->
                            { comments = commentsBefore |> ropePrependTo inner.comments
                            , syntax = inner.syntax
                            }
                        )
                        whitespaceAndComments
                        (ParserFast.oneOf4
                            (ParserFast.mapWithRange
                                (\range comments ->
                                    { comments = comments
                                    , syntax = Just (ElmSyntax.ExposingAll range)
                                    }
                                )
                                (ParserFast.symbolFollowedBy "..." whitespaceAndComments)
                                |> ParserFast.followedBySymbol ")"
                            )
                            (ParserFast.mapWithRange
                                (\range comments ->
                                    { comments = comments
                                    , syntax = Just (ElmSyntax.ExposingAll range)
                                    }
                                )
                                (ParserFast.symbolFollowedBy ".." whitespaceAndComments)
                                |> ParserFast.followedBySymbol ")"
                            )
                            (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                            (exposingWithinParensExplicitFollowedByWhitespaceAndCommentsMap Just
                                |> ParserFast.followedBySymbol ")"
                            )
                        )
                    )
                )
            )
            whitespaceAndComments
            { comments = ropeEmpty, syntax = Nothing }
        )


{-| [`Parser`](#Parser) for a list of [`ElmSyntax.Declaration`](ElmSyntax#Declaration)s
and comments in between
-}
declarations :
    Parser
        { comments : Comments
        , syntax :
            List
                { documentation : Maybe (ElmSyntax.Node String)
                , declaration : ElmSyntax.Node ElmSyntax.Declaration
                }
        }
declarations =
    manyWithComments
        (topIndentedFollowedBy
            (ParserFast.map2
                (\declarationParsed commentsAfter ->
                    { comments = declarationParsed.comments |> ropePrependTo commentsAfter
                    , syntax =
                        { documentation = declarationParsed.documentation
                        , declaration = declarationParsed.declaration
                        }
                    }
                )
                declaration
                whitespaceAndComments
            )
        )


{-| [`Parser`](#Parser) for an [`ElmSyntax.Declaration`](ElmSyntax#Declaration)
-}
declaration :
    Parser
        { comments : Comments
        , documentation : Maybe (ElmSyntax.Node String)
        , declaration : ElmSyntax.Node ElmSyntax.Declaration
        }
declaration =
    ParserFast.oneOf2Map
        identity
        (ParserFast.map3
            (\documentation commentsBeforeDeclaration declarationResult ->
                { comments =
                    commentsBeforeDeclaration
                        |> ropePrependTo declarationResult.comments
                , documentation = Just documentation
                , declaration = declarationResult.syntax
                }
            )
            documentationComment
            whitespaceAndCommentsEndsTopIndented
            declarationAfterDocumentation
        )
        (\declarationResult ->
            { comments = declarationResult.comments
            , documentation = Nothing
            , declaration = declarationResult.syntax
            }
        )
        declarationAfterDocumentation


declarationAfterDocumentation :
    ParserFast.Parser
        (WithComments
            (ElmSyntax.Node ElmSyntax.Declaration)
        )
declarationAfterDocumentation =
    ParserFast.oneOf4
        valueOrFunctionDeclarationWithoutDocumentation
        choiceTypeOrTypeAliasDeclarationWithoutDocumentation
        portDeclarationWithoutDocumentation
        infixDeclaration


documentationComment : Parser (ElmSyntax.Node String)
documentationComment =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    ParserFast.nestableMultiCommentMapWithRange
        (\range value ->
            { range = range, value = value }
        )
        ( '{', "-" )
        ( '-', "}" )


listLast : List a -> Maybe a
listLast list =
    case list of
        [] ->
            Nothing

        [ onlyElement ] ->
            Just onlyElement

        _ :: el1 :: el2Up ->
            listLast (el1 :: el2Up)


type TypeOrTypeAliasDeclarationWithoutDocumentation
    = ChoiceTypeDeclarationWithoutDocumentation
        { name : ElmSyntax.Node String
        , parameters : List (ElmSyntax.Node String)
        , equalsKeySymbolRange : TextGrid.Range
        , variant0 :
            { name : ElmSyntax.Node String
            , values : List (ElmSyntax.Node ElmSyntax.Type)
            }
        , variant1UpReverse :
            List
                { -- the vertical bar |
                  orKeySymbolRange : TextGrid.Range
                , name : ElmSyntax.Node String
                , values : List (ElmSyntax.Node ElmSyntax.Type)
                }
        }
    | TypeAliasDeclarationWithoutDocumentation
        { aliasKeywordRange : TextGrid.Range
        , name : ElmSyntax.Node String
        , parameters : List (ElmSyntax.Node String)
        , equalsKeySymbolRange : TextGrid.Range
        , type_ : ElmSyntax.Node ElmSyntax.Type
        }


valueOrFunctionDeclarationWithoutDocumentation : Parser (WithComments (ElmSyntax.Node ElmSyntax.Declaration))
valueOrFunctionDeclarationWithoutDocumentation =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature parametersToEquals commentsAfterEqual result ->
            case maybeSignature of
                Nothing ->
                    { comments =
                        commentsAfterStartName
                            |> ropePrependTo parametersToEquals.comments
                            |> ropePrependTo commentsAfterEqual
                            |> ropePrependTo result.comments
                    , syntax =
                        { range = { start = startNameStart, end = result.syntax.range.end }
                        , value =
                            ElmSyntax.DeclarationValueOrFunction
                                { signature = Nothing
                                , name = startNameNode.value
                                , implementationNameRange = startNameNode.range
                                , parameters = parametersToEquals.parameters
                                , equalsKeySymbolRange =
                                    parametersToEquals.equalsKeySymbolRange
                                , result = result.syntax
                                }
                        }
                    }

                Just signature ->
                    { comments =
                        (commentsAfterStartName |> ropePrependTo signature.comments)
                            |> ropePrependTo parametersToEquals.comments
                            |> ropePrependTo commentsAfterEqual
                            |> ropePrependTo result.comments
                    , syntax =
                        { range = { start = startNameStart, end = result.syntax.range.end }
                        , value =
                            ElmSyntax.DeclarationValueOrFunction
                                { signature =
                                    Just
                                        { name = startNameNode
                                        , type_ = signature.type_
                                        }
                                , name = signature.implementationName.value
                                , implementationNameRange =
                                    signature.implementationName.range
                                , parameters = parametersToEquals.parameters
                                , equalsKeySymbolRange =
                                    parametersToEquals.equalsKeySymbolRange
                                , result = result.syntax
                                }
                        }
                    }
        )
        functionNameNotInfixNode
        whitespaceAndComments
        (ParserFast.map4OrSucceed
            (\commentsBeforeType typeResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeType
                            |> ropePrependTo typeResult.comments
                            |> ropePrependTo implementationName.comments
                            |> ropePrependTo afterImplementationName
                    , implementationName = implementationName.syntax
                    , type_ = typeResult.syntax
                    }
            )
            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
            type_
            (whitespaceAndCommentsEndsTopIndentedFollowedBy
                nameLowercaseNode
            )
            whitespaceAndComments
            Nothing
        )
        parameterPatternsEquals
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments
        |> ParserFast.validate
            (\result ->
                case result.syntax.value of
                    ElmSyntax.DeclarationValueOrFunction letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just signatureNode ->
                                letFunctionDeclaration.name
                                    == signatureNode.name.value

                    ElmSyntax.DeclarationTypeAlias _ ->
                        True

                    ElmSyntax.DeclarationChoiceType _ ->
                        True

                    ElmSyntax.DeclarationPort _ ->
                        True

                    ElmSyntax.DeclarationOperator _ ->
                        True
            )


parameterPatternsEquals :
    Parser
        { equalsKeySymbolRange : TextGrid.Range
        , comments : Comments
        , parameters : List (ElmSyntax.Node ElmSyntax.Pattern)
        }
parameterPatternsEquals =
    ParserFast.mapWithEndLocation
        (\equalsKeySymbolEndLocation parametersResult ->
            { equalsKeySymbolRange =
                { start =
                    equalsKeySymbolEndLocation
                        |> locationAddColumn -1
                , end = equalsKeySymbolEndLocation
                }
            , parameters = parametersResult.syntax
            , comments = parametersResult.comments
            }
        )
        (untilWithComments
            (ParserFast.symbol "=" ())
            (ParserFast.map2
                (\patternResult commentsAfterPattern ->
                    { comments = patternResult.comments |> ropePrependTo commentsAfterPattern
                    , syntax = patternResult.syntax
                    }
                )
                patternNotSpaceSeparated
                whitespaceAndComments
            )
        )


infixDeclaration : Parser (WithComments (ElmSyntax.Node ElmSyntax.Declaration))
infixDeclaration =
    ParserFast.map9WithRange
        (\range commentsAfterInfix direction commentsAfterDirection precedence commentsAfterPrecedence operator commentsAfterOperator commentsAfterEqual fn ->
            { comments =
                commentsAfterInfix
                    |> ropePrependTo commentsAfterDirection
                    |> ropePrependTo commentsAfterPrecedence
                    |> ropePrependTo commentsAfterOperator
                    |> ropePrependTo commentsAfterEqual
            , syntax =
                { range = range
                , value =
                    ElmSyntax.DeclarationOperator
                        { direction = direction, precedence = precedence, operator = operator, function = fn }
                }
            }
        )
        (ParserFast.keywordFollowedBy "infix" whitespaceAndComments)
        infixDirection
        whitespaceAndComments
        (ParserFast.integerDecimalMapWithRange (\range value -> { range = range, value = value }))
        whitespaceAndComments
        (ParserFast.symbolFollowedBy "("
            (ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
                (\operatorRange operator ->
                    { range =
                        { start = { line = operatorRange.start.line, column = operatorRange.start.column - 1 }
                        , end = { line = operatorRange.end.line, column = operatorRange.end.column + 1 }
                        }
                    , value = operator
                    }
                )
                isOperatorSymbolCharAsString
                isAllowedOperatorToken
                ")"
            )
        )
        whitespaceAndComments
        (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
        nameLowercaseNode


infixDirection : Parser (ElmSyntax.Node ElmSyntax.InfixDirection)
infixDirection =
    ParserFast.oneOf3
        (ParserFast.mapWithRange (\range value -> { range = range, value = value }) (ParserFast.keyword "right" ElmSyntax.Right))
        (ParserFast.mapWithRange (\range value -> { range = range, value = value }) (ParserFast.keyword "left" ElmSyntax.Left))
        (ParserFast.mapWithRange (\range value -> { range = range, value = value }) (ParserFast.keyword "non" ElmSyntax.Non))


portDeclarationWithoutDocumentation : Parser (WithComments (ElmSyntax.Node ElmSyntax.Declaration))
portDeclarationWithoutDocumentation =
    ParserFast.map5
        (\commentsAfterPort nameNode commentsAfterName commentsAfterColon typeResult ->
            { comments =
                commentsAfterPort
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo commentsAfterColon
                    |> ropePrependTo typeResult.comments
            , syntax =
                { range =
                    { start = { line = nameNode.range.start.line, column = 1 }
                    , end = typeResult.syntax.range.end
                    }
                , value =
                    ElmSyntax.DeclarationPort
                        { name = nameNode
                        , type_ = typeResult.syntax
                        }
                }
            }
        )
        (ParserFast.keywordFollowedBy "port" whitespaceAndComments)
        nameLowercaseNodeUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
        type_


choiceTypeOrTypeAliasDeclarationWithoutDocumentation : Parser (WithComments (ElmSyntax.Node ElmSyntax.Declaration))
choiceTypeOrTypeAliasDeclarationWithoutDocumentation =
    ParserFast.map2WithStartLocation
        (\start commentsAfterType afterStart ->
            let
                allComments : Comments
                allComments =
                    commentsAfterType |> ropePrependTo afterStart.comments
            in
            case afterStart.syntax of
                ChoiceTypeDeclarationWithoutDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : TextGrid.Location
                        end =
                            case typeDeclarationAfterDocumentation.variant1UpReverse of
                                lastVariant :: _ ->
                                    case lastVariant.values |> listLast of
                                        Nothing ->
                                            lastVariant.name.range.end

                                        Just lastVariantLastValue ->
                                            lastVariantLastValue.range.end

                                [] ->
                                    case typeDeclarationAfterDocumentation.variant0.values |> listLast of
                                        Nothing ->
                                            typeDeclarationAfterDocumentation.variant0.name.range.end

                                        Just lastVariantLastValue ->
                                            lastVariantLastValue.range.end
                    in
                    { comments = allComments
                    , syntax =
                        { range = { start = start, end = end }
                        , value =
                            ElmSyntax.DeclarationChoiceType
                                { name = typeDeclarationAfterDocumentation.name
                                , parameters = typeDeclarationAfterDocumentation.parameters
                                , equalsKeySymbolRange =
                                    typeDeclarationAfterDocumentation.equalsKeySymbolRange
                                , variant0 =
                                    typeDeclarationAfterDocumentation.variant0
                                , variant1Up =
                                    typeDeclarationAfterDocumentation.variant1UpReverse
                                        |> List.reverse
                                }
                        }
                    }

                TypeAliasDeclarationWithoutDocumentation typeAliasDeclarationAfterDocumentation ->
                    { comments = allComments
                    , syntax =
                        { range = { start = start, end = typeAliasDeclarationAfterDocumentation.type_.range.end }
                        , value =
                            ElmSyntax.DeclarationTypeAlias
                                { aliasKeywordRange = typeAliasDeclarationAfterDocumentation.aliasKeywordRange
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , parameters = typeAliasDeclarationAfterDocumentation.parameters
                                , equalsKeySymbolRange =
                                    typeAliasDeclarationAfterDocumentation.equalsKeySymbolRange
                                , type_ = typeAliasDeclarationAfterDocumentation.type_
                                }
                        }
                    }
        )
        (ParserFast.keywordFollowedBy "type" whitespaceAndComments)
        (ParserFast.oneOf2
            typeAliasDeclarationWithoutDocumentationAfterTypePrefix
            choiceTypeDeclarationWithoutDocumentationAfterTypePrefix
        )


typeAliasDeclarationWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDeclarationWithoutDocumentationAfterTypePrefix =
    ParserFast.map6WithStartLocation
        (\aliasKeywordStartLocation commentsAfterAlias name commentsAfterName parametersToEquals commentsAfterEqual typeResult ->
            { comments =
                commentsAfterAlias
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo parametersToEquals.comments
                    |> ropePrependTo commentsAfterEqual
                    |> ropePrependTo typeResult.comments
            , syntax =
                TypeAliasDeclarationWithoutDocumentation
                    { aliasKeywordRange =
                        { start = aliasKeywordStartLocation
                        , end =
                            aliasKeywordStartLocation
                                |> locationAddColumn 5
                        }
                    , name = name
                    , parameters = parametersToEquals.parameters
                    , equalsKeySymbolRange =
                        parametersToEquals.equalsKeySymbolRange
                    , type_ = typeResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "alias" whitespaceAndComments)
        nameUppercaseNode
        whitespaceAndComments
        typeParameterListEquals
        whitespaceAndComments
        type_


choiceTypeDeclarationWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
choiceTypeDeclarationWithoutDocumentationAfterTypePrefix =
    ParserFast.map7
        (\name commentsAfterName parametersToEquals commentsAfterEqual commentsBeforeHeadVariant variant0 variant1UpReverse ->
            { comments =
                commentsAfterName
                    |> ropePrependTo parametersToEquals.comments
                    |> ropePrependTo commentsAfterEqual
                    |> ropePrependTo commentsBeforeHeadVariant
                    |> ropePrependTo variant0.comments
                    |> ropePrependTo variant1UpReverse.comments
            , syntax =
                ChoiceTypeDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parametersToEquals.parameters
                    , equalsKeySymbolRange = parametersToEquals.equalsKeySymbolRange
                    , variant0 = variant0.syntax
                    , variant1UpReverse = variant1UpReverse.syntax
                    }
            }
        )
        nameUppercaseNode
        whitespaceAndComments
        typeParameterListEquals
        whitespaceAndComments
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "|" whitespaceAndComments)
            ropeEmpty
        )
        variantDeclarationFollowedByWhitespaceAndComments
        (manyWithCommentsReverse
            (ParserFast.symbolFollowedBy "|"
                (ParserFast.map3WithStartLocation
                    (\orKeySymbolEndLocation commentsAfterOr commentsWithExtraOr variantResult ->
                        { comments =
                            commentsAfterOr
                                |> ropePrependTo commentsWithExtraOr
                                |> ropePrependTo variantResult.comments
                        , syntax =
                            { orKeySymbolRange =
                                { start =
                                    orKeySymbolEndLocation
                                        |> locationAddColumn -1
                                , end = orKeySymbolEndLocation
                                }
                            , name = variantResult.syntax.name
                            , values = variantResult.syntax.values
                            }
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "|" whitespaceAndComments)
                        ropeEmpty
                    )
                    variantDeclarationFollowedByWhitespaceAndComments
                )
            )
        )


variantDeclarationFollowedByWhitespaceAndComments :
    Parser
        (WithComments
            { name : ElmSyntax.Node String
            , values : List (ElmSyntax.Node ElmSyntax.Type)
            }
        )
variantDeclarationFollowedByWhitespaceAndComments =
    ParserFast.map3
        (\nameNode commentsAfterName valuesReverse ->
            { comments =
                commentsAfterName
                    |> ropePrependTo valuesReverse.comments
            , syntax =
                { name = nameNode
                , values =
                    valuesReverse.syntax
                        |> List.reverse
                }
            }
        )
        nameUppercaseNode
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeResult commentsAfter ->
                        { comments =
                            typeResult.comments
                                |> ropePrependTo commentsAfter
                        , syntax = typeResult.syntax
                        }
                    )
                    typeNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )


typeParameterListEquals :
    Parser
        { equalsKeySymbolRange : TextGrid.Range
        , comments : Comments
        , parameters : List (ElmSyntax.Node String)
        }
typeParameterListEquals =
    ParserFast.mapWithEndLocation
        (\equalsKeySymbolEndLocation parametersResult ->
            { equalsKeySymbolRange =
                { start =
                    equalsKeySymbolEndLocation
                        |> locationAddColumn -1
                , end = equalsKeySymbolEndLocation
                }
            , parameters = parametersResult.syntax
            , comments = parametersResult.comments
            }
        )
        (untilWithComments (ParserFast.symbol "=" ())
            (ParserFast.map2
                (\name commentsAfterName ->
                    { comments = commentsAfterName
                    , syntax = name
                    }
                )
                nameLowercaseNodeUnderscoreSuffixingKeywords
                whitespaceAndComments
            )
        )


{-| [`Parser`](#Parser) for an [`ElmSyntax.Type`](ElmSyntax#Type)
-}
type_ : Parser { comments : Comments, syntax : ElmSyntax.Node ElmSyntax.Type }
type_ =
    ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
        (ParserFast.map2
            (\startType commentsAfter ->
                { comments =
                    startType.comments
                        |> ropePrependTo commentsAfter
                , syntax = startType.syntax
                }
            )
            (ParserFast.lazy (\() -> typeNotFunction))
            whitespaceAndComments
        )
        (ParserFast.symbolFollowedBy "->"
            (ParserFast.map4WithStartLocation
                (\arrowKeySymbolEndLocation commentsAfterArrow commentsWithExtraArrow typeResult commentsAfterType ->
                    { comments =
                        commentsAfterArrow
                            |> ropePrependTo commentsWithExtraArrow
                            |> ropePrependTo typeResult.comments
                            |> ropePrependTo commentsAfterType
                    , syntax = typeResult.syntax
                    , arrowKeySymbolEndLocation = arrowKeySymbolEndLocation
                    }
                )
                whitespaceAndComments
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "->" whitespaceAndComments)
                    ropeEmpty
                )
                (ParserFast.lazy (\() -> typeNotFunction))
                whitespaceAndComments
            )
        )
        (\inType arrowToOutType ->
            { comments =
                inType.comments
                    |> ropePrependTo arrowToOutType.comments
            , syntax =
                ElmSyntax.nodeCombine
                    (\input output ->
                        ElmSyntax.TypeFunction
                            { input = input
                            , arrowKeySymbolRange =
                                { start =
                                    arrowToOutType.arrowKeySymbolEndLocation
                                        |> locationAddColumn -2
                                , end = arrowToOutType.arrowKeySymbolEndLocation
                                }
                            , output = output
                            }
                    )
                    inType.syntax
                    arrowToOutType.syntax
            }
        )


typeNotSpaceSeparated : Parser (WithComments (ElmSyntax.Node ElmSyntax.Type))
typeNotSpaceSeparated =
    ParserFast.oneOf4
        typeUnitOrParenthesizedOrTupleOrTriple
        typeConstructWithoutArguments
        typeVariable
        typeRecordOrRecordExtension


typeNotFunction : Parser (WithComments (ElmSyntax.Node ElmSyntax.Type))
typeNotFunction =
    ParserFast.oneOf4
        typeUnitOrParenthesizedOrTupleOrTriple
        typeConstructWithArgumentsFollowedByWhitespaceAndComments
        typeVariable
        typeRecordOrRecordExtension


typeUnitOrParenthesizedOrTupleOrTriple : Parser (WithComments (ElmSyntax.Node ElmSyntax.Type))
typeUnitOrParenthesizedOrTupleOrTriple =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf2
            (ParserFast.symbolWithEndLocation ")"
                (\unitEnd ->
                    { comments = ropeEmpty
                    , syntax =
                        { range =
                            { start = unitEnd |> locationAddColumn -2
                            , end = unitEnd
                            }
                        , value = ElmSyntax.TypeUnit
                        }
                    }
                )
            )
            (ParserFast.map4WithRange
                (\rangeAfterOpeningParen commentsBeforeFirstPart firstPart commentsAfterFirstPart lastToSecondPart ->
                    { comments =
                        commentsBeforeFirstPart
                            |> ropePrependTo firstPart.comments
                            |> ropePrependTo commentsAfterFirstPart
                            |> ropePrependTo lastToSecondPart.comments
                    , syntax =
                        { range =
                            { start = rangeAfterOpeningParen.start |> locationAddColumn -1
                            , end = rangeAfterOpeningParen.end
                            }
                        , value =
                            case lastToSecondPart.syntax of
                                Nothing ->
                                    ElmSyntax.TypeParenthesized firstPart.syntax

                                Just firstAndMaybeThirdPart ->
                                    case firstAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            ElmSyntax.TypeTuple
                                                { part0 = firstPart.syntax
                                                , part1 = firstAndMaybeThirdPart.secondPart
                                                }

                                        Just thirdPart ->
                                            ElmSyntax.TypeTriple
                                                { part0 = firstPart.syntax
                                                , part1 = firstAndMaybeThirdPart.secondPart
                                                , part2 = thirdPart
                                                }
                        }
                    }
                )
                whitespaceAndComments
                type_
                whitespaceAndComments
                (ParserFast.oneOf2
                    (ParserFast.symbol ")"
                        { comments = ropeEmpty, syntax = Nothing }
                    )
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore secondPartResult commentsAfter maybeThirdPartResult ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo secondPartResult.comments
                                        |> ropePrependTo commentsAfter
                                , syntax = Just { maybeThirdPart = maybeThirdPartResult.syntax, secondPart = secondPartResult.syntax }
                                }
                            )
                            whitespaceAndComments
                            type_
                            whitespaceAndComments
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")"
                                    { comments = ropeEmpty, syntax = Nothing }
                                )
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore thirdPartResult commentsAfter ->
                                            { comments =
                                                commentsBefore
                                                    |> ropePrependTo thirdPartResult.comments
                                                    |> ropePrependTo commentsAfter
                                            , syntax = Just thirdPartResult.syntax
                                            }
                                        )
                                        whitespaceAndComments
                                        type_
                                        whitespaceAndComments
                                        |> ParserFast.followedBySymbol ")"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


typeVariable : Parser (WithComments (ElmSyntax.Node ElmSyntax.Type))
typeVariable =
    nameLowercaseMapWithRange
        (\range var ->
            { comments = ropeEmpty
            , syntax =
                { range = range, value = ElmSyntax.TypeVariable var }
            }
        )


typeRecordOrRecordExtension : Parser (WithComments (ElmSyntax.Node ElmSyntax.Type))
typeRecordOrRecordExtension =
    ParserFast.map2WithRange
        (\range commentsBefore afterCurly ->
            case afterCurly of
                Nothing ->
                    { comments = commentsBefore
                    , syntax =
                        { range = range, value = typeRecordEmpty }
                    }

                Just afterCurlyResult ->
                    { comments =
                        commentsBefore
                            |> ropePrependTo afterCurlyResult.comments
                    , syntax =
                        { range = range, value = afterCurlyResult.syntax }
                    }
        )
        (ParserFast.symbolFollowedBy "{" whitespaceAndComments)
        (ParserFast.oneOf2
            (ParserFast.symbol "}" Nothing)
            (ParserFast.map4
                (\commentsBeforeFirstName firstNameNode commentsAfterFirstName afterFirstName ->
                    Just
                        { comments =
                            commentsBeforeFirstName
                                |> ropePrependTo commentsAfterFirstName
                                |> ropePrependTo afterFirstName.comments
                        , syntax =
                            case afterFirstName.syntax of
                                RecordExtensionExpressionAfterName recordExtensionAfterName ->
                                    ElmSyntax.TypeRecordExtension
                                        { recordVariable = firstNameNode
                                        , barKeySymbolRange =
                                            recordExtensionAfterName.barKeySymbolRange
                                        , field0 = recordExtensionAfterName.field0
                                        , field1Up = recordExtensionAfterName.field1Up
                                        }

                                FieldsAfterName fieldsAfterName ->
                                    ElmSyntax.TypeRecord
                                        ({ name = firstNameNode
                                         , colonKeySymbolRange =
                                            fieldsAfterName.firstFieldEqualsKeySymbolRange
                                         , value = fieldsAfterName.firstFieldValue
                                         }
                                            :: fieldsAfterName.tailFields
                                        )
                        }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                    ropeEmpty
                )
                nameLowercaseNodeUnderscoreSuffixingKeywords
                whitespaceAndComments
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.map3WithStartLocation
                            (\barKeySymbolEndLocation commentsBefore field0 field1Up ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo field0.comments
                                        |> ropePrependTo field1Up.comments
                                , syntax =
                                    RecordExtensionExpressionAfterName
                                        { barKeySymbolRange =
                                            { start =
                                                barKeySymbolEndLocation
                                                    |> locationAddColumn -1
                                            , end = barKeySymbolEndLocation
                                            }
                                        , field0 = field0.syntax
                                        , field1Up = field1Up.syntax
                                        }
                                }
                            )
                            whitespaceAndComments
                            typeRecordFieldDefinitionFollowedByWhitespaceAndComments
                            (manyWithComments
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore commentsWithExtraComma field ->
                                            { comments =
                                                commentsBefore
                                                    |> ropePrependTo commentsWithExtraComma
                                                    |> ropePrependTo field.comments
                                            , syntax = field.syntax
                                            }
                                        )
                                        whitespaceAndComments
                                        (ParserFast.orSucceed
                                            (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                                            ropeEmpty
                                        )
                                        typeRecordFieldDefinitionFollowedByWhitespaceAndComments
                                    )
                                )
                            )
                        )
                    )
                    (ParserFast.map4WithStartLocation
                        (\colonKeySymbolStartRange commentsBeforeFirstFieldValue firstFieldValue commentsAfterFirstFieldValue tailFields ->
                            { comments =
                                commentsBeforeFirstFieldValue
                                    |> ropePrependTo firstFieldValue.comments
                                    |> ropePrependTo commentsAfterFirstFieldValue
                                    |> ropePrependTo tailFields.comments
                            , syntax =
                                FieldsAfterName
                                    { firstFieldEqualsKeySymbolRange =
                                        { start = colonKeySymbolStartRange
                                        , end =
                                            colonKeySymbolStartRange
                                                |> locationAddColumn 1
                                        }
                                    , firstFieldValue = firstFieldValue.syntax
                                    , tailFields = tailFields.syntax
                                    }
                            }
                        )
                        (ParserFast.oneOf2OrSucceed
                            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                            (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
                            ropeEmpty
                        )
                        type_
                        whitespaceAndComments
                        (ParserFast.orSucceed
                            (ParserFast.symbolFollowedBy "," recordFieldsType)
                            { comments = ropeEmpty, syntax = [] }
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
        )


typeRecordEmpty : ElmSyntax.Type
typeRecordEmpty =
    ElmSyntax.TypeRecord []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName
        { barKeySymbolRange : TextGrid.Range
        , field0 :
            { name : ElmSyntax.Node String
            , colonKeySymbolRange : TextGrid.Range
            , value : ElmSyntax.Node ElmSyntax.Type
            }
        , field1Up :
            List
                { name : ElmSyntax.Node String
                , colonKeySymbolRange : TextGrid.Range
                , value : ElmSyntax.Node ElmSyntax.Type
                }
        }
    | FieldsAfterName
        { firstFieldEqualsKeySymbolRange : TextGrid.Range
        , firstFieldValue : ElmSyntax.Node ElmSyntax.Type
        , tailFields :
            List
                { name : ElmSyntax.Node String
                , colonKeySymbolRange : TextGrid.Range
                , value : ElmSyntax.Node ElmSyntax.Type
                }
        }


recordFieldsType :
    Parser
        (WithComments
            (List
                { name : ElmSyntax.Node String
                , colonKeySymbolRange : TextGrid.Range
                , value : ElmSyntax.Node ElmSyntax.Type
                }
            )
        )
recordFieldsType =
    ParserFast.map4
        (\commentsBefore commentsWithExtraComma head tail ->
            { comments =
                commentsWithExtraComma
                    |> ropePrependTo commentsBefore
                    |> ropePrependTo head.comments
                    |> ropePrependTo tail.comments
            , syntax = head.syntax :: tail.syntax
            }
        )
        whitespaceAndComments
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "," whitespaceAndComments)
            ropeEmpty
        )
        typeRecordFieldDefinitionFollowedByWhitespaceAndComments
        (manyWithComments
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map3
                    (\commentsBefore commentsWithExtraComma field ->
                        { comments =
                            commentsBefore
                                |> ropePrependTo commentsWithExtraComma
                                |> ropePrependTo field.comments
                        , syntax = field.syntax
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                        ropeEmpty
                    )
                    typeRecordFieldDefinitionFollowedByWhitespaceAndComments
                )
            )
        )


typeRecordFieldDefinitionFollowedByWhitespaceAndComments :
    Parser
        (WithComments
            { name : ElmSyntax.Node String
            , colonKeySymbolRange : TextGrid.Range
            , value : ElmSyntax.Node ElmSyntax.Type
            }
        )
typeRecordFieldDefinitionFollowedByWhitespaceAndComments =
    ParserFast.map5
        (\name commentsAfterName fromColonKeySymbol value commentsAfterValue ->
            { comments =
                commentsAfterName
                    |> ropePrependTo fromColonKeySymbol.comments
                    |> ropePrependTo value.comments
                    |> ropePrependTo commentsAfterValue
            , syntax =
                { name = name
                , colonKeySymbolRange =
                    { start = fromColonKeySymbol.startLocation
                    , end =
                        fromColonKeySymbol.startLocation
                            |> locationAddColumn 1
                    }
                , value = value.syntax
                }
            }
        )
        nameLowercaseNodeUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.mapWithStartLocation
            (\startLocation comments ->
                { startLocation = startLocation
                , comments = comments
                }
            )
            (ParserFast.oneOf2OrSucceed
                (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
                ropeEmpty
            )
        )
        type_
        whitespaceAndComments


typeConstructWithoutArguments : Parser (WithComments (ElmSyntax.Node ElmSyntax.Type))
typeConstructWithoutArguments =
    ParserFast.map2WithRange
        (\range startName afterStartName ->
            let
                name : { qualification : ElmSyntax.ModuleName, name : String }
                name =
                    case afterStartName of
                        Nothing ->
                            { qualification = [], name = startName }

                        Just ( qualificationAfterStartName, unqualified ) ->
                            { qualification = startName :: qualificationAfterStartName
                            , name = unqualified
                            }
            in
            { comments = ropeEmpty
            , syntax =
                { range = range
                , value =
                    ElmSyntax.TypeConstruct
                        { reference = { range = range, value = name }
                        , arguments = []
                        }
                }
            }
        )
        nameUppercase
        maybeDotNamesUppercaseTuple


maybeDotNamesUppercaseTuple : Parser (Maybe ( List String, String ))
maybeDotNamesUppercaseTuple =
    ParserFast.map2OrSucceed
        (\firstName afterFirstName ->
            case afterFirstName of
                Nothing ->
                    Just ( [], firstName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( firstName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." nameUppercase)
        (ParserFast.lazy (\() -> maybeDotNamesUppercaseTuple))
        Nothing


typeConstructWithArgumentsFollowedByWhitespaceAndComments : Parser (WithComments (ElmSyntax.Node ElmSyntax.Type))
typeConstructWithArgumentsFollowedByWhitespaceAndComments =
    ParserFast.map3
        (\nameNode commentsAfterName argsReverse ->
            let
                range : TextGrid.Range
                range =
                    case argsReverse.syntax of
                        [] ->
                            nameNode.range

                        lastArgumentNode :: _ ->
                            { start = nameNode.range.start, end = lastArgumentNode.range.end }
            in
            { comments =
                commentsAfterName
                    |> ropePrependTo argsReverse.comments
            , syntax =
                { range = range
                , value =
                    ElmSyntax.TypeConstruct
                        { reference = nameNode
                        , arguments = List.reverse argsReverse.syntax
                        }
                }
            }
        )
        (ParserFast.map2WithRange
            (\range startName afterStartName ->
                let
                    name : { qualification : ElmSyntax.ModuleName, name : String }
                    name =
                        case afterStartName of
                            Nothing ->
                                { qualification = [], name = startName }

                            Just ( qualificationAfterStartName, unqualified ) ->
                                { qualification = startName :: qualificationAfterStartName
                                , name = unqualified
                                }
                in
                { range = range, value = name }
            )
            nameUppercase
            maybeDotNamesUppercaseTuple
        )
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeResult commentsAfter ->
                        { comments =
                            typeResult.comments
                                |> ropePrependTo commentsAfter
                        , syntax = typeResult.syntax
                        }
                    )
                    typeNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )


subExpression : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
subExpression =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    expressionString

                "(" ->
                    expressionStartingWithParensOpeningIfNecessaryFollowedByRecordAccess

                "[" ->
                    expressionArray

                "{" ->
                    expressionRecordOrRecordUpdateFollowedByRecordAccess

                "." ->
                    expressionRecordAccessFunction

                "-" ->
                    expressionNegation

                "'" ->
                    expressionChar

                _ ->
                    referenceOrNumberExpression
        )


referenceOrNumberExpression : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
referenceOrNumberExpression =
    ParserFast.oneOf3
        expressionQualifiedOrVariantOrRecordConstructorReferenceFollowedByRecordAccess
        expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
        expressionNumber


followedByMultiRecordAccess : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression)) -> Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
followedByMultiRecordAccess beforeRecordAccesses =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (ParserFast.symbolFollowedBy "."
            nameLowercaseNodeUnderscoreSuffixingKeywords
        )
        beforeRecordAccesses
        (\fieldNode leftResult ->
            { comments = leftResult.comments
            , syntax =
                { range = { start = leftResult.syntax.range.start, end = fieldNode.range.end }
                , value =
                    ElmSyntax.ExpressionRecordAccess
                        { record = leftResult.syntax
                        , field = fieldNode
                        }
                }
            }
        )
        Basics.identity


multiRecordAccessMap : (List (ElmSyntax.Node String) -> res) -> ParserFast.Parser res
multiRecordAccessMap fieldsToRes =
    ParserFast.loopWhileSucceeds
        (ParserFast.symbolFollowedBy "." nameLowercaseNode)
        []
        (::)
        (\reversed -> fieldsToRes (List.reverse reversed))


precedence1ApR : InfixOperatorInfo
precedence1ApR =
    infixLeft 1 "|>"


precedence1ApL : InfixOperatorInfo
precedence1ApL =
    infixRight 1 "<|"


precedence2Or : InfixOperatorInfo
precedence2Or =
    infixRight 2 "||"


precedence3And : InfixOperatorInfo
precedence3And =
    infixRight 3 "&&"


precedence4Eq : InfixOperatorInfo
precedence4Eq =
    infixNonAssociative 4 "=="


precedence4Neq : InfixOperatorInfo
precedence4Neq =
    infixNonAssociative 4 "!="


precedence4Le : InfixOperatorInfo
precedence4Le =
    infixNonAssociative 4 "<="


precedence4Ge : InfixOperatorInfo
precedence4Ge =
    infixNonAssociative 4 ">="


precedence4Gt : InfixOperatorInfo
precedence4Gt =
    infixNonAssociative 4 ">"


precedence4Lt : InfixOperatorInfo
precedence4Lt =
    infixNonAssociative 4 "<"


precedence5append : InfixOperatorInfo
precedence5append =
    infixRight 5 "++"


precedence5Cons : InfixOperatorInfo
precedence5Cons =
    infixRight 5 "::"


precedence5Keep : InfixOperatorInfo
precedence5Keep =
    infixLeft 5 "|="


precedence6Add : InfixOperatorInfo
precedence6Add =
    infixLeft 6 "+"


precedence6Sub : InfixOperatorInfo
precedence6Sub =
    infixLeft 6 "-"


precedence6Ignore : InfixOperatorInfo
precedence6Ignore =
    infixLeft 6 "|."


precedence7Idiv : InfixOperatorInfo
precedence7Idiv =
    infixLeft 7 "//"


precedence7Mul : InfixOperatorInfo
precedence7Mul =
    infixLeft 7 "*"


precedence7Fdiv : InfixOperatorInfo
precedence7Fdiv =
    infixLeft 7 "/"


precedence7Slash : InfixOperatorInfo
precedence7Slash =
    infixRight 7 "</>"


precedence8QuestionMark : InfixOperatorInfo
precedence8QuestionMark =
    infixLeft 8 "<?>"


precedence8Pow : InfixOperatorInfo
precedence8Pow =
    infixRight 8 "^"


precedence9ComposeR : InfixOperatorInfo
precedence9ComposeR =
    infixRight 9 ">>"


precedence9ComposeL : InfixOperatorInfo
precedence9ComposeL =
    infixLeft 9 "<<"


{-| [`Parser`](#Parser) for an [`ElmSyntax.Expression`](https://elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression#Expression)
-}
expression : Parser { comments : Comments, syntax : ElmSyntax.Node ElmSyntax.Expression }
expression =
    expressionFollowedByWhitespaceAndComments


expressionFollowedByWhitespaceAndComments : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionFollowedByWhitespaceAndComments =
    extendedSubExpressionFollowedByWhitespaceAndComments
        { afterCommitting = .extensionRightParser
        , validateRightPrecedence = Just
        }


expressionArray : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionArray =
    ParserFast.symbolFollowedBy "["
        (ParserFast.map2WithRange
            (\range commentsBefore elements ->
                { comments = commentsBefore |> ropePrependTo elements.comments
                , syntax =
                    { range = { start = { line = range.start.line, column = range.start.column - 1 }, end = range.end }
                    , value = elements.syntax
                    }
                }
            )
            whitespaceAndComments
            (ParserFast.oneOf2
                (ParserFast.symbol "]" { comments = ropeEmpty, syntax = ElmSyntax.ExpressionList [] })
                (ParserFast.map3
                    (\commentsBeforeHead head tail ->
                        { comments =
                            commentsBeforeHead
                                |> ropePrependTo head.comments
                                |> ropePrependTo tail.comments
                        , syntax = ElmSyntax.ExpressionList (head.syntax :: tail.syntax)
                        }
                    )
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                        ropeEmpty
                    )
                    expressionFollowedByWhitespaceAndComments
                    (manyWithComments
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map3
                                (\commentsBefore commentsWithExtraComma expressionResult ->
                                    { comments =
                                        commentsBefore
                                            |> ropePrependTo commentsWithExtraComma
                                            |> ropePrependTo expressionResult.comments
                                    , syntax = expressionResult.syntax
                                    }
                                )
                                whitespaceAndComments
                                (ParserFast.orSucceed
                                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                                    ropeEmpty
                                )
                                expressionFollowedByWhitespaceAndComments
                            )
                        )
                    )
                    |> ParserFast.followedBySymbol "]"
                )
            )
        )


expressionRecordOrRecordUpdateFollowedByRecordAccess : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionRecordOrRecordUpdateFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map2WithRange
            (\range commentsBefore afterCurly ->
                { comments =
                    commentsBefore
                        |> ropePrependTo afterCurly.comments
                , syntax = { range = rangeMoveStartLeftByOneColumn range, value = afterCurly.syntax }
                }
            )
            whitespaceAndComments
            recordOrRecordUpdateContentsFollowedByCurlyEnd
            |> followedByMultiRecordAccess
        )


recordOrRecordUpdateContentsFollowedByCurlyEnd : Parser (WithComments ElmSyntax.Expression)
recordOrRecordUpdateContentsFollowedByCurlyEnd =
    -- crimes were committed here
    ParserFast.oneOf3
        (ParserFast.symbol "}" { comments = ropeEmpty, syntax = ElmSyntax.ExpressionRecord [] })
        (ParserFast.mapOrFail identity
            (ParserFast.map5
                (\firstFieldNameOrUpdatedRecordVariable commentsAfterName afterNameBeforeFields field1Up commentsBeforeClosingCurly ->
                    let
                        comments : Comments
                        comments =
                            commentsAfterName
                                |> ropePrependTo afterNameBeforeFields.comments
                                |> ropePrependTo field1Up.comments
                                |> ropePrependTo commentsBeforeClosingCurly
                    in
                    Just
                        { comments = comments
                        , syntax =
                            case afterNameBeforeFields.syntax of
                                RecordUpdateFirstSetter recordUpdateFirstSetter ->
                                    ElmSyntax.ExpressionRecordUpdate
                                        { recordVariable = firstFieldNameOrUpdatedRecordVariable
                                        , barKeySymbolRange = recordUpdateFirstSetter.barKeySymbolRange
                                        , field0 = recordUpdateFirstSetter.field
                                        , field1Up = field1Up.syntax
                                        }

                                FieldsFirstValue firstFieldValue ->
                                    ElmSyntax.ExpressionRecord
                                        ({ name = firstFieldNameOrUpdatedRecordVariable
                                         , equalsKeySymbolRange = firstFieldValue.equalsKeySymbolRange
                                         , value = firstFieldValue.value
                                         }
                                            :: field1Up.syntax
                                        )
                        }
                )
                (nameLowercaseMapWithRange
                    (\range recordVariable ->
                        { range = range, value = recordVariable }
                    )
                )
                whitespaceAndComments
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.map2WithStartLocation
                            (\barKeySymbolEndLocation commentsBefore setterResult ->
                                { comments = commentsBefore |> ropePrependTo setterResult.comments
                                , syntax =
                                    RecordUpdateFirstSetter
                                        { barKeySymbolRange =
                                            { start =
                                                barKeySymbolEndLocation
                                                    |> locationAddColumn -1
                                            , end = barKeySymbolEndLocation
                                            }
                                        , field = setterResult.syntax
                                        }
                                }
                            )
                            whitespaceAndComments
                            recordSetterNodeFollowedByWhitespaceAndComments
                        )
                    )
                    (ParserFast.map2WithStartLocation
                        (\equalsKeySymbolStartLocation commentsBefore valueResult ->
                            { comments =
                                commentsBefore
                                    |> ropePrependTo valueResult.comments
                            , syntax =
                                FieldsFirstValue
                                    { equalsKeySymbolRange =
                                        { start = equalsKeySymbolStartLocation
                                        , end =
                                            equalsKeySymbolStartLocation
                                                |> locationAddColumn 1
                                        }
                                    , value = valueResult.syntax
                                    }
                            }
                        )
                        (ParserFast.oneOf2OrSucceed
                            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                            (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
                            ropeEmpty
                        )
                        expressionFollowedByWhitespaceAndComments
                    )
                )
                recordFields
                (whitespaceAndComments |> ParserFast.followedBySymbol "}")
            )
        )
        -- prefixed comma
        (ParserFast.map2
            (\recordFieldsResult commentsAfterFields ->
                { comments =
                    recordFieldsResult.comments
                        |> ropePrependTo commentsAfterFields
                , syntax =
                    ElmSyntax.ExpressionRecord recordFieldsResult.syntax
                }
            )
            recordFields
            (whitespaceAndComments |> ParserFast.followedBySymbol "}")
        )


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter
        { barKeySymbolRange : TextGrid.Range
        , field :
            { name : ElmSyntax.Node String
            , equalsKeySymbolRange : TextGrid.Range
            , value : ElmSyntax.Node ElmSyntax.Expression
            }
        }
    | FieldsFirstValue
        { equalsKeySymbolRange : TextGrid.Range
        , value : ElmSyntax.Node ElmSyntax.Expression
        }


recordFields :
    Parser
        (WithComments
            (List
                { name : ElmSyntax.Node String
                , equalsKeySymbolRange : TextGrid.Range
                , value : ElmSyntax.Node ElmSyntax.Expression
                }
            )
        )
recordFields =
    manyWithComments
        (ParserFast.symbolFollowedBy ","
            (ParserFast.map3
                (\commentsBefore commentsWithExtraComma setterResult ->
                    { comments =
                        commentsBefore
                            |> ropePrependTo commentsWithExtraComma
                            |> ropePrependTo setterResult.comments
                    , syntax = setterResult.syntax
                    }
                )
                whitespaceAndComments
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                    ropeEmpty
                )
                recordSetterNodeFollowedByWhitespaceAndComments
            )
        )


recordSetterNodeFollowedByWhitespaceAndComments :
    Parser
        (WithComments
            { name : ElmSyntax.Node String
            , equalsKeySymbolRange : TextGrid.Range
            , value : ElmSyntax.Node ElmSyntax.Expression
            }
        )
recordSetterNodeFollowedByWhitespaceAndComments =
    ParserFast.map4
        (\nameNode commentsAfterName fromEqualsKeySymbol valueResult ->
            { comments =
                commentsAfterName
                    |> ropePrependTo fromEqualsKeySymbol.comments
                    |> ropePrependTo valueResult.comments
            , syntax =
                { name = nameNode
                , equalsKeySymbolRange =
                    { start = fromEqualsKeySymbol.startLocation
                    , end =
                        fromEqualsKeySymbol.startLocation
                            |> locationAddColumn 1
                    }
                , value = valueResult.syntax
                }
            }
        )
        nameLowercaseNodeUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.mapWithStartLocation
            (\startLocation comments ->
                { startLocation = startLocation, comments = comments }
            )
            (ParserFast.oneOf2OrSucceed
                (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
                ropeEmpty
            )
        )
        expressionFollowedByWhitespaceAndComments


expressionString : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionString =
    singleOrTripleQuotedStringLiteralMapWithRange
        (\range stringAndLineSpread ->
            { comments = ropeEmpty
            , syntax =
                { range = range, value = ElmSyntax.ExpressionString stringAndLineSpread }
            }
        )


expressionChar : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionChar =
    characterLiteralMapWithRange
        (\range char ->
            { comments = ropeEmpty
            , syntax =
                { range = range, value = ElmSyntax.ExpressionChar char }
            }
        )


expressionLambdaFollowedByWhitespaceAndComments : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionLambdaFollowedByWhitespaceAndComments =
    ParserFast.map6WithStartLocation
        (\start commentsAfterBackslash parameter0 commentsAfterParameter0 parameter1UpToArrowKeySymbol commentsAfterArrowKeySymbol expressionResult ->
            { comments =
                commentsAfterBackslash
                    |> ropePrependTo parameter0.comments
                    |> ropePrependTo commentsAfterParameter0
                    |> ropePrependTo parameter1UpToArrowKeySymbol.comments
                    |> ropePrependTo commentsAfterArrowKeySymbol
                    |> ropePrependTo expressionResult.comments
            , syntax =
                { range = { start = start, end = expressionResult.syntax.range.end }
                , value =
                    ElmSyntax.ExpressionLambda
                        { parameter0 = parameter0.syntax
                        , parameter1Up =
                            parameter1UpToArrowKeySymbol.parameters
                        , arrowKeySymbolRange =
                            { start =
                                parameter1UpToArrowKeySymbol.endLocation
                                    |> locationAddColumn -2
                            , end = parameter1UpToArrowKeySymbol.endLocation
                            }
                        , result = expressionResult.syntax
                        }
                }
            }
        )
        (ParserFast.symbolFollowedBy "\\" whitespaceAndComments)
        patternNotSpaceSeparated
        whitespaceAndComments
        (ParserFast.mapWithEndLocation
            (\endLocation parameters ->
                { endLocation = endLocation
                , parameters = parameters.syntax
                , comments = parameters.comments
                }
            )
            (untilWithComments
                (ParserFast.oneOf2
                    (ParserFast.symbol "->" ())
                    (ParserFast.symbol "=>" ())
                )
                (ParserFast.map2
                    (\patternResult commentsAfter ->
                        { comments =
                            patternResult.comments
                                |> ropePrependTo commentsAfter
                        , syntax = patternResult.syntax
                        }
                    )
                    patternNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments


locationAddColumn : Int -> TextGrid.Location -> TextGrid.Location
locationAddColumn columnPlus location =
    { line = location.line
    , column = location.column + columnPlus
    }


expressionWhenIsFollowedByOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionWhenIsFollowedByOptimisticLayout =
    ParserFast.map5WithStartLocation
        (\start commentsAfterCase casedExpressionResult ofKeywordRange commentsAfterOf casesResult ->
            let
                ( case0, case1UpReverse ) =
                    casesResult.syntax
            in
            { comments =
                commentsAfterCase
                    |> ropePrependTo casedExpressionResult.comments
                    |> ropePrependTo commentsAfterOf
                    |> ropePrependTo casesResult.comments
            , syntax =
                { range =
                    { start = start
                    , end =
                        case case1UpReverse of
                            lastCase :: _ ->
                                lastCase.result.range.end

                            [] ->
                                case0.result.range.end
                    }
                , value =
                    ElmSyntax.ExpressionCaseOf
                        { matched = casedExpressionResult.syntax
                        , ofKeywordRange = ofKeywordRange
                        , case0 = case0
                        , case1Up = case1UpReverse |> List.reverse
                        }
                }
            }
        )
        (ParserFast.keywordFollowedBy "case" whitespaceAndComments)
        expressionFollowedByWhitespaceAndComments
        (ParserFast.keywordRange "of")
        whitespaceAndComments
        (ParserFast.withIndentSetToColumn
            casesFollowedByWhitespaceAndComments
        )


casesFollowedByWhitespaceAndComments :
    Parser
        (WithComments
            ( { pattern : ElmSyntax.Node ElmSyntax.Pattern
              , arrowKeySymbolRange : TextGrid.Range
              , result : ElmSyntax.Node ElmSyntax.Expression
              }
            , List
                { pattern : ElmSyntax.Node ElmSyntax.Pattern
                , arrowKeySymbolRange : TextGrid.Range
                , result : ElmSyntax.Node ElmSyntax.Expression
                }
            )
        )
casesFollowedByWhitespaceAndComments =
    ParserFast.map6
        (\firstCasePatternResult commentsAfterFirstCasePattern arrowKeySymbolRange commentsAfterFirstCaseArrowRight firstCaseExpressionResult lastToSecondCase ->
            { comments =
                firstCasePatternResult.comments
                    |> ropePrependTo commentsAfterFirstCasePattern
                    |> ropePrependTo commentsAfterFirstCaseArrowRight
                    |> ropePrependTo firstCaseExpressionResult.comments
                    |> ropePrependTo lastToSecondCase.comments
            , syntax =
                ( { pattern = firstCasePatternResult.syntax
                  , arrowKeySymbolRange = arrowKeySymbolRange
                  , result = firstCaseExpressionResult.syntax
                  }
                , lastToSecondCase.syntax
                )
            }
        )
        pattern
        whitespaceAndComments
        (ParserFast.symbolWithRange "->" identity)
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments
        (manyWithCommentsReverse caseStatementFollowedByWhitespaceAndComments)


caseStatementFollowedByWhitespaceAndComments :
    Parser
        (WithComments
            { pattern : ElmSyntax.Node ElmSyntax.Pattern
            , arrowKeySymbolRange : TextGrid.Range
            , result : ElmSyntax.Node ElmSyntax.Expression
            }
        )
caseStatementFollowedByWhitespaceAndComments =
    topIndentedFollowedBy
        (ParserFast.map5
            (\patternResult commentsBeforeArrowRight arrowKeySymbolRange commentsAfterArrowRight expr ->
                { comments =
                    patternResult.comments
                        |> ropePrependTo commentsBeforeArrowRight
                        |> ropePrependTo commentsAfterArrowRight
                        |> ropePrependTo expr.comments
                , syntax =
                    { pattern = patternResult.syntax
                    , arrowKeySymbolRange = arrowKeySymbolRange
                    , result = expr.syntax
                    }
                }
            )
            pattern
            whitespaceAndComments
            (ParserFast.symbolWithRange "->" identity)
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
        )


letExpressionFollowedByOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
letExpressionFollowedByOptimisticLayout =
    ParserFast.map3WithStartLocation
        (\start letDeclarationsResult commentsAfterIn expressionResult ->
            { comments =
                letDeclarationsResult.comments
                    |> ropePrependTo commentsAfterIn
                    |> ropePrependTo expressionResult.comments
            , syntax =
                { range = { start = start, end = expressionResult.syntax.range.end }
                , value =
                    ElmSyntax.ExpressionLetIn
                        { declaration0 = letDeclarationsResult.declaration0
                        , declaration1Up = letDeclarationsResult.declaration1Up
                        , inKeywordRange =
                            { start =
                                letDeclarationsResult.locationAfterInKeyword
                                    |> locationAddColumn -2
                            , end = letDeclarationsResult.locationAfterInKeyword
                            }
                        , result = expressionResult.syntax
                        }
                }
            }
        )
        (ParserFast.withIndentSetToColumn
            (ParserFast.keywordFollowedBy "let"
                (ParserFast.map2
                    (\commentsAfterLet letDeclarationsResult ->
                        { comments =
                            commentsAfterLet
                                |> ropePrependTo letDeclarationsResult.comments
                        , declaration0 = letDeclarationsResult.declaration0
                        , declaration1Up = letDeclarationsResult.declaration1Up
                        , locationAfterInKeyword =
                            letDeclarationsResult.locationAfterInKeyword
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.mapWithEndLocation
                        (\locationAfterInKeyword letDeclarationsResult ->
                            { locationAfterInKeyword = locationAfterInKeyword
                            , declaration0 = letDeclarationsResult.declaration0
                            , declaration1Up = letDeclarationsResult.declaration1Up
                            , comments = letDeclarationsResult.comments
                            }
                        )
                        (ParserFast.withIndentSetToColumn letDeclarationsIn)
                    )
                )
            )
        )
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments


letDeclarationsIn :
    Parser
        { comments : Comments
        , declaration0 : ElmSyntax.Node ElmSyntax.LetDeclaration
        , declaration1Up : List (ElmSyntax.Node ElmSyntax.LetDeclaration)
        }
letDeclarationsIn =
    topIndentedFollowedBy
        (ParserFast.map3
            (\letDeclaration0Result commentsAfterLetDeclaration0 letDeclaration1UpResult ->
                { comments =
                    letDeclaration0Result.comments
                        |> ropePrependTo commentsAfterLetDeclaration0
                        |> ropePrependTo letDeclaration1UpResult.comments
                , declaration0 = letDeclaration0Result.syntax
                , declaration1Up = letDeclaration1UpResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunctionFollowedByOptimisticLayout
                letDestructuringDeclarationFollowedByOptimisticLayout
            )
            whitespaceAndComments
            (untilWithComments
                (ParserFast.keyword "in" ())
                letBlockElementFollowedByOptimisticLayout
            )
        )


letBlockElementFollowedByOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.LetDeclaration))
letBlockElementFollowedByOptimisticLayout =
    topIndentedFollowedBy
        (ParserFast.oneOf2
            letFunctionFollowedByOptimisticLayout
            letDestructuringDeclarationFollowedByOptimisticLayout
        )


letDestructuringDeclarationFollowedByOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.LetDeclaration))
letDestructuringDeclarationFollowedByOptimisticLayout =
    ParserFast.map5
        (\patternResult commentsAfterPattern equalsKeySymbolRange commentsAfterEquals expressionResult ->
            { comments =
                patternResult.comments
                    |> ropePrependTo commentsAfterPattern
                    |> ropePrependTo commentsAfterEquals
                    |> ropePrependTo expressionResult.comments
            , syntax =
                { range = { start = patternResult.syntax.range.start, end = expressionResult.syntax.range.end }
                , value =
                    ElmSyntax.LetDestructuring
                        { pattern = patternResult.syntax
                        , equalsKeySymbolRange = equalsKeySymbolRange
                        , expression = expressionResult.syntax
                        }
                }
            }
        )
        patternNotSpaceSeparated
        whitespaceAndComments
        (ParserFast.symbolWithRange "=" identity)
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments


letFunctionFollowedByOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.LetDeclaration))
letFunctionFollowedByOptimisticLayout =
    ParserFast.oneOf2
        (ParserFast.map6WithStartLocation
            (\startNameStart startNameNode commentsAfterStartName maybeSignature parametersToEquals commentsAfterEqual expressionResult ->
                case maybeSignature of
                    Nothing ->
                        { comments =
                            commentsAfterStartName
                                |> ropePrependTo parametersToEquals.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo expressionResult.comments
                        , syntax =
                            { range = { start = startNameStart, end = expressionResult.syntax.range.end }
                            , value =
                                ElmSyntax.LetValueOrFunctionDeclaration
                                    { signature = Nothing
                                    , name = startNameNode.value
                                    , implementationNameRange = startNameNode.range
                                    , parameters = parametersToEquals.parameters
                                    , equalsKeySymbolRange =
                                        parametersToEquals.equalsKeySymbolRange
                                    , result = expressionResult.syntax
                                    }
                            }
                        }

                    Just signature ->
                        { comments =
                            (commentsAfterStartName |> ropePrependTo signature.comments)
                                |> ropePrependTo parametersToEquals.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo expressionResult.comments
                        , syntax =
                            { range = { start = startNameStart, end = expressionResult.syntax.range.end }
                            , value =
                                ElmSyntax.LetValueOrFunctionDeclaration
                                    { signature =
                                        Just
                                            { name = startNameNode
                                            , type_ = signature.type_
                                            }
                                    , name = signature.implementationName.value
                                    , implementationNameRange =
                                        signature.implementationName.range
                                    , parameters = parametersToEquals.parameters
                                    , equalsKeySymbolRange =
                                        parametersToEquals.equalsKeySymbolRange
                                    , result = expressionResult.syntax
                                    }
                            }
                        }
            )
            nameLowercaseNodeUnderscoreSuffixingKeywords
            whitespaceAndComments
            (ParserFast.map4OrSucceed
                (\commentsBeforeType typeResult implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeType
                                |> ropePrependTo typeResult.comments
                                |> ropePrependTo implementationName.comments
                                |> ropePrependTo afterImplementationName
                        , implementationName = implementationName.syntax
                        , type_ = typeResult.syntax
                        }
                )
                (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                type_
                (whitespaceAndCommentsEndsTopIndentedFollowedBy
                    nameLowercaseNodeUnderscoreSuffixingKeywords
                )
                whitespaceAndComments
                Nothing
            )
            parameterPatternsEquals
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
            |> ParserFast.validate
                (\letDeclarationResult ->
                    case letDeclarationResult.syntax.value of
                        ElmSyntax.LetDestructuring _ ->
                            True

                        ElmSyntax.LetValueOrFunctionDeclaration letFunctionDeclaration ->
                            case letFunctionDeclaration.signature of
                                Nothing ->
                                    True

                                Just letSignature ->
                                    letFunctionDeclaration.name
                                        == letSignature.name.value
                )
        )
        (ParserFast.map8WithStartLocation
            (\start commentsBeforeType typeResult commentsBetweenTypeAndName nameNode afterImplementationName parametersToEquals commentsAfterEqual result ->
                { comments =
                    commentsBeforeType
                        |> ropePrependTo typeResult.comments
                        |> ropePrependTo commentsBetweenTypeAndName
                        |> ropePrependTo afterImplementationName
                        |> ropePrependTo parametersToEquals.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    { range = { start = start, end = result.syntax.range.end }
                    , value =
                        ElmSyntax.LetValueOrFunctionDeclaration
                            { signature =
                                Just
                                    { name =
                                        { range = { start = start, end = start }
                                        , value = nameNode.value
                                        }
                                    , type_ = typeResult.syntax
                                    }
                            , name = nameNode.value
                            , implementationNameRange = nameNode.range
                            , parameters = parametersToEquals.parameters
                            , equalsKeySymbolRange =
                                parametersToEquals.equalsKeySymbolRange
                            , result = result.syntax
                            }
                    }
                }
            )
            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
            type_
            whitespaceAndCommentsEndsTopIndented
            nameLowercaseNodeUnderscoreSuffixingKeywords
            whitespaceAndComments
            parameterPatternsEquals
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
        )


expressionNumber : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionNumber =
    ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
        (\range n ->
            { comments = ropeEmpty
            , syntax = { range = range, value = ElmSyntax.ExpressionFloat n }
            }
        )
        (\range n ->
            { comments = ropeEmpty
            , syntax =
                { range = range
                , value =
                    ElmSyntax.ExpressionInteger
                        { value = n, base = ElmSyntax.IntBase10 }
                }
            }
        )
        (\range n ->
            { comments = ropeEmpty
            , syntax =
                { range = range
                , value =
                    ElmSyntax.ExpressionInteger
                        { value = n, base = ElmSyntax.IntBase16 }
                }
            }
        )


expressionIfThenElseFollowedByOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionIfThenElseFollowedByOptimisticLayout =
    ParserFast.map8WithStartLocation
        (\start commentsAfterIf condition thenKeywordRange commentsAfterThen onTrue elseKeywordRange commentsAfterElse onFalse ->
            { comments =
                commentsAfterIf
                    |> ropePrependTo condition.comments
                    |> ropePrependTo commentsAfterThen
                    |> ropePrependTo onTrue.comments
                    |> ropePrependTo commentsAfterElse
                    |> ropePrependTo onFalse.comments
            , syntax =
                { range = { start = start, end = onFalse.syntax.range.end }
                , value =
                    ElmSyntax.ExpressionIfThenElse
                        { condition = condition.syntax
                        , thenKeywordRange = thenKeywordRange
                        , onTrue = onTrue.syntax
                        , elseKeywordRange = elseKeywordRange
                        , onFalse = onFalse.syntax
                        }
                }
            }
        )
        (ParserFast.keywordFollowedBy "if" whitespaceAndComments)
        expressionFollowedByWhitespaceAndComments
        (ParserFast.keywordRange "then")
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments
        (ParserFast.keywordRange "else")
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments


expressionNegation : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionNegation =
    ParserFast.symbolBacktrackableFollowedBy "-"
        (ParserFast.offsetSourceAndThen
            (\offset source ->
                case String.slice (offset - 2) (offset - 1) source of
                    " " ->
                        negationAfterMinus

                    -- not "\n" or "\r" since expressions are always indented
                    "(" ->
                        negationAfterMinus

                    ")" ->
                        negationAfterMinus

                    -- from the end of a multiline comment
                    "}" ->
                        negationAfterMinus

                    -- from lambda arrow
                    ">" ->
                        negationAfterMinus

                    -- from field or assignment
                    "=" ->
                        negationAfterMinus

                    -- from list or tuple or triple
                    "," ->
                        negationAfterMinus

                    -- from let...in
                    "n" ->
                        if
                            case String.slice (offset - 3) (offset - 2) source of
                                "i" ->
                                    Basics.not
                                        (String.all Unicode.isLatinAlphaNumOrUnderscoreFast
                                            (String.slice (offset - 4) (offset - 3) source)
                                        )

                                _ ->
                                    False
                        then
                            negationAfterMinus

                        else
                            ParserFast.problem

                    _ ->
                        ParserFast.problem
            )
        )


negationAfterMinus : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
negationAfterMinus =
    ParserFast.map
        (\subExpressionResult ->
            { comments = subExpressionResult.comments
            , syntax =
                { range =
                    { start =
                        { line = subExpressionResult.syntax.range.start.line
                        , column = subExpressionResult.syntax.range.start.column - 1
                        }
                    , end = subExpressionResult.syntax.range.end
                    }
                , value = ElmSyntax.ExpressionNegation subExpressionResult.syntax
                }
            }
        )
        subExpression


expressionQualifiedOrVariantOrRecordConstructorReferenceFollowedByRecordAccess : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionQualifiedOrVariantOrRecordConstructorReferenceFollowedByRecordAccess =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = ropeEmpty
            , syntax =
                { range = range
                , value =
                    case after of
                        Nothing ->
                            ElmSyntax.ExpressionReference { qualification = [], name = firstName }

                        Just ( qualificationAfter, unqualified ) ->
                            ElmSyntax.ExpressionReference
                                { qualification = firstName :: qualificationAfter
                                , name = unqualified
                                }
                }
            }
        )
        nameUppercase
        maybeDotReferenceExpressionTuple
        |> followedByMultiRecordAccess


maybeDotReferenceExpressionTuple : Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    ParserFast.orSucceed
        (ParserFast.symbolFollowedBy "."
            (ParserFast.oneOf2Map
                Just
                (ParserFast.map2
                    (\firstName after ->
                        case after of
                            Nothing ->
                                ( [], firstName )

                            Just ( qualificationAfter, unqualified ) ->
                                ( firstName :: qualificationAfter, unqualified )
                    )
                    nameUppercase
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                (\name -> Just ( [], name ))
                nameLowercaseUnderscoreSuffixingKeywords
            )
        )
        Nothing


expressionUnqualifiedFunctionReferenceFollowedByRecordAccess : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionUnqualifiedFunctionReferenceFollowedByRecordAccess =
    nameLowercaseMapWithRange
        (\range unqualified ->
            { comments = ropeEmpty
            , syntax =
                { range = range
                , value =
                    ElmSyntax.ExpressionReference
                        { qualification = [], name = unqualified }
                }
            }
        )
        |> followedByMultiRecordAccess


expressionRecordAccessFunction : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionRecordAccessFunction =
    ParserFast.symbolFollowedBy "."
        (nameLowercaseMapWithRange
            (\range field ->
                { comments = ropeEmpty
                , syntax =
                    { range = range |> rangeMoveStartLeftByOneColumn
                    , value = ElmSyntax.ExpressionRecordAccessFunction ("." ++ field)
                    }
                }
            )
        )


rangeMoveStartLeftByOneColumn : TextGrid.Range -> TextGrid.Range
rangeMoveStartLeftByOneColumn range =
    { start = { line = range.start.line, column = range.start.column - 1 }
    , end = range.end
    }


expressionStartingWithParensOpeningIfNecessaryFollowedByRecordAccess : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionStartingWithParensOpeningIfNecessaryFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf3
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = ropeEmpty
                    , syntax =
                        { range = { start = { line = end.line, column = end.column - 2 }, end = end }
                        , value = ElmSyntax.ExpressionUnit
                        }
                    }
                )
            )
            allowedPrefixOperatorFollowedByClosingParensOneOf
            expressionParenthesizedOrTupleOrTripleAfterOpeningParens
        )


allowedPrefixOperatorFollowedByClosingParensOneOf : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
allowedPrefixOperatorFollowedByClosingParensOneOf =
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
        (\operatorRange operator ->
            { comments = ropeEmpty
            , syntax =
                { range =
                    { start = { line = operatorRange.start.line, column = operatorRange.start.column - 1 }
                    , end = { line = operatorRange.end.line, column = operatorRange.end.column + 1 }
                    }
                , value = ElmSyntax.ExpressionOperatorFunction operator
                }
            }
        )
        isOperatorSymbolCharAsString
        isAllowedOperatorToken
        ")"


expressionParenthesizedOrTupleOrTripleAfterOpeningParens : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
expressionParenthesizedOrTupleOrTripleAfterOpeningParens =
    ParserFast.map4WithRange
        (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart commentsAfterFirstPart tailParts ->
            { comments =
                commentsBeforeFirstPart
                    |> ropePrependTo firstPart.comments
                    |> ropePrependTo commentsAfterFirstPart
                    |> ropePrependTo tailParts.comments
            , syntax =
                case tailParts.syntax of
                    TupledParenthesizedFollowedByRecordAccesses recordAccesses ->
                        case recordAccesses of
                            [] ->
                                { range =
                                    { start = { line = rangeAfterOpeningParens.start.line, column = rangeAfterOpeningParens.start.column - 1 }
                                    , end = rangeAfterOpeningParens.end
                                    }
                                , value = ElmSyntax.ExpressionParenthesized firstPart.syntax
                                }

                            firstRecordAccess :: _ ->
                                let
                                    range : TextGrid.Range
                                    range =
                                        { start = rangeAfterOpeningParens.start |> locationAddColumn -1
                                        , end = firstRecordAccess.range.start |> locationAddColumn -1
                                        }

                                    parenthesizedNode : ElmSyntax.Node ElmSyntax.Expression
                                    parenthesizedNode =
                                        { range = range
                                        , value = ElmSyntax.ExpressionParenthesized firstPart.syntax
                                        }
                                in
                                recordAccesses
                                    |> List.foldl
                                        (\fieldNode leftNode ->
                                            { range =
                                                { start = leftNode.range.start, end = fieldNode.range.end }
                                            , value =
                                                ElmSyntax.ExpressionRecordAccess
                                                    { record = leftNode
                                                    , field = fieldNode
                                                    }
                                            }
                                        )
                                        parenthesizedNode

                    TupledTwoOrThree ( secondPart, maybeThirdPart ) ->
                        { range =
                            { start = { line = rangeAfterOpeningParens.start.line, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                        , value =
                            case maybeThirdPart of
                                Nothing ->
                                    ElmSyntax.ExpressionTuple
                                        { part0 = firstPart.syntax
                                        , part1 = secondPart
                                        }

                                Just thirdPart ->
                                    ElmSyntax.ExpressionTriple
                                        { part0 = firstPart.syntax
                                        , part1 = secondPart
                                        , part2 = thirdPart
                                        }
                        }
            }
        )
        whitespaceAndComments
        expression
        whitespaceAndComments
        (ParserFast.oneOf2
            (ParserFast.symbolFollowedBy ")"
                (multiRecordAccessMap
                    (\recordAccesses -> { comments = ropeEmpty, syntax = TupledParenthesizedFollowedByRecordAccesses recordAccesses })
                )
            )
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map4
                    (\commentsBefore partResult commentsAfter maybeThirdPart ->
                        { comments =
                            commentsBefore
                                |> ropePrependTo partResult.comments
                                |> ropePrependTo commentsAfter
                                |> ropePrependTo maybeThirdPart.comments
                        , syntax = TupledTwoOrThree ( partResult.syntax, maybeThirdPart.syntax )
                        }
                    )
                    whitespaceAndComments
                    expression
                    whitespaceAndComments
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map3
                                (\commentsBefore partResult commentsAfter ->
                                    { comments =
                                        commentsBefore
                                            |> ropePrependTo partResult.comments
                                            |> ropePrependTo commentsAfter
                                    , syntax = Just partResult.syntax
                                    }
                                )
                                whitespaceAndComments
                                expression
                                whitespaceAndComments
                                |> ParserFast.followedBySymbol ")"
                            )
                        )
                    )
                )
            )
        )


type Tupled
    = TupledParenthesizedFollowedByRecordAccesses (List (ElmSyntax.Node String))
    | TupledTwoOrThree
        ( ElmSyntax.Node ElmSyntax.Expression
        , Maybe (ElmSyntax.Node ElmSyntax.Expression)
        )



---


extendedSubExpressionFollowedByWhitespaceAndComments :
    { afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
    , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
extendedSubExpressionFollowedByWhitespaceAndComments info =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (infixOperatorAndThen info)
        subExpressionMaybeAppliedFollowedByWhitespaceAndComments
        (\extensionRightResult leftNodeWithComments ->
            { comments =
                leftNodeWithComments.comments
                    |> ropePrependTo extensionRightResult.comments
            , syntax =
                leftNodeWithComments.syntax
                    |> applyExtensionRight extensionRightResult.syntax
            }
        )
        Basics.identity


extensionRightParser :
    { afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
    , direction : ElmSyntax.InfixDirection
    , symbol : String
    , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments ExtensionRight)
extensionRightParser extensionRightInfo =
    ParserFast.map2WithStartLocation
        (\operatorEndLocation commentsBefore right ->
            { comments = commentsBefore |> ropePrependTo right.comments
            , syntax =
                ExtendRightByOperation
                    { symbol =
                        { range =
                            { start =
                                operatorEndLocation
                                    |> locationAddColumn -(extensionRightInfo.symbol |> String.length)
                            , end = operatorEndLocation
                            }
                        , value = extensionRightInfo.symbol
                        }
                    , direction = extensionRightInfo.direction
                    , expression = right.syntax
                    }
            }
        )
        whitespaceAndComments
        (ParserFast.lazy
            (\() ->
                extendedSubExpressionFollowedByWhitespaceAndComments
                    { afterCommitting = extensionRightInfo.afterCommitting
                    , validateRightPrecedence = extensionRightInfo.validateRightPrecedence
                    }
            )
        )


infixOperatorAndThen :
    { afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
    , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments ExtensionRight)
infixOperatorAndThen extensionRightConstraints =
    let
        toResult : InfixOperatorInfo -> Maybe InfixOperatorInfo
        toResult =
            extensionRightConstraints.validateRightPrecedence

        apRResult : Maybe InfixOperatorInfo
        apRResult =
            toResult precedence1ApR

        appendResult : Maybe InfixOperatorInfo
        appendResult =
            toResult precedence5append

        apLResult : Maybe InfixOperatorInfo
        apLResult =
            toResult precedence1ApL

        composeRResult : Maybe InfixOperatorInfo
        composeRResult =
            toResult precedence9ComposeR

        eqResult : Maybe InfixOperatorInfo
        eqResult =
            toResult precedence4Eq

        mulResult : Maybe InfixOperatorInfo
        mulResult =
            toResult precedence7Mul

        consResult : Maybe InfixOperatorInfo
        consResult =
            toResult precedence5Cons

        addResult : Maybe InfixOperatorInfo
        addResult =
            toResult precedence6Add

        subResult : Maybe InfixOperatorInfo
        subResult =
            toResult precedence6Sub

        ignoreResult : Maybe InfixOperatorInfo
        ignoreResult =
            toResult precedence6Ignore

        andResult : Maybe InfixOperatorInfo
        andResult =
            toResult precedence3And

        keepResult : Maybe InfixOperatorInfo
        keepResult =
            toResult precedence5Keep

        composeLResult : Maybe InfixOperatorInfo
        composeLResult =
            toResult precedence9ComposeL

        neqResult : Maybe InfixOperatorInfo
        neqResult =
            toResult precedence4Neq

        idivResult : Maybe InfixOperatorInfo
        idivResult =
            toResult precedence7Idiv

        fdivResult : Maybe InfixOperatorInfo
        fdivResult =
            toResult precedence7Fdiv

        slashResult : Maybe InfixOperatorInfo
        slashResult =
            toResult precedence7Slash

        orResult : Maybe InfixOperatorInfo
        orResult =
            toResult precedence2Or

        leResult : Maybe InfixOperatorInfo
        leResult =
            toResult precedence4Le

        geResult : Maybe InfixOperatorInfo
        geResult =
            toResult precedence4Ge

        gtResult : Maybe InfixOperatorInfo
        gtResult =
            toResult precedence4Gt

        questionMarkResult : Maybe InfixOperatorInfo
        questionMarkResult =
            toResult precedence8QuestionMark

        ltResult : Maybe InfixOperatorInfo
        ltResult =
            toResult precedence4Lt

        powResult : Maybe InfixOperatorInfo
        powResult =
            toResult precedence8Pow
    in
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ToResultAndThen
        isOperatorSymbolCharAsString
        (\operator ->
            case operator of
                "|>" ->
                    apRResult

                "++" ->
                    appendResult

                "<|" ->
                    apLResult

                ">>" ->
                    composeRResult

                "==" ->
                    eqResult

                "*" ->
                    mulResult

                "::" ->
                    consResult

                "+" ->
                    addResult

                "-" ->
                    subResult

                "|." ->
                    ignoreResult

                "&&" ->
                    andResult

                "|=" ->
                    keepResult

                "<<" ->
                    composeLResult

                "/=" ->
                    neqResult

                "!=" ->
                    neqResult

                "//" ->
                    idivResult

                "/" ->
                    fdivResult

                "</>" ->
                    slashResult

                "||" ->
                    orResult

                "<=" ->
                    leResult

                ">=" ->
                    geResult

                ">" ->
                    gtResult

                "<?>" ->
                    questionMarkResult

                "<" ->
                    ltResult

                "^" ->
                    powResult

                _ ->
                    Nothing
        )
        extensionRightConstraints.afterCommitting


subExpressionMaybeAppliedFollowedByWhitespaceAndComments : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
subExpressionMaybeAppliedFollowedByWhitespaceAndComments =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    literalExpressionOptimisticLayout

                "(" ->
                    tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied

                "[" ->
                    listOrGlslExpressionOptimisticLayout

                "{" ->
                    recordExpressionFollowedByRecordAccessMaybeApplied

                "c" ->
                    caseOfOrUnqualifiedReferenceExpressionMaybeApplied

                "\\" ->
                    expressionLambdaFollowedByWhitespaceAndComments

                "l" ->
                    letOrUnqualifiedReferenceExpressionMaybeApplied

                "i" ->
                    ifOrUnqualifiedReferenceExpressionMaybeApplied

                "." ->
                    recordAccessFunctionExpressionMaybeApplied

                "-" ->
                    negationOperationOptimisticLayout

                "'" ->
                    charLiteralExpressionOptimisticLayout

                _ ->
                    referenceOrNumberExpressionMaybeApplied
        )


negationOperationOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
negationOperationOptimisticLayout =
    expressionNegation |> followedByOptimisticLayout


charLiteralExpressionOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
charLiteralExpressionOptimisticLayout =
    expressionChar |> followedByOptimisticLayout


literalExpressionOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
literalExpressionOptimisticLayout =
    expressionString |> followedByOptimisticLayout


listOrGlslExpressionOptimisticLayout : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
listOrGlslExpressionOptimisticLayout =
    expressionArray |> followedByOptimisticLayout


followedByOptimisticLayout : Parser (WithComments a) -> Parser (WithComments a)
followedByOptimisticLayout parser =
    ParserFast.map2
        (\result commentsAfter ->
            { comments = result.comments |> ropePrependTo commentsAfter
            , syntax = result.syntax
            }
        )
        parser
        whitespaceAndComments


recordAccessFunctionExpressionMaybeApplied : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
recordAccessFunctionExpressionMaybeApplied =
    expressionRecordAccessFunction |> followedByMultiArgumentApplication


recordExpressionFollowedByRecordAccessMaybeApplied : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
recordExpressionFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if record access
    expressionRecordOrRecordUpdateFollowedByRecordAccess
        |> followedByMultiArgumentApplication


tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if not parenthesized
    expressionStartingWithParensOpeningIfNecessaryFollowedByRecordAccess
        |> followedByMultiArgumentApplication


caseOfOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
caseOfOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        expressionWhenIsFollowedByOptimisticLayout
        (expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


letOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
letOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        letExpressionFollowedByOptimisticLayout
        (expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


ifOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
ifOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        expressionIfThenElseFollowedByOptimisticLayout
        (expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


referenceOrNumberExpressionMaybeApplied : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
referenceOrNumberExpressionMaybeApplied =
    ParserFast.oneOf3
        (expressionQualifiedOrVariantOrRecordConstructorReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (expressionNumber |> followedByOptimisticLayout)


followedByMultiArgumentApplication : Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression)) -> Parser (WithComments (ElmSyntax.Node ElmSyntax.Expression))
followedByMultiArgumentApplication appliedExpressionParser =
    ParserFast.map3
        (\leftExpressionResult commentsBeforeExtension maybeArgumentsReverse ->
            { comments =
                leftExpressionResult.comments
                    |> ropePrependTo commentsBeforeExtension
                    |> ropePrependTo maybeArgumentsReverse.comments
            , syntax =
                case maybeArgumentsReverse.syntax of
                    [] ->
                        leftExpressionResult.syntax

                    lastArgumentNode :: beforeLastArgumentNodeReverse ->
                        let
                            arguments :
                                { head : ElmSyntax.Node ElmSyntax.Expression
                                , tail : List (ElmSyntax.Node ElmSyntax.Expression)
                                }
                            arguments =
                                listFilledReverse lastArgumentNode beforeLastArgumentNodeReverse
                        in
                        { range = { start = leftExpressionResult.syntax.range.start, end = lastArgumentNode.range.end }
                        , value =
                            ElmSyntax.ExpressionCall
                                { called = leftExpressionResult.syntax
                                , argument0 = arguments.head
                                , argument1Up = arguments.tail
                                }
                        }
            }
        )
        appliedExpressionParser
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfter ->
                        { comments = arg.comments |> ropePrependTo commentsAfter
                        , syntax = arg.syntax
                        }
                    )
                    subExpression
                    whitespaceAndComments
                )
            )
        )


listFilledReverse : a -> List a -> { head : a, tail : List a }
listFilledReverse head tail =
    case (head :: tail) |> List.reverse of
        last :: beforeLastReverse ->
            { head = last, tail = beforeLastReverse }

        [] ->
            -- only if List.reverse has a bug
            { head = head, tail = [] }


applyExtensionRight : ExtensionRight -> ElmSyntax.Node ElmSyntax.Expression -> ElmSyntax.Node ElmSyntax.Expression
applyExtensionRight (ExtendRightByOperation operation) leftNode =
    { range = { start = leftNode.range.start, end = operation.expression.range.end }
    , value =
        ElmSyntax.ExpressionInfixOperation
            { operator = operation.symbol
            , left = leftNode
            , right = operation.expression
            }
    }


type alias InfixOperatorInfo =
    { leftPrecedence : Int
    , symbol : String
    , extensionRightParser : Parser (WithComments ExtensionRight)
    }


infixLeft : Int -> String -> InfixOperatorInfo
infixLeft leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , direction = ElmSyntax.Left
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence > leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
            }
    }


infixRight : Int -> String -> InfixOperatorInfo
infixRight leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , direction = ElmSyntax.Right
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence >= leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
            }
    }


infixNonAssociative : Int -> String -> InfixOperatorInfo
infixNonAssociative leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting =
                \rightInfo ->
                    if rightInfo.leftPrecedence == leftPrecedence then
                        ParserFast.problem

                    else
                        rightInfo.extensionRightParser
            , direction = ElmSyntax.Non
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence >= leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
            }
    }


type ExtensionRight
    = ExtendRightByOperation
        { symbol : ElmSyntax.Node String
        , direction : ElmSyntax.InfixDirection
        , expression : ElmSyntax.Node ElmSyntax.Expression
        }


{-| [`Parser`](#Parser) for an [`ElmSyntax.Pattern`](ElmSyntax#Pattern)
-}
pattern : Parser { comments : Comments, syntax : ElmSyntax.Node ElmSyntax.Pattern }
pattern =
    ParserFast.map2
        (\leftMaybeConsed maybeAsExtension ->
            case maybeAsExtension of
                Nothing ->
                    leftMaybeConsed

                Just asExtension ->
                    { comments =
                        leftMaybeConsed.comments
                            |> ropePrependTo asExtension.comments
                    , syntax =
                        { range =
                            { start = leftMaybeConsed.syntax.range.start
                            , end = asExtension.syntax.range.end
                            }
                        , value =
                            ElmSyntax.PatternAs
                                { pattern = leftMaybeConsed.syntax
                                , asKeywordRange =
                                    { start = asExtension.asKeywordStartLocation
                                    , end =
                                        asExtension.asKeywordStartLocation
                                            |> locationAddColumn 2
                                    }
                                , variable = asExtension.syntax
                                }
                        }
                    }
        )
        (ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
            (ParserFast.map2
                (\startPatternResult commentsAfter ->
                    { comments = startPatternResult.comments |> ropePrependTo commentsAfter
                    , syntax = startPatternResult.syntax
                    }
                )
                (ParserFast.lazy (\() -> composablePattern))
                whitespaceAndComments
            )
            (ParserFast.symbolFollowedBy "::"
                (ParserFast.map3WithStartLocation
                    (\consKeySymbolEndLocation commentsAfterCons patternResult commentsAfterTailSubPattern ->
                        { comments =
                            commentsAfterCons
                                |> ropePrependTo patternResult.comments
                                |> ropePrependTo commentsAfterTailSubPattern
                        , syntax = patternResult.syntax
                        , consKeySymbolEndLocation = consKeySymbolEndLocation
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.lazy (\() -> composablePattern))
                    whitespaceAndComments
                )
            )
            (\consed fromCons ->
                { comments = consed.comments |> ropePrependTo fromCons.comments
                , syntax =
                    ElmSyntax.nodeCombine
                        (\head tail ->
                            ElmSyntax.PatternListCons
                                { head = head
                                , consKeySymbolRange =
                                    { start =
                                        fromCons.consKeySymbolEndLocation
                                            |> locationAddColumn -2
                                    , end = fromCons.consKeySymbolEndLocation
                                    }
                                , tail = tail
                                }
                        )
                        consed.syntax
                        fromCons.syntax
                }
            )
        )
        (ParserFast.orSucceed
            (ParserFast.map2WithStartLocation
                (\asKeywordStartLocation commentsAfterAs name ->
                    Just
                        { comments = commentsAfterAs
                        , syntax = name
                        , asKeywordStartLocation = asKeywordStartLocation
                        }
                )
                (ParserFast.keywordFollowedBy "as" whitespaceAndComments)
                nameLowercaseNodeUnderscoreSuffixingKeywords
            )
            Nothing
        )


patternUnitOrParenthesizedOrTupleOrTriple : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
patternUnitOrParenthesizedOrTupleOrTriple =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2WithRange
            (\range commentsBeforeHead contentResult ->
                { comments =
                    commentsBeforeHead
                        |> ropePrependTo contentResult.comments
                , syntax =
                    { range = { start = { line = range.start.line, column = range.start.column - 1 }, end = range.end }
                    , value = contentResult.syntax
                    }
                }
            )
            whitespaceAndComments
            -- yes, (  ) is a valid pattern but not a valid type or expression
            (ParserFast.oneOf2
                (ParserFast.symbol ")" { comments = ropeEmpty, syntax = ElmSyntax.PatternUnit })
                (ParserFast.map3
                    (\headResult commentsAfterHead tailResult ->
                        { comments =
                            headResult.comments
                                |> ropePrependTo commentsAfterHead
                                |> ropePrependTo tailResult.comments
                        , syntax =
                            case tailResult.syntax of
                                Nothing ->
                                    ElmSyntax.PatternParenthesized headResult.syntax

                                Just secondAndMaybeThirdPart ->
                                    case secondAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            ElmSyntax.PatternTuple
                                                { part0 = headResult.syntax
                                                , part1 = secondAndMaybeThirdPart.secondPart
                                                }

                                        Just thirdPart ->
                                            ElmSyntax.PatternTriple
                                                { part0 = headResult.syntax
                                                , part1 = secondAndMaybeThirdPart.secondPart
                                                , part2 = thirdPart
                                                }
                        }
                    )
                    pattern
                    whitespaceAndComments
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map4
                                (\commentsBefore secondPart commentsAfter maybeThirdPart ->
                                    { comments =
                                        commentsBefore
                                            |> ropePrependTo secondPart.comments
                                            |> ropePrependTo commentsAfter
                                            |> ropePrependTo maybeThirdPart.comments
                                    , syntax = Just { maybeThirdPart = maybeThirdPart.syntax, secondPart = secondPart.syntax }
                                    }
                                )
                                whitespaceAndComments
                                pattern
                                whitespaceAndComments
                                (ParserFast.oneOf2
                                    (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                                    (ParserFast.symbolFollowedBy ","
                                        (ParserFast.map3
                                            (\commentsBefore thirdPart commentsAfter ->
                                                { comments =
                                                    commentsBefore
                                                        |> ropePrependTo thirdPart.comments
                                                        |> ropePrependTo commentsAfter
                                                , syntax = Just thirdPart.syntax
                                                }
                                            )
                                            whitespaceAndComments
                                            pattern
                                            whitespaceAndComments
                                            |> ParserFast.followedBySymbol ")"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


varPattern : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
varPattern =
    nameLowercaseMapWithRange
        (\range var ->
            { comments = ropeEmpty
            , syntax = { range = range, value = ElmSyntax.PatternVariable var }
            }
        )


numberPart : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
numberPart =
    ParserFast.integerDecimalOrHexadecimalMapWithRange
        (\range n ->
            { comments = ropeEmpty
            , syntax =
                { range = range
                , value =
                    ElmSyntax.PatternInt
                        { value = n, base = ElmSyntax.IntBase10 }
                }
            }
        )
        (\range n ->
            { comments = ropeEmpty
            , syntax =
                { range = range
                , value =
                    ElmSyntax.PatternInt
                        { value = n, base = ElmSyntax.IntBase16 }
                }
            }
        )


charPattern : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
charPattern =
    characterLiteralMapWithRange
        (\range char ->
            { comments = ropeEmpty, syntax = { range = range, value = ElmSyntax.PatternChar char } }
        )


listPattern : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
listPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = { range = range, value = patternListEmpty }
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> ropePrependTo elements.comments
                    , syntax = { range = range, value = ElmSyntax.PatternListExact elements.syntax }
                    }
        )
        (ParserFast.symbolFollowedBy "[" whitespaceAndComments)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map4
                (\commentsBeforeHead head commentsAfterHead tail ->
                    Just
                        { comments =
                            commentsBeforeHead
                                |> ropePrependTo head.comments
                                |> ropePrependTo tail.comments
                                |> ropePrependTo commentsAfterHead
                        , syntax = head.syntax :: tail.syntax
                        }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                    ropeEmpty
                )
                pattern
                whitespaceAndComments
                (manyWithComments
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore commentsWithExtraComma v commentsAfter ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo commentsWithExtraComma
                                        |> ropePrependTo v.comments
                                        |> ropePrependTo commentsAfter
                                , syntax = v.syntax
                                }
                            )
                            whitespaceAndComments
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                                ropeEmpty
                            )
                            pattern
                            whitespaceAndComments
                        )
                    )
                )
                |> ParserFast.followedBySymbol "]"
            )
        )


patternListEmpty : ElmSyntax.Pattern
patternListEmpty =
    ElmSyntax.PatternListExact []


composablePattern : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
composablePattern =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithConsumeValues
        allPattern
        patternUnitOrParenthesizedOrTupleOrTriple
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


patternNotSpaceSeparated : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
patternNotSpaceSeparated =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithoutConsumeArgs
        allPattern
        patternUnitOrParenthesizedOrTupleOrTriple
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


allPattern : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
allPattern =
    ParserFast.symbolWithEndLocation "_"
        (\endLocation ->
            { comments = ropeEmpty
            , syntax =
                { range =
                    { start = { line = endLocation.line, column = endLocation.column - 1 }
                    , end = endLocation
                    }
                , value = ElmSyntax.PatternIgnored
                }
            }
        )


stringPattern : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
stringPattern =
    singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = ropeEmpty
            , syntax = { range = range, value = ElmSyntax.PatternString string }
            }
        )


qualifiedPatternWithConsumeValues : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
qualifiedPatternWithConsumeValues =
    ParserFast.map3
        (\referenceNode afterStartName valuesReverse ->
            let
                range : TextGrid.Range
                range =
                    case valuesReverse.syntax of
                        [] ->
                            referenceNode.range

                        lastArgumentNode :: _ ->
                            { start = referenceNode.range.start, end = lastArgumentNode.range.end }
            in
            { comments = afterStartName |> ropePrependTo valuesReverse.comments
            , syntax =
                { range = range
                , value =
                    ElmSyntax.PatternVariant
                        { qualification = referenceNode.value.qualification
                        , name = referenceNode.value.name
                        , values = valuesReverse.syntax
                        }
                }
            }
        )
        (ParserFast.map2WithRange
            (\range firstName after ->
                { range = range
                , value =
                    case after of
                        Nothing ->
                            { qualification = [], name = firstName }

                        Just ( qualificationAfter, unqualified ) ->
                            { qualification = firstName :: qualificationAfter, name = unqualified }
                }
            )
            nameUppercase
            maybeDotNamesUppercaseTuple
        )
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\value commentsAfter ->
                        { comments = value.comments |> ropePrependTo commentsAfter
                        , syntax = value.syntax
                        }
                    )
                    patternNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = ropeEmpty
            , syntax =
                { range = range
                , value =
                    ElmSyntax.PatternVariant
                        (case after of
                            Nothing ->
                                { qualification = [], name = firstName, values = [] }

                            Just ( qualificationAfter, unqualified ) ->
                                { qualification = firstName :: qualificationAfter
                                , name = unqualified
                                , values = []
                                }
                        )
                }
            }
        )
        nameUppercase
        maybeDotNamesUppercaseTuple


recordPattern : Parser (WithComments (ElmSyntax.Node ElmSyntax.Pattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements elements ->
            { comments = commentsBeforeElements |> ropePrependTo elements.comments
            , syntax =
                { range = range, value = ElmSyntax.PatternRecord elements.syntax }
            }
        )
        (ParserFast.symbolFollowedBy "{" whitespaceAndComments)
        (ParserFast.oneOf2
            (ParserFast.symbol "}" { comments = ropeEmpty, syntax = [] })
            (ParserFast.map4
                (\commentsBeforeHead headName commentsAfterHeadName tail ->
                    { comments =
                        commentsBeforeHead
                            |> ropePrependTo commentsAfterHeadName
                            |> ropePrependTo tail.comments
                    , syntax = headName :: tail.syntax
                    }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                    ropeEmpty
                )
                nameLowercaseNodeUnderscoreSuffixingKeywords
                whitespaceAndComments
                (manyWithComments
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBeforeName commentsWithExtraComma name commentsAfterName ->
                                { comments =
                                    commentsBeforeName
                                        |> ropePrependTo commentsWithExtraComma
                                        |> ropePrependTo commentsAfterName
                                , syntax = name
                                }
                            )
                            whitespaceAndComments
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                                ropeEmpty
                            )
                            nameLowercaseNodeUnderscoreSuffixingKeywords
                            whitespaceAndComments
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
        )


isNotReserved : String -> Bool
isNotReserved name =
    case name of
        "module" ->
            False

        "exposing" ->
            False

        "import" ->
            False

        "as" ->
            False

        "if" ->
            False

        "then" ->
            False

        "else" ->
            False

        "let" ->
            False

        "in" ->
            False

        "when" ->
            False

        "is" ->
            False

        "case" ->
            False

        "of" ->
            False

        "port" ->
            False

        --"infixr"
        --"infixl"
        "type" ->
            False

        -- "infix" Apparently this is not a reserved keyword
        -- "alias" Apparently this is not a reserved keyword
        "where" ->
            False

        _ ->
            True


ifKeywordUnderscoreSuffix : String -> String
ifKeywordUnderscoreSuffix name =
    case name of
        "module" ->
            "module_"

        "exposing" ->
            "exposing_"

        "import" ->
            "import_"

        "as" ->
            "as_"

        "if" ->
            "if_"

        "then" ->
            "then_"

        "else" ->
            "else_"

        "let" ->
            "let_"

        "in" ->
            "in_"

        "case" ->
            "case_"

        "when" ->
            "when_"

        "is" ->
            "is_"

        "of" ->
            "of_"

        "port" ->
            "port_"

        "type" ->
            "type_"

        "where" ->
            "where_"

        _ ->
            name


escapedCharValueMap : (Char -> res) -> Parser res
escapedCharValueMap charToRes =
    ParserFast.oneOf7
        (ParserFast.symbol "'" (charToRes '\''))
        (ParserFast.symbol "\"" (charToRes '"'))
        (ParserFast.symbol "n" (charToRes '\n'))
        (ParserFast.symbol "t" (charToRes '\t'))
        (ParserFast.symbol "r" (charToRes '\u{000D}'))
        (ParserFast.symbol "\\" (charToRes '\\'))
        (ParserFast.symbolFollowedBy "u{"
            (ParserFast.ifFollowedByWhileMapWithoutLinebreak
                (\hex ->
                    charToRes (Char.fromCode (hexStringToInt hex))
                )
                Char.isHexDigit
                Char.isHexDigit
                |> ParserFast.followedBySymbol "}"
            )
        )


hexStringToInt : String -> Int
hexStringToInt string =
    String.foldr
        (\c soFar ->
            { exponent = soFar.exponent + 1
            , result = soFar.result + 16 ^ soFar.exponent * charToHex c
            }
        )
        { exponent = 0, result = 0 }
        string
        |> .result


charToHex : Char -> Int
charToHex c =
    case c of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        'f' ->
            15

        'A' ->
            10

        'B' ->
            11

        'C' ->
            12

        'D' ->
            13

        'E' ->
            14

        -- 'F'
        _ ->
            15


characterLiteralMapWithRange : (TextGrid.Range -> Char -> res) -> Parser res
characterLiteralMapWithRange rangeAndCharToRes =
    ParserFast.symbolFollowedBy "'"
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { line = startRow, column = startColumn - 1 }
                    , end = { line = endRow, column = endColumn + 1 }
                    }
                    char
            )
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap identity))
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { line = startRow, column = startColumn - 1 }
                    , end = { line = endRow, column = endColumn + 1 }
                    }
                    char
            )
            ParserFast.anyChar
            |> ParserFast.followedBySymbol "'"
        )


singleOrTripleQuotedStringLiteralMapWithRange :
    (TextGrid.Range
     -> { content : String, quotingStyle : ElmSyntax.StringQuotingStyle }
     -> res
    )
    -> Parser res
singleOrTripleQuotedStringLiteralMapWithRange rangeAndStringToRes =
    ParserFast.symbolFollowedBy "\""
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { line = startRow, column = startColumn - 1 }
                    , end = { line = endRow, column = endColumn }
                    }
                    { content = string, quotingStyle = ElmSyntax.StringTripleQuoted }
            )
            (ParserFast.symbolFollowedBy "\"\""
                tripleQuotedStringLiteralOfterTripleDoubleQuote
            )
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { line = startRow, column = startColumn - 1 }
                    , end = { line = endRow, column = endColumn }
                    }
                    { content = string, quotingStyle = ElmSyntax.StringSingleQuoted }
            )
            singleQuotedStringLiteralAfterDoubleQuote
        )


singleQuotedStringLiteralAfterDoubleQuote : Parser String
singleQuotedStringLiteralAfterDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"" ())
        (ParserFast.oneOf2
            (ParserFast.whileAtLeast1WithoutLinebreak
                (\c ->
                    case c of
                        '"' ->
                            False

                        '\\' ->
                            False

                        _ ->
                            not (Unicode.isUtf16Surrogate c)
                )
            )
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


tripleQuotedStringLiteralOfterTripleDoubleQuote : Parser String
tripleQuotedStringLiteralOfterTripleDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"\"\"" ())
        (ParserFast.oneOf3
            (ParserFast.symbol "\"" "\"")
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
            (ParserFast.while
                (\c ->
                    case c of
                        '"' ->
                            False

                        '\\' ->
                            False

                        _ ->
                            not (Unicode.isUtf16Surrogate c)
                )
            )
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


{-| [`Parser`](#Parser) for a name used for
record field names and unqualified function/value references
-}
nameLowercase : Parser String
nameLowercase =
    ParserFast.ifFollowedByWhileValidateWithoutLinebreak
        Unicode.unicodeIsLowerFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


nameLowercaseUnderscoreSuffixingKeywords : Parser String
nameLowercaseUnderscoreSuffixingKeywords =
    ParserFast.ifFollowedByWhileMapWithoutLinebreak
        ifKeywordUnderscoreSuffix
        Unicode.unicodeIsLowerFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast


nameLowercaseNode : Parser (ElmSyntax.Node String)
nameLowercaseNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak (\range value -> { range = range, value = value })
        Unicode.unicodeIsLowerFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


nameLowercaseNodeUnderscoreSuffixingKeywords : Parser (ElmSyntax.Node String)
nameLowercaseNodeUnderscoreSuffixingKeywords =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak
        (\range name ->
            { range = range, value = name |> ifKeywordUnderscoreSuffix }
        )
        Unicode.unicodeIsLowerFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast


nameLowercaseMapWithRange : (TextGrid.Range -> String -> res) -> Parser res
nameLowercaseMapWithRange rangeAndNameToResult =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak
        rangeAndNameToResult
        Unicode.unicodeIsLowerFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameNotInfixNode : Parser (ElmSyntax.Node String)
functionNameNotInfixNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak (\range value -> { range = range, value = value })
        Unicode.unicodeIsLowerFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast
        (\name ->
            case name of
                "infix" ->
                    False

                nameNotInfix ->
                    nameNotInfix |> isNotReserved
        )


{-| [`Parser`](#Parser) for a name used for
type names, variant names, record type alias constructor function names and module names
-}
nameUppercase : Parser String
nameUppercase =
    ParserFast.ifFollowedByWhileWithoutLinebreak
        Unicode.unicodeIsUpperFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast


nameUppercaseMapWithRange : (TextGrid.Range -> String -> res) -> Parser res
nameUppercaseMapWithRange rangeAndNameToRes =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak rangeAndNameToRes
        Unicode.unicodeIsUpperFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast


nameUppercaseNode : Parser (ElmSyntax.Node String)
nameUppercaseNode =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak (\range value -> { range = range, value = value })
        Unicode.unicodeIsUpperFast
        Unicode.unicodeIsAlphaNumOrUnderscoreFast


isAllowedOperatorToken : String -> Bool
isAllowedOperatorToken operatorCandidateToValidate =
    case operatorCandidateToValidate of
        "==" ->
            True

        "!=" ->
            True

        "::" ->
            True

        "++" ->
            True

        "+" ->
            True

        "*" ->
            True

        "<|" ->
            True

        "|>" ->
            True

        "||" ->
            True

        "<=" ->
            True

        ">=" ->
            True

        "|=" ->
            True

        "|." ->
            True

        "//" ->
            True

        "</>" ->
            True

        "<?>" ->
            True

        "^" ->
            True

        "<<" ->
            True

        ">>" ->
            True

        "<" ->
            True

        ">" ->
            True

        "/" ->
            True

        "&&" ->
            True

        "-" ->
            True

        _ ->
            False


isOperatorSymbolCharAsString : String -> Bool
isOperatorSymbolCharAsString c =
    case c of
        "|" ->
            True

        "+" ->
            True

        "<" ->
            True

        ">" ->
            True

        "=" ->
            True

        "*" ->
            True

        ":" ->
            True

        "-" ->
            True

        "/" ->
            True

        "&" ->
            True

        "." ->
            True

        "?" ->
            True

        "^" ->
            True

        "!" ->
            True

        _ ->
            False


{-| [`Parser`](#Parser) for a `--...` comment
-}
singleLineComment : Parser (ElmSyntax.Node String)
singleLineComment =
    ParserFast.symbolFollowedBy "--"
        (ParserFast.whileMapWithRange
            (\c ->
                case c of
                    '\u{000D}' ->
                        False

                    '\n' ->
                        False

                    _ ->
                        not (Unicode.isUtf16Surrogate c)
            )
            (\range content ->
                { range =
                    { start = { line = range.start.line, column = range.start.column - 2 }
                    , end = { line = range.start.line, column = range.end.column }
                    }
                , value = "--" ++ content
                }
            )
        )


{-| [`Parser`](#Parser) for a `{-...-}` comment,
also verifying that it itself isn't a documentation comment
-}
multiLineComment : Parser (ElmSyntax.Node String)
multiLineComment =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice (offset + 2) (offset + 3) source of
                "|" ->
                    ParserFast.problem

                _ ->
                    multiLineCommentNoCheck
        )


multiLineCommentNoCheck : Parser (ElmSyntax.Node String)
multiLineCommentNoCheck =
    ParserFast.nestableMultiCommentMapWithRange (\range value -> { range = range, value = value })
        ( '{', "-" )
        ( '-', "}" )


{-| [`Parser`](#Parser) for the space between syntax tokens
which can contain spaces, linebreaks, [`multiLineComment`](#multiLineComment)s
and [`singleLineComment`](#singleLineComment)s
-}
whitespaceAndComments : Parser Comments
whitespaceAndComments =
    ParserFast.skipWhileWhitespaceBacktrackableFollowedBy
        -- whitespace can't be followed by more whitespace
        --
        -- since comments are comparatively rare
        -- but expensive to check for, we allow shortcutting
        (ParserFast.offsetSourceAndThenOrSucceed
            (\offset source ->
                case source |> String.slice offset (offset + 2) of
                    "--" ->
                        -- this will always succeed from here, so no need to fall back to empty
                        Just fromSingleLineCommentNode

                    "{-" ->
                        Just fromMultilineCommentNodeOrEmptyOnProblem

                    _ ->
                        Nothing
            )
            ropeEmpty
        )


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    ParserFast.map2OrSucceed
        (\comment commentsAfter ->
            ropeOne comment |> ropeFilledPrependTo commentsAfter
        )
        (multiLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop
        ropeEmpty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    ParserFast.map2
        (\content commentsAfter ->
            ropeOne content |> ropeFilledPrependTo commentsAfter
        )
        (singleLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop


whitespaceAndCommentsOrEmptyLoop : Parser Comments
whitespaceAndCommentsOrEmptyLoop =
    ParserFast.loopWhileSucceeds
        (ParserFast.oneOf2
            singleLineComment
            multiLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        ropeEmpty
        (\right soFar -> soFar |> ropePrependToFilled (ropeOne right))
        identity


positivelyIndentedFollowedBy : Parser a -> Parser a
positivelyIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if
                (column > 1)
                    && (indent |> List.all (\nestedIndent -> column /= nestedIndent))
            then
                nextParser

            else
                ParserFast.problem
        )


whitespaceAndCommentsEndsTopIndentedFollowedByComments : Parser Comments -> Parser Comments
whitespaceAndCommentsEndsTopIndentedFollowedByComments nextParser =
    ParserFast.map2
        (\commentsBefore afterComments ->
            commentsBefore |> ropePrependTo afterComments
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndentedFollowedByWithComments : Parser (WithComments syntax) -> Parser (WithComments syntax)
whitespaceAndCommentsEndsTopIndentedFollowedByWithComments nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore |> ropePrependTo after.comments
            , syntax = after.syntax
            }
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndentedFollowedBy : Parser syntax -> Parser (WithComments syntax)
whitespaceAndCommentsEndsTopIndentedFollowedBy nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore, syntax = after }
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndented : Parser Comments
whitespaceAndCommentsEndsTopIndented =
    whitespaceAndComments |> endsTopIndented


endsTopIndented : Parser a -> Parser a
endsTopIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent ->
            case indent of
                [] ->
                    column == 1

                highestIndent :: _ ->
                    column - highestIndent == 0
        )
        parser


topIndentedFollowedBy : Parser a -> Parser a
topIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            case indent of
                [] ->
                    if column == 1 then
                        nextParser

                    else
                        ParserFast.problem

                highestIndent :: _ ->
                    if column - highestIndent == 0 then
                        nextParser

                    else
                        ParserFast.problem
        )


type alias WithComments res =
    { comments : Comments, syntax : res }


{-| A bag of comment nodes.
Each comment string contains the `{-`, `-}` or `--`.

Access with [`commentsToList`](#commentsToList)

-}
type alias Comments =
    Maybe (RopeFilled (ElmSyntax.Node String))


{-| Extract a list of comment nodes from parse result [`Comments`](#Comments)
-}
commentsToList : Comments -> List (ElmSyntax.Node String)
commentsToList comments =
    ropeToList comments


untilWithComments : ParserFast.Parser () -> ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
untilWithComments end element =
    ParserFast.loopUntil
        end
        element
        ( ropeEmpty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> ropePrependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = List.reverse itemsSoFar
            }
        )


manyWithComments : ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
manyWithComments p =
    ParserFast.loopWhileSucceeds p
        ( ropeEmpty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> ropePrependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = List.reverse itemsSoFar
            }
        )


{-| Same as `manyWithComments` except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithCommentsReverse : ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
manyWithCommentsReverse p =
    ParserFast.loopWhileSucceeds p
        { comments = ropeEmpty, syntax = [] }
        (\pResult soFar ->
            { comments = soFar.comments |> ropePrependTo pResult.comments
            , syntax = pResult.syntax :: soFar.syntax
            }
        )
        (\result -> result)


type alias Rope a =
    Maybe (RopeFilled a)


{-| Constantly appending lists of comments when combining parse can get expensive,
so we summarize everything in this temporary structure
and only convert to a list when we're done.

Inspired by [miniBill/elm-rope](https://elm-lang.org/packages/miniBill/elm-rope/latest/)

-}
type RopeFilled a
    = RopeLeaf a ()
    | RopeBranch2 (RopeFilled a) (RopeFilled a)


ropeEmpty : Rope a_
ropeEmpty =
    Nothing


ropeOne : a -> RopeFilled a
ropeOne onlyElement =
    RopeLeaf onlyElement ()


ropeFilledPrependTo : Rope a -> RopeFilled a -> Rope a
ropeFilledPrependTo right leftLikelyFilled =
    Just
        (case right of
            Nothing ->
                leftLikelyFilled

            Just rightLikelyFilled ->
                RopeBranch2 leftLikelyFilled rightLikelyFilled
        )


ropePrependToFilled : RopeFilled a -> Rope a -> Rope a
ropePrependToFilled rightLikelyFilled left =
    Just
        (case left of
            Nothing ->
                rightLikelyFilled

            Just leftLikelyFilled ->
                RopeBranch2 leftLikelyFilled rightLikelyFilled
        )


ropePrependTo : Rope a -> Rope a -> Rope a
ropePrependTo right left =
    case left of
        Nothing ->
            right

        Just leftLikelyFilled ->
            case right of
                Nothing ->
                    left

                Just rightLikelyFilled ->
                    Just (RopeBranch2 leftLikelyFilled rightLikelyFilled)


ropeToList : Rope a -> List a
ropeToList rope =
    case rope of
        Nothing ->
            []

        Just ropeLikelyFilled ->
            ropeLikelyFilledToListInto [] ropeLikelyFilled


ropeLikelyFilledToListInto : List a -> RopeFilled a -> List a
ropeLikelyFilledToListInto initialAcc ropeLikelyFilled =
    -- IGNORE TCO
    case ropeLikelyFilled of
        RopeLeaf onlyElement () ->
            onlyElement :: initialAcc

        RopeBranch2 left right ->
            ropeLikelyFilledToListInto
                (ropeLikelyFilledToListInto
                    initialAcc
                    right
                )
                left
