module ElmSyntax exposing
    ( File, ModuleName, Import
    , DefaultModuleData, EffectModuleData, Module(..)
    , Exposing(..), TopLevelExpose(..)
    , Declaration(..), ValueOrFunctionDeclarationInfo, ChoiceTypeDeclarationInfo, TypeAliasDeclarationInfo, InfixDeclarationInfo, InfixDirection(..)
    , Pattern(..), Expression(..), LetDeclaration(..), StringQuotingStyle(..), TypeAnnotation(..)
    , Location, locationCompare, Range, rangeEmpty, Node, nodeCombine, nodeMap, nodeRange, nodeValue
    )

{-| Elm syntax tree

@docs File, ModuleName, Import
@docs DefaultModuleData, EffectModuleData, Module
@docs Exposing, TopLevelExpose
@docs Declaration, ValueOrFunctionDeclarationInfo, ChoiceTypeDeclarationInfo, TypeAliasDeclarationInfo, InfixDeclarationInfo, InfixDirection
@docs Pattern, Expression, LetDeclaration, StringQuotingStyle, TypeAnnotation
@docs Location, locationCompare, Range, rangeEmpty, Node, nodeCombine, nodeMap, nodeRange, nodeValue

-}


{-| module header. For example:

    module Html.Attributes exposing (style)

TODO rename to ModuleHeader

-}
type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


{-| Used for imports, module names, and for qualification.
For example:

    module ElmSyntax ...

    import Foo.Bar ...

    import ... as Something

    My.Module.something

    My.Module.SomeType


TODO inline as String

-}
type alias ModuleName =
    List String


{-| Data for a default default

TODO inline

-}
type alias DefaultModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    }


{-| Data for an effect module

TODO inline

-}
type alias EffectModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    , command : Maybe (Node String)
    , subscription : Maybe (Node String)
    }


{-| Type annotation for a file.

TODO rename to module

-}
type alias File =
    { moduleDefinition : Node Module
    , imports : List (Node Import)
    , declarations : List (Node Declaration)
    , comments : List (Node String)
    }


{-| exposing declaration for both imports and module headers.
For example:

    exposing (Foo(..))
    exposing (..)

-}
type Exposing
    = ExposingAll Range
    | ExposingExplicit (List (Node TopLevelExpose))


{-| An exposed entity
-}
type TopLevelExpose
    = ExposeOperator String
    | ExposeVariable String
    | ExposeTypeName String
    | ExposeChoiceType
        { name : String
        , openRange : Range
        }


{-| For example:

    import Html.Attributes as HA exposing (style)

-}
type alias Import =
    { moduleName : Node ModuleName
    , moduleAlias : Maybe (Node ModuleName)
    , exposingList : Maybe (Node Exposing)
    }


{-| A module-level declaration. Can be one of the following:

  - Function/value declaration: `add x y = x + y`
  - Custom type declaration: `type Color = Blue | Red`
  - Type alias declaration: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Infix declaration. You will probably not need this, while only core packages can define these.

TODO prefix `Declaration-` instead of suffix.

-}
type Declaration
    = ValueOrFunctionDeclaration ValueOrFunctionDeclarationInfo
    | TypeAliasDeclaration TypeAliasDeclarationInfo
    | ChoiceTypeDeclaration ChoiceTypeDeclarationInfo
    | PortDeclaration
        { name : Node String
        , typeAnnotation : Node TypeAnnotation
        }
    | InfixDeclaration InfixDeclarationInfo


{-| custom type. For example:

    {-| This is a color
    -}
    type Color
        = Blue
        | Red

TODO inline

-}
type alias ChoiceTypeDeclarationInfo =
    { documentation : Maybe (Node String)
    , name : Node String
    , parameters : List (Node String)
    , -- TODO split into variant0 and variant1Up
      variants :
        List
            -- TODO remove Node wrapping
            (Node
                { name : Node String
                , values : List (Node TypeAnnotation)
                }
            )
    }


{-| For example:

    {-| This is a person
    -}
    type alias Person =
        { name : String
        , age : Int
        }

TODO inline

-}
type alias TypeAliasDeclarationInfo =
    { documentation : Maybe (Node String)
    , name : Node String
    , -- TODO rename to parameters
      generics : List (Node String)
    , typeAnnotation : Node TypeAnnotation
    }


{-| Type annotation for a infix definition

TODO inline

-}
type alias InfixDeclarationInfo =
    { direction : Node InfixDirection
    , precedence : Node Int
    , operator : Node String
    , function : Node String
    }


{-| Infix operator associativity
-}
type InfixDirection
    = Left
    | Right
    | Non


{-| value/full function declaration

TODO inline

-}
type alias ValueOrFunctionDeclarationInfo =
    { documentation : Maybe (Node String)
    , signature :
        Maybe
            -- TODO do not wrap in Node
            (Node
                { -- TODO only store name range
                  name : Node String
                , typeAnnotation : Node TypeAnnotation
                }
            )
    , -- TODO remove Node and inline
      declaration :
        Node
            { name : Node String
            , parameters : List (Node Pattern)
            , expression : Node Expression
            }
    }


{-| A value or function:

  - `ExpressionUnit`: `()`
  - `ExpressionInteger`: `-42`
  - `ExpressionHex`: `0x1F`
  - `ExpressionFloat`: `42.0`
  - `ExpressionChar`: `'a'`
  - `ExpressionString`: `"text"`
  - `ExpressionOperatorFunction`: `(+)`
  - `ExpressionNegation`: `-a`
  - `ExpressionParenthesized`: `(a)`
  - `ExpressionCall`: `add a b`
  - `ExpressionInfixOperation`: `a + b`
  - `ExpressionReference`: `add` or `Basics.True` or `portCmd`
  - `ExpressionIfThenElse`: `if a then b else c`
  - `ExpressionLetIn`: `let a = 4 in a`
  - `ExpressionCaseOf`: `case a of` followed by pattern matches
  - `ExpressionLambda`: `(\a -> a)`
  - `ExpressionRecord`: `{ name = "text" }`
  - `ExpressionList`: `[ x, y ]`
  - `ExpressionRecordAccess`: `a.name`
  - `ExpressionRecordAccessFunction`: `.name`
  - `ExpressionRecordUpdate`: `{ Some.record | name = "text" }`

-}
type Expression
    = ExpressionUnit
    | ExpressionInteger Int
    | -- TODO join with Integer
      ExpressionHex Int
    | ExpressionFloat Float
    | ExpressionChar Char
    | ExpressionString { content : String, quotingStyle : StringQuotingStyle }
    | ExpressionOperatorFunction String
    | ExpressionNegation (Node Expression)
    | ExpressionParenthesized (Node Expression)
    | ExpressionTuple
        { part0 : Node Expression
        , part1 : Node Expression
        }
    | ExpressionTriple
        { part0 : Node Expression
        , part1 : Node Expression
        , part2 : Node Expression
        }
    | ExpressionCall
        -- TODO change to { called : Node Expression, argument0 : Node Expression, argument1Up : List (Node Expression) }
        (List (Node Expression))
    | ExpressionInfixOperation
        { operator : String
        , left : Node Expression
        , right : Node Expression
        }
    | -- TODO split into ExpressionReference and ExpressionReferenceVariantOrRecordTypeAliasCOnstructor
      ExpressionReference { qualification : ModuleName, name : String }
    | ExpressionIfThenElse
        { condition : Node Expression
        , onTrue : Node Expression
        , onFalse : Node Expression
        }
    | ExpressionLetIn
        { -- TODO split into declaration0 and declaration1Up
          declarations : List (Node LetDeclaration)
        , result : Node Expression
        }
    | ExpressionCaseOf
        { expression : Node Expression
        , -- TODO split into case0 and case1Up
          cases :
            List
                { pattern : Node Pattern
                , result : Node Expression
                }
        }
    | ExpressionLambda
        { parameters : List (Node Pattern)
        , result : Node Expression
        }
    | ExpressionRecord
        (List
            -- TODO remove Node wrapping
            (Node
                { name : Node String
                , value : Node Expression
                }
            )
        )
    | ExpressionList (List (Node Expression))
    | ExpressionRecordAccess
        { record : Node Expression
        , field : Node String
        }
    | ExpressionRecordAccessFunction String
    | ExpressionRecordUpdate
        { recordVariable : Node String
        , fields :
            List
                -- TODO remove Node wrapping
                (Node
                    { name : Node String
                    , value : Node Expression
                    }
                )
        }


{-| String literals can be single double-quoted (single line) and triple double-quoted (usually multi-line)?
Used by [`ExpressionString`](#Expression) and [`PatternString`](#Pattern)
-}
type StringQuotingStyle
    = StringSingleQuoted
    | StringTripleQuoted


{-| Element before the result of a let block
-}
type LetDeclaration
    = -- rename to LetValueOrFunctionDeclaration
      LetFunction ValueOrFunctionDeclarationInfo
    | LetDestructuring
        { pattern : Node Pattern
        , expression : Node Expression
        }


{-| Custom type for different type annotations. For example:

  - `TypeAnnotationVariable`: `a`
  - `TypeAnnotationConstruct`: `Maybe (Int -> String)`
  - `TypeAnnotationUnit`: `()`
  - `TypeAnnotationParenthesized`: `(a -> b)`
  - `TypeAnnotationRecord`: `{ name : String}`
  - `TypeAnnotationRecordExtension`: `{ a | name : String}`
  - `TypeAnnotationFunction`: `Int -> String`

TODO rename to Type(-)

-}
type TypeAnnotation
    = TypeAnnotationVariable String
    | TypeAnnotationConstruct
        { reference : Node { qualification : ModuleName, name : String }
        , arguments : List (Node TypeAnnotation)
        }
    | TypeAnnotationUnit
    | TypeAnnotationParenthesized (Node TypeAnnotation)
    | TypeAnnotationTuple
        { part0 : Node TypeAnnotation
        , part1 : Node TypeAnnotation
        }
    | TypeAnnotationTriple
        { part0 : Node TypeAnnotation
        , part1 : Node TypeAnnotation
        , part2 : Node TypeAnnotation
        }
    | TypeAnnotationRecord
        (List
            -- TODO remove Node wrapping
            (Node
                { name : Node String
                , value : Node TypeAnnotation
                }
            )
        )
    | TypeAnnotationRecordExtension
        { recordVariable : Node String
        , fields :
            -- TODO remove Node wrapping
            Node
                (List
                    (Node
                        -- TODO remove Node wrapping
                        { name : Node String
                        , value : Node TypeAnnotation
                        }
                    )
                )
        }
    | TypeAnnotationFunction
        { input : Node TypeAnnotation
        , output : Node TypeAnnotation
        }


{-| Custom type for all patterns such as:

  - `PatternIgnored`: `_` or `_name`
  - `PatternUnit`: `()`
  - `PatternChar`: `'c'`
  - `PatternString`: `"hello"`
  - `PatternInt`: `42`
  - `PatternHex`: `0x11`
  - `PatternTuple`: `(a, b)`
  - `PatternRecord`: `{name, age}`
  - `PatternListCons`: `x :: xs`
  - `PatternListExact`: `[ x, y ]`
  - `PatternVariable`: `x`
  - `PatternVariant`: `Just _`
  - `PatternAs`: `_ as x`
  - `PatternParenthesized`: `( _ )`

-}
type Pattern
    = PatternIgnored
    | PatternUnit
    | PatternChar Char
    | PatternString { content : String, quotingStyle : StringQuotingStyle }
    | PatternInt Int
    | -- TODO join with Integer
      PatternHex Int
    | PatternTuple { part0 : Node Pattern, part1 : Node Pattern }
    | PatternTriple
        { part0 : Node Pattern
        , part1 : Node Pattern
        , part2 : Node Pattern
        }
    | PatternRecord (List (Node String))
    | PatternListCons { head : Node Pattern, tail : Node Pattern }
    | PatternListExact (List (Node Pattern))
    | PatternVariable String
    | PatternVariant
        { qualification : List String
        , name : String
        , values : List (Node Pattern)
        }
    | PatternAs { pattern : Node Pattern, variable : Node String }
    | PatternParenthesized (Node Pattern)


{-| An element of the AST (Abstract Syntax Tree).

The purpose of this type is to add the information of the [`Range`](#Range), i.e. where in the source code the
element of the tree was found.

-}
type alias Node value =
    { range : Range, value : value }


{-| Combine two nodes, constructing a new node which will have the outer most range of the child nodes
-}
nodeCombine : (Node a -> Node b -> c) -> Node a -> Node b -> Node c
nodeCombine f a b =
    { range = { start = a.range.start, end = b.range.end }, value = f a b }


{-| Map the value within a node leaving the range untouched
-}
nodeMap : (a -> b) -> Node a -> Node b
nodeMap f node =
    { range = node.range, value = f node.value }


{-| Extract the range out of a `Node a`. Prefer `.range`
-}
nodeRange : Node value_ -> Range
nodeRange node =
    node.range


{-| Extract the value (`a`) out of a `Node a`. Prefer `.value`
-}
nodeValue : Node value -> value
nodeValue node =
    node.value


{-| Source location. Starts at 1 for the first line and 1 for the first character in the line
-}
type alias Location =
    { -- TODO rename to line
      row : Int
    , column : Int
    }


locationStart : Location
locationStart =
    { row = 1, column = 1 }


locationCompare : Location -> Location -> Basics.Order
locationCompare a b =
    if a.row < b.row then
        LT

    else if a.row > b.row then
        GT

    else
        Basics.compare a.column b.column


{-| Range for a piece of code with a start and end
-}
type alias Range =
    { start : Location
    , end : Location
    }


{-| Useless [`Range`](#Range), pointing to a 0-width region at the very start.
Avoid using this whenever you can
-}
rangeEmpty : Range
rangeEmpty =
    { start = locationStart
    , end = locationStart
    }


rangeIncludesLocation : Location -> Range -> Bool
rangeIncludesLocation location range =
    (locationCompare location range.start /= LT)
        && (locationCompare location range.end /= GT)
