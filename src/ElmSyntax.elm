module ElmSyntax exposing
    ( Module, ModuleName, Import
    , ModuleHeader, ModuleHeaderSpecific(..)
    , Exposing(..), Expose(..)
    , Declaration(..), ChoiceTypeDeclaration, OperatorDeclaration, InfixDirection(..), TypeAliasDeclaration, ValueOrFunctionDeclaration
    , Pattern(..), Expression(..), LetDeclaration(..), StringQuotingStyle(..), IntBase(..), Type(..)
    , Node, nodeCombine
    )

{-| Elm syntax tree

@docs Module, ModuleName, Import
@docs ModuleHeader, ModuleHeaderSpecific
@docs Exposing, Expose
@docs Declaration, ChoiceTypeDeclaration, OperatorDeclaration, InfixDirection, TypeAliasDeclaration, ValueOrFunctionDeclaration
@docs Pattern, Expression, LetDeclaration, StringQuotingStyle, IntBase, Type
@docs Node, nodeCombine

-}

import TextGrid


{-| module header. For example:

    module Html.Attributes exposing (style)

-}
type alias ModuleHeader =
    { moduleName : Node ModuleName
    , exposing_ : Node Exposing
    , specific : Maybe ModuleHeaderSpecific
    }


type ModuleHeaderSpecific
    = ModuleHeaderSpecificPort { moduleKeywordRange : TextGrid.Range }
    | ModuleHeaderSpecificEffect
        { moduleKeywordRange : TextGrid.Range
        , command : Maybe (Node String)
        , subscription : Maybe (Node String)
        }


{-| Used for imports, module names, and for qualification.
For example:

    module ElmSyntax ...

    import Foo.Bar ...

    My.Module.something

    My.Module.SomeType

-}
type alias ModuleName =
    List String


{-| A file
-}
type alias Module =
    { header : Node ModuleHeader
    , imports : List (Node Import)
    , declarations :
        List
            { documentation : Maybe (Node String)
            , declaration : Node Declaration
            }
    , comments : List (Node String)
    }


{-| exposing declaration for both imports and module headers.
For example:

    exposing (Foo(..))
    exposing (..)

-}
type Exposing
    = ExposingAll TextGrid.Range
    | ExposingExplicit (List (Node Expose))


{-| An exposed entity
-}
type Expose
    = ExposeOperator String
    | ExposeVariable String
    | ExposeTypeName String
    | ExposeChoiceType
        { name : String
        , openRange : TextGrid.Range
        }


{-| For example:

    import Html.Attributes as HA exposing (style)

-}
type alias Import =
    { moduleName : Node ModuleName
    , alias :
        Maybe
            { asKeywordRange : TextGrid.Range
            , name : Node String
            }
    , exposing_ : Maybe (Node Exposing)
    }


{-| A module-level declaration. Can be one of the following:

  - Function/value declaration: `add x y = x + y`
  - Custom type declaration: `type Color = Blue | Red`
  - Type alias declaration: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Infix declaration. You will probably not need this, while only core packages can define these.

-}
type Declaration
    = -- TODO they all share the option to have documentation, move it outside
      -- TODO add DeclarationValueOrFunctionSignatureOnly
      DeclarationValueOrFunction ValueOrFunctionDeclaration
    | DeclarationTypeAlias TypeAliasDeclaration
    | DeclarationChoiceType ChoiceTypeDeclaration
    | DeclarationPort
        { name : Node String
        , type_ : Node Type
        }
    | DeclarationOperator OperatorDeclaration


{-| custom type. For example:

    {-| This is a color
    -}
    type Color
        = Blue
        | Red

-}
type alias ChoiceTypeDeclaration =
    { name : Node String
    , parameters : List (Node String)
    , equalsKeySymbolRange : TextGrid.Range
    , variant0 :
        { name : Node String
        , values : List (Node Type)
        }
    , variant1Up :
        List
            { -- the vertical bar |
              orKeySymbolRange : TextGrid.Range
            , name : Node String
            , values : List (Node Type)
            }
    }


{-| `type alias` declaration. For example:

    {-| This is a person
    -}
    type alias Person =
        { name : String
        , age : Int
        }

-}
type alias TypeAliasDeclaration =
    { aliasKeywordRange : TextGrid.Range
    , name : Node String
    , parameters : List (Node String)
    , equalsKeySymbolRange : TextGrid.Range
    , type_ : Node Type
    }


{-| `infix` declaration, for example

    infix left  0 (|>) = apR

-}
type alias OperatorDeclaration =
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


{-| value/function declaration, for example

    add2 n =
        n + 2

    hello : String
    hello =
        "Yahallo!"

-}
type alias ValueOrFunctionDeclaration =
    { signature :
        Maybe
            { name : Node String
            , type_ : Node Type
            }
    , name : String
    , implementationNameRange : TextGrid.Range
    , parameters : List (Node Pattern)
    , equalsKeySymbolRange : TextGrid.Range
    , result : Node Expression
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
  - `ExpressionTuple`: `( a, b )`
  - `ExpressionTriple`: `( a, b, c )`
  - `ExpressionCall`: `add a b`
  - `ExpressionInfixOperation`: `a + b`
  - `ExpressionReference`: `add` or `Basics.True` or `portCmd`
  - `ExpressionIfThenElse`: `if a then b else c`
  - `ExpressionLetIn`: `let a = 4 in a`
  - `ExpressionCaseOf`: `case a of` followed by pattern matches
  - `ExpressionLambda`: `\a -> a`
  - `ExpressionRecord`: `{ name = "text" }`
  - `ExpressionList`: `[ x, y ]`
  - `ExpressionRecordAccess`: `a.name`
  - `ExpressionRecordAccessFunction`: `.name`
  - `ExpressionRecordUpdate`: `{ record | name = "text" }`

-}
type Expression
    = ExpressionUnit
    | ExpressionInteger { value : Int, base : IntBase }
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
        { called : Node Expression
        , argument0 : Node Expression
        , argument1Up : List (Node Expression)
        }
    | ExpressionInfixOperation
        { operator : Node String
        , left : Node Expression
        , right : Node Expression
        }
    | -- TODO split into ExpressionReference and ExpressionReferenceVariantOrRecordTypeAliasCOnstructor
      ExpressionReference { qualification : ModuleName, name : String }
    | ExpressionIfThenElse
        { condition : Node Expression
        , thenKeywordRange : TextGrid.Range
        , onTrue : Node Expression
        , elseKeywordRange : TextGrid.Range
        , onFalse : Node Expression
        }
    | ExpressionLetIn
        { declaration0 : Node LetDeclaration
        , declaration1Up : List (Node LetDeclaration)
        , inKeywordRange : TextGrid.Range
        , result : Node Expression
        }
    | ExpressionCaseOf
        { matched : Node Expression
        , ofKeywordRange : TextGrid.Range
        , case0 :
            { pattern : Node Pattern
            , arrowKeySymbolRange : TextGrid.Range
            , result : Node Expression
            }
        , case1Up :
            List
                { pattern : Node Pattern
                , arrowKeySymbolRange : TextGrid.Range
                , result : Node Expression
                }
        }
    | ExpressionLambda
        { parameter0 : Node Pattern
        , parameter1Up : List (Node Pattern)
        , arrowKeySymbolRange : TextGrid.Range
        , result : Node Expression
        }
    | ExpressionRecord
        (List
            { name : Node String
            , equalsKeySymbolRange : TextGrid.Range
            , value : Node Expression
            }
        )
    | ExpressionList (List (Node Expression))
    | ExpressionRecordAccess
        { record : Node Expression
        , field : Node String
        }
    | ExpressionRecordAccessFunction String
    | ExpressionRecordUpdate
        { recordVariable : Node String
        , -- vertical bar |
          barKeySymbolRange : TextGrid.Range
        , field0 :
            { name : Node String
            , equalsKeySymbolRange : TextGrid.Range
            , value : Node Expression
            }
        , field1Up :
            List
                { name : Node String
                , equalsKeySymbolRange : TextGrid.Range
                , value : Node Expression
                }
        }


type IntBase
    = IntBase10
    | IntBase16


{-| String literals can be single double-quoted (single line) and triple double-quoted (usually multi-line)?
Used by [`ExpressionString`](#Expression) and [`PatternString`](#Pattern)
-}
type StringQuotingStyle
    = StringSingleQuoted
    | StringTripleQuoted


{-| Element before the result of a let block
-}
type LetDeclaration
    = LetValueOrFunctionDeclaration ValueOrFunctionDeclaration
    | LetDestructuring
        { pattern : Node Pattern
        , equalsKeySymbolRange : TextGrid.Range
        , expression : Node Expression
        }


{-| Custom type for different type annotations. For example:

  - `TypeUnit`: `()`
  - `TypeVariable`: `a`
  - `TypeConstruct`: `Maybe (Int -> String)`
  - `TypeParenthesized`: `a -> b`
  - `TypeTuple`: `( a, b )`
  - `TypeTriple`: `( a, b, c )`
  - `TypeRecord`: `{ name : String}`
  - `TypeRecordExtension`: `{ a | name : String}`
  - `TypeFunction`: `Int -> String`

-}
type Type
    = TypeUnit
    | TypeVariable String
    | TypeConstruct
        { reference : Node { qualification : ModuleName, name : String }
        , arguments : List (Node Type)
        }
    | TypeParenthesized (Node Type)
    | TypeTuple
        { part0 : Node Type
        , part1 : Node Type
        }
    | TypeTriple
        { part0 : Node Type
        , part1 : Node Type
        , part2 : Node Type
        }
    | TypeRecord
        (List
            { name : Node String
            , colonKeySymbolRange : TextGrid.Range
            , value : Node Type
            }
        )
    | TypeRecordExtension
        { recordVariable : Node String
        , -- vertical bar |
          barKeySymbolRange : TextGrid.Range
        , field0 :
            { name : Node String
            , colonKeySymbolRange : TextGrid.Range
            , value : Node Type
            }
        , field1Up :
            List
                { name : Node String
                , colonKeySymbolRange : TextGrid.Range
                , value : Node Type
                }
        }
    | TypeFunction
        { input : Node Type
        , arrowKeySymbolRange : TextGrid.Range
        , output : Node Type
        }


{-| Custom type for all patterns such as:

  - `PatternIgnored`: `_`
  - `PatternUnit`: `()`
  - `PatternChar`: `'c'`
  - `PatternString`: `"hello"`
  - `PatternInt`: `42`
  - `PatternHex`: `0x11`
  - `PatternTuple`: `( a, b )`
  - `PatternTriple`: `( a, b, c )`
  - `PatternRecord`: `{ name, age }`
  - `PatternListCons`: `x :: xs`
  - `PatternListExact`: `[ x, y ]`
  - `PatternVariable`: `x`
  - `PatternVariant`: `Just _`
  - `PatternAs`: `_ as x`
  - `PatternParenthesized`: `(_)`

-}
type Pattern
    = PatternIgnored
    | PatternUnit
    | PatternVariable String
    | PatternChar Char
    | PatternInt { value : Int, base : IntBase }
    | PatternString { content : String, quotingStyle : StringQuotingStyle }
    | PatternRecord (List (Node String))
    | PatternParenthesized (Node Pattern)
    | PatternAs
        { pattern : Node Pattern
        , asKeywordRange : TextGrid.Range
        , variable : Node String
        }
    | PatternTuple { part0 : Node Pattern, part1 : Node Pattern }
    | PatternTriple
        { part0 : Node Pattern
        , part1 : Node Pattern
        , part2 : Node Pattern
        }
    | PatternListCons
        { head : Node Pattern
        , consKeySymbolRange : TextGrid.Range
        , tail : Node Pattern
        }
    | PatternListExact (List (Node Pattern))
    | PatternVariant
        { qualification : List String
        , name : String
        , values : List (Node Pattern)
        }


{-| An element of the AST (Abstract Syntax Tree).

The purpose of this type is to add the information of the [`Range`](TextGrid#Range), i.e. where in the source code the
element of the tree was found.

-}
type alias Node value =
    { range : TextGrid.Range, value : value }


{-| Combine two nodes, constructing a new node which will have the outer most range of the child nodes
-}
nodeCombine : (Node a -> Node b -> c) -> Node a -> Node b -> Node c
nodeCombine f a b =
    { range = { start = a.range.start, end = b.range.end }, value = f a b }
