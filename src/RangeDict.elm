module RangeDict exposing (RangeDict, insert, mapFromList, singleton, toListMap, unionFromListMap)

import Dict exposing (Dict)
import TextGrid


type alias RangeDict value =
    Dict ( ( Int, Int ), ( Int, Int ) ) value


singleton : TextGrid.Range -> v -> RangeDict v
singleton range value =
    Dict.singleton (rangeToComparable range) value


{-| Indirect conversion from a list to key-value pairs to avoid successive List.map calls.
-}
mapFromList : (a -> ( TextGrid.Range, v )) -> List a -> RangeDict v
mapFromList toAssociation list =
    List.foldl
        (\element acc ->
            let
                ( range, v ) =
                    toAssociation element
            in
            Dict.insert (rangeToComparable range) v acc
        )
        Dict.empty
        list


unionFromListMap : (element -> RangeDict value) -> List element -> RangeDict value
unionFromListMap elementToDict list =
    list
        |> List.foldl
            (\el soFar -> Dict.union (el |> elementToDict) soFar)
            Dict.empty


insert : TextGrid.Range -> v -> RangeDict v -> RangeDict v
insert range value rangeDict =
    Dict.insert (rangeToComparable range) value rangeDict


toListMap : (TextGrid.Range -> value -> element) -> RangeDict value -> List element
toListMap rangeAndValueToElement rangeDict =
    rangeDict
        |> Dict.foldr
            (\range value soFar ->
                rangeAndValueToElement (range |> rangeFromComparable) value
                    :: soFar
            )
            []


rangeToComparable : TextGrid.Range -> ( ( Int, Int ), ( Int, Int ) )
rangeToComparable range =
    ( ( range.start.line, range.start.column )
    , ( range.end.line, range.end.column )
    )


rangeFromComparable : ( ( Int, Int ), ( Int, Int ) ) -> TextGrid.Range
rangeFromComparable ( start, end ) =
    { start = start |> locationFromComparable, end = end |> locationFromComparable }


locationFromComparable : ( Int, Int ) -> TextGrid.Location
locationFromComparable ( line, column ) =
    { line = line, column = column }
