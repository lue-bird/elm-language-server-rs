module RangeDict exposing (RangeDict(..), empty, foldl, insert, justValuesMap, mapFromList, singleton, toListMap, union, unionFromListMap)

import Dict exposing (Dict)
import TextGrid


type RangeDict v
    = RangeDict (Dict ( ( Int, Int ), ( Int, Int ) ) v)


empty : RangeDict v_
empty =
    RangeDict Dict.empty


singleton : TextGrid.Range -> v -> RangeDict v
singleton range value =
    RangeDict (Dict.singleton (rangeToComparable range) value)


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
        |> RangeDict


unionFromListMap : (element -> RangeDict value) -> List element -> RangeDict value
unionFromListMap elementToDict list =
    list
        |> List.foldl
            (\el soFar -> union (el |> elementToDict) soFar)
            empty


insert : TextGrid.Range -> v -> RangeDict v -> RangeDict v
insert range value (RangeDict rangeDict) =
    RangeDict (Dict.insert (rangeToComparable range) value rangeDict)


justValuesMap : (TextGrid.Range -> value -> Maybe valueMapped) -> RangeDict value -> RangeDict valueMapped
justValuesMap rangeAndValueMap rangeDict =
    rangeDict
        |> foldl
            (\range value soFar ->
                case rangeAndValueMap range value of
                    Nothing ->
                        soFar

                    Just valueMapped ->
                        soFar |> insert range valueMapped
            )
            empty


toListMap : (TextGrid.Range -> value -> element) -> RangeDict value -> List element
toListMap rangeAndValueToElement rangeDict =
    rangeDict
        |> foldr
            (\range value soFar ->
                rangeAndValueToElement range value :: soFar
            )
            []


foldr : (TextGrid.Range -> v -> folded -> folded) -> folded -> RangeDict v -> folded
foldr reduce initialFolded (RangeDict rangeDict) =
    rangeDict
        |> Dict.foldr (\range value -> reduce (rangeFromTupleTuple range) value)
            initialFolded


foldl : (TextGrid.Range -> v -> folded -> folded) -> folded -> RangeDict v -> folded
foldl reduce initialFolded (RangeDict rangeDict) =
    rangeDict
        |> Dict.foldl (\range value -> reduce (rangeFromTupleTuple range) value)
            initialFolded


union : RangeDict v -> RangeDict v -> RangeDict v
union (RangeDict aRangeDict) (RangeDict bRangeDict) =
    RangeDict (Dict.union aRangeDict bRangeDict)


rangeToComparable : TextGrid.Range -> ( ( Int, Int ), ( Int, Int ) )
rangeToComparable range =
    ( ( range.start.line, range.start.column )
    , ( range.end.line, range.end.column )
    )


rangeFromTupleTuple : ( ( Int, Int ), ( Int, Int ) ) -> TextGrid.Range
rangeFromTupleTuple ( start, end ) =
    { start = start |> locationFromTuple, end = end |> locationFromTuple }


locationFromTuple : ( Int, Int ) -> TextGrid.Location
locationFromTuple ( line, column ) =
    { line = line, column = column }
