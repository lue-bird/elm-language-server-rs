module TextGrid exposing
    ( Location, locationCompare, Range, rangeIncludesLocation
    )

{-| View Text as lines and columns

@docs Location, locationCompare, Range, rangeIncludesLocation

-}


{-| Source location.
Starts at 1 for the first line and 1 for the first character in the line
-}
type alias Location =
    { line : Int
    , column : Int
    }


locationCompare : Location -> Location -> Basics.Order
locationCompare a b =
    if a.line < b.line then
        LT

    else if a.line > b.line then
        GT

    else
        Basics.compare a.column b.column


{-| Range for a piece of code with a start and end
-}
type alias Range =
    { start : Location
    , end : Location
    }


rangeIncludesLocation : Location -> Range -> Bool
rangeIncludesLocation location range =
    -- can be optimized
    (case locationCompare location range.start of
        LT ->
            False

        EQ ->
            True

        GT ->
            True
    )
        && (case locationCompare location range.end of
                GT ->
                    False

                LT ->
                    True

                EQ ->
                    True
           )
