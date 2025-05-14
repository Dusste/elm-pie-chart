module Donut.Util exposing (..)

import Dict exposing (Dict)
import Donut.SharedTypes as SharedTypes


slice : Int -> Int -> List a -> List a
slice start end lst =
    List.take (end - start) (List.drop start lst)


fromComplexToChartData : List SharedTypes.RawDataPoint -> List SharedTypes.ChartData
fromComplexToChartData dataPointLst =
    let
        totalAmount : Int
        totalAmount =
            dataPointLst
                |> List.map .amount
                |> List.sum

        chartDataLst : List SharedTypes.ChartData
        chartDataLst =
            dataPointLst
                |> List.map
                    (\dataPoint ->
                        { percentage = (toFloat dataPoint.amount / toFloat totalAmount) * 100
                        , uniqueVoteValue = dataPoint.label
                        , id = dataPoint.id
                        , color = Just <| fromTWcolorToHex dataPoint.color
                        }
                    )
    in
    chartDataLst


toChartData : Float -> Float -> List SharedTypes.DataPoint -> List SharedTypes.ChartData
toChartData numOfVoters numOfVariations sortedLst =
    case sortedLst of
        [] ->
            []

        x :: xs ->
            let
                groupByLst : List String
                groupByLst =
                    List.map (\{ groupBy } -> groupBy) xs

                determineColor : String
                determineColor =
                    fromTWcolorToHex x.color
            in
            if List.isEmpty xs then
                { percentage = numOfVoters / numOfVariations * 100
                , uniqueVoteValue = x.groupBy
                , id = x.id
                , color = Just determineColor
                }
                    :: toChartData 1 numOfVariations []

            else if List.member x.groupBy groupByLst then
                [] ++ toChartData (numOfVoters + 1) numOfVariations xs

            else
                { uniqueVoteValue = x.groupBy
                , percentage = numOfVoters / numOfVariations * 100
                , id = x.id
                , color = Just determineColor
                }
                    :: toChartData 1 numOfVariations xs


getHexColor : Int -> String
getHexColor target =
    colorConfig
        |> Dict.get target
        |> fromTWcolorToHex


fromTWcolorToHex : Maybe String -> String
fromTWcolorToHex twCol =
    twCol
        |> Maybe.andThen
            (\color ->
                if color == "bg-pink-400" then
                    Just "#f472b6"

                else if color == "bg-sky-400" then
                    Just "#38bdf8"

                else if color == "bg-lime-400" then
                    Just "#a3e635"

                else if color == "bg-purple-900" then
                    Just "#581c87"

                else if color == "bg-sky-700" then
                    Just "#0369a1"

                else if color == "bg-lime-500" then
                    Just "#84cc16"

                else if color == "bg-pink-700" then
                    Just "#be185d"

                else if color == "bg-teal-700" then
                    Just "#0f766e"

                else if color == "bg-lime-900" then
                    Just "#365314"

                else if color == "bg-teal-800" then
                    Just "#115e59"

                else if color == "bg-pink-200" then
                    Just "#fbcfe8"

                else if color == "bg-lime-300" then
                    Just "#bef264"

                else if color == "bg-teal-500" then
                    Just "#14b8a6"

                else if color == "bg-sky-600" then
                    Just "#0284c7"

                else if color == "bg-lime-700" then
                    Just "#4d7c0f"

                else if color == "bg-teal-900" then
                    Just "#134e4a"

                else if color == "bg-lime-600" then
                    Just "#65a30d"

                else if color == "bg-pink-300" then
                    Just "#f9a8d4"

                else if color == "bg-green-400" then
                    Just "rgb(74 222 128)"

                else if color == "bg-orange-400" then
                    Just "rgb(251 146 60)"

                else if color == "bg-red-400" then
                    Just "rgb(248 113 113)"

                else if color == "bg-sky-500" then
                    Just "#0ea5e9"

                else if color == "bg-teal-200" then
                    Just "#99f6e4"

                else if color == "bg-pink-500" then
                    Just "#ec4899"

                else if color == "bg-teal-300" then
                    Just "#5eead4"

                else if color == "bg-lime-200" then
                    Just "#d9f99d"

                else if color == "bg-sky-900" then
                    Just "#0c4a6e"

                else if color == "bg-pink-600" then
                    Just "#db2777"

                else if color == "bg-teal-600" then
                    Just "#0d9488"

                else if color == "bg-sky-800" then
                    Just "#075985"

                else if color == "bg-pink-900" then
                    Just "#831843"

                else if color == "bg-sky-200" then
                    Just "#bae6fd"

                else if color == "bg-lime-800" then
                    Just "#3f6212"

                else if color == "bg-pink-800" then
                    Just "#9d174d"

                else if color == "bg-sky-300" then
                    Just "#7dd3fc"

                else
                    Just color
            )
        |> Maybe.withDefault "#2dd4bf"


colorConfig : Dict Int String
colorConfig =
    Dict.fromList
        [ ( 0, "bg-pink-400" )
        , ( 1, "bg-sky-400" )
        , ( 2, "bg-lime-400" )
        , ( 3, "bg-purple-900" )
        , ( 4, "bg-sky-700" )
        , ( 5, "bg-lime-500" )
        , ( 6, "bg-teal-500" )
        , ( 7, "bg-lime-900" )
        , ( 8, "bg-teal-700" )
        , ( 9, "bg-pink-700" )
        , ( 10, "bg-teal-800" )
        , ( 11, "bg-lime-900" )
        , ( 12, "bg-teal-800" )
        , ( 13, "bg-pink-200" )
        , ( 14, "bg-lime-300" )
        , ( 15, "bg-teal-500" )
        , ( 16, "bg-sky-600" )
        , ( 17, "bg-lime-700" )
        , ( 18, "bg-teal-900" )
        , ( 19, "bg-lime-600" )
        , ( 20, "bg-pink-300" )
        , ( 21, "bg-sky-500" )
        , ( 22, "bg-teal-200" )
        , ( 23, "bg-pink-500" )
        , ( 24, "bg-teal-300" )
        , ( 25, "bg-lime-200" )
        , ( 26, "bg-sky-900" )
        , ( 27, "bg-pink-600" )
        , ( 28, "bg-teal-600" )
        , ( 29, "bg-sky-800" )
        , ( 30, "bg-pink-900" )
        , ( 31, "bg-sky-200" )
        , ( 32, "bg-lime-800" )
        , ( 33, "bg-pink-800" )
        , ( 34, "bg-sky-300" )
        ]
