module Donut.Chart exposing (DonutInput(..), Model, Msg, init, update, view)

import Donut.SharedTypes as SharedTypes
import Donut.Util as Util
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type alias Model =
    { hoveredSegment : Maybe String
    }


initialModel : Model
initialModel =
    { hoveredSegment = Nothing }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


type DonutInput
    = GroupedBy (List SharedTypes.DataPoint)
    | RawAmounts (List SharedTypes.RawDataPoint)


type alias DonutOutput =
    { chartData : List SharedTypes.ChartData
    , cumulativeOffsets : List Float
    }


type Msg
    = SegmentHovered String
    | SegmentUnhovered


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SegmentHovered s ->
            ( { model
                | hoveredSegment = Just s
              }
            , Cmd.none
            )

        SegmentUnhovered ->
            ( { model | hoveredSegment = Nothing }, Cmd.none )


inputToOutput : DonutInput -> DonutOutput
inputToOutput dataPoint =
    let
        chartDataLst : List SharedTypes.ChartData
        chartDataLst =
            case dataPoint of
                GroupedBy dataPointLst ->
                    dataPointLst
                        |> List.sortBy
                            (\dataPoint_ ->
                                case dataPoint_.groupBy |> String.toInt |> Maybe.map toFloat of
                                    Just float ->
                                        float

                                    Nothing ->
                                        -1
                            )
                        |> Util.toChartData 1 (List.length dataPointLst |> toFloat)

                RawAmounts dataPointLst ->
                    Util.fromComplexToChartData dataPointLst

        percentLst : List Float
        percentLst =
            chartDataLst
                |> List.map .percentage

        {- It calculates the starting point (offset) for each segment in a donut chart.
           Each value is the sum of the previous percentages.
           The last value isn't a cumulative total, but the start position for that segment.
        -}
        cumulativeOffsets : List Float
        cumulativeOffsets =
            List.indexedMap
                (\i _ ->
                    if i == 0 then
                        0

                    else
                        percentLst
                            |> Util.slice 0 i
                            |> List.foldl (+) 0
                )
                percentLst
    in
    { chartData = chartDataLst
    , cumulativeOffsets = cumulativeOffsets
    }


view : (Msg -> msg) -> DonutInput -> Model -> Html msg
view toSelf donutInput model =
    let
        donutOutput : DonutOutput
        donutOutput =
            inputToOutput donutInput
    in
    Html.div
        [ HA.class "flex flex-row items-start gap-4 relative" ]
        [ Html.div
            []
            [ Svg.svg
                [ SvgAttr.viewBox "0 0 40 40"
                , SvgAttr.class "w-full h-auto"
                , SvgAttr.transform "rotate(-90 20 20)"
                , SvgAttr.viewBox "0 0 40 40"
                , SvgAttr.class "w-full h-auto"
                , SvgAttr.transform "rotate(-90 20 20)"
                , SvgAttr.transform "translate(0,-25px)"
                ]
                (Svg.circle
                    [ SvgAttr.cx "20"
                    , SvgAttr.cy "20"
                    , SvgAttr.r "15.915"
                    , SvgAttr.fill "transparent"
                    ]
                    []
                    :: segmentsWithEvents toSelf donutOutput model
                )
            ]
        , viewTooltip model donutOutput
        ]


segmentsWithEvents : (Msg -> msg) -> DonutOutput -> Model -> List (Svg msg)
segmentsWithEvents toSelf donutOutput model =
    let
        outerRadius : Float
        outerRadius =
            -- full circle's circumference
            -- 2 * pi * r
            15.91549430918954

        innerRadius =
            9.0

        baseTilt =
            45

        -- force first segment to start at 45°
        gap =
            (2 / outerRadius) * 180 / pi
    in
    List.indexedMap
        (\idx chartData ->
            let
                offset : Float
                offset =
                    donutOutput.cumulativeOffsets
                        |> List.Extra.getAt idx
                        |> Maybe.withDefault 0

                color : String
                color =
                    chartData.color |> Maybe.withDefault (Util.getHexColor idx)

                startAngle : Float
                startAngle =
                    (offset * 3.6) + (gap / 2) + baseTilt

                endAngle : Float
                endAngle =
                    ((offset + chartData.percentage) * 3.6) - (gap / 2) + baseTilt

                pathD : String
                pathD =
                    describeDonutSlice 20 20 innerRadius outerRadius startAngle endAngle
            in
            Svg.path
                [ HA.id chartData.uniqueVoteValue
                , SvgAttr.d pathD
                , SvgAttr.fill color
                , SvgAttr.stroke "transparent"
                , SvgAttr.strokeWidth "1"
                , SvgAttr.class "donut-segment"
                , HE.onMouseOver (toSelf <| SegmentHovered chartData.uniqueVoteValue)
                , HE.onMouseLeave (toSelf SegmentUnhovered)
                ]
                []
        )
        donutOutput.chartData


polarToCartesian : Float -> Float -> Float -> Float -> { x : Float, y : Float }
polarToCartesian centerX centerY radius angleInDegrees =
    let
        angleInRadians : Float
        angleInRadians =
            (angleInDegrees - 90) * pi / 180
    in
    { x = centerX + radius * cos angleInRadians
    , y = centerY + radius * sin angleInRadians
    }


describeDonutSlice : Float -> Float -> Float -> Float -> Float -> Float -> String
describeDonutSlice cx cy innerR outerR startAngle endAngle =
    let
        -- convert degrees to radians
        toRad deg =
            (deg - 90) * pi / 180

        -- outer arc points
        outerStart =
            polarToCartesian cx cy outerR endAngle

        outerEnd =
            polarToCartesian cx cy outerR startAngle

        -- inner arc points (reverse direction)
        innerStart =
            polarToCartesian cx cy innerR startAngle

        innerEnd =
            polarToCartesian cx cy innerR endAngle

        largeArcFlag =
            if abs (endAngle - startAngle) <= 180 then
                "0"

            else
                "1"
    in
    String.join " "
        [ "M"
        , String.fromFloat outerStart.x
        , String.fromFloat outerStart.y
        , "A"
        , String.fromFloat outerR
        , String.fromFloat outerR
        , "0"
        , largeArcFlag
        , "0"
        , String.fromFloat outerEnd.x
        , String.fromFloat outerEnd.y
        , "L"
        , String.fromFloat innerStart.x
        , String.fromFloat innerStart.y
        , "A"
        , String.fromFloat innerR
        , String.fromFloat innerR
        , "0"
        , largeArcFlag
        , "1"
        , String.fromFloat innerEnd.x
        , String.fromFloat innerEnd.y
        , "Z"
        ]


viewTooltip : Model -> DonutOutput -> Html msg
viewTooltip model donutOutput =
    case model.hoveredSegment of
        Just segmentId ->
            case List.filter (\c -> c.uniqueVoteValue == segmentId) donutOutput.chartData of
                [ chartData ] ->
                    Html.div
                        [ HA.class "bg-white shadow-md rounded px-2 py-1 border border-gray-200 text-sm absolute"
                        , HA.style "left" "90%"
                        ]
                        [ Html.p
                            [ HA.class "text-gray-700 font-semibold"
                            , HA.style "color" (chartData.color |> Maybe.withDefault "text-gray-700")
                            ]
                            [ Html.text chartData.uniqueVoteValue ]
                        , Html.p
                            [ HA.class "text-gray-500 font-semibold" ]
                            [ Html.text (String.fromInt (round chartData.percentage) ++ "%") ]
                        ]

                _ ->
                    Html.text ""

        Nothing ->
            Html.text ""
