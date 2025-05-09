module Donut.Chart exposing (DonutInput, Model, Msg, init, update, view)

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


type alias DonutInput =
    List SharedTypes.DataPoint


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
        SegmentHovered i ->
            ( { model | hoveredSegment = Just i }, Cmd.none )

        SegmentUnhovered ->
            ( { model | hoveredSegment = Nothing }, Cmd.none )


inputToOutput : DonutInput -> DonutOutput
inputToOutput dataPointLst =
    let
        chartDataLst : List SharedTypes.ChartData
        chartDataLst =
            dataPointLst
                |> List.sortBy
                    (\dataPoint ->
                        case dataPoint.groupBy |> String.toInt |> Maybe.map toFloat of
                            Just float ->
                                float

                            Nothing ->
                                -1
                    )
                |> Util.toChartData 1 (List.length dataPointLst |> toFloat)

        percentLst =
            chartDataLst
                |> List.map .percentage

        {- It calculates the starting point (offset) for each segment in a donut chart.
           Each value is the sum of the previous percentages.
           The last value isn't a cumulative total, but the start position for that segment.
        -}
        cumulativeOffsets =
            percentLst
                |> List.indexedMap
                    (\i _ ->
                        if i == 0 then
                            0

                        else
                            percentLst
                                |> Util.slice 0 i
                                |> List.foldl (+) 0
                    )
    in
    { chartData = chartDataLst
    , cumulativeOffsets = cumulativeOffsets
    }


view : (Msg -> msg) -> DonutInput -> Model -> Html msg
view toSelf lst model =
    let
        donutOutput : DonutOutput
        donutOutput =
            inputToOutput lst

        radius : Float
        radius =
            -- full circle's circumference
            -- 2 * pi * r
            15.91549430918954

        strokeWidth =
            3.5

        baseCircle : List (Svg msg)
        baseCircle =
            [ Svg.circle
                [ SvgAttr.class "donut-hole"
                , SvgAttr.cx "20"
                , SvgAttr.cy "20"
                , SvgAttr.r (String.fromFloat radius)
                , SvgAttr.fill "transparent"
                ]
                []
            , Svg.circle
                [ SvgAttr.class "donut-ring"
                , SvgAttr.cx "20"
                , SvgAttr.cy "20"
                , SvgAttr.r (String.fromFloat radius)
                , SvgAttr.fill "transparent"
                , SvgAttr.strokeWidth (String.fromFloat strokeWidth)
                , SvgAttr.stroke "#EBEBEB"
                ]
                []
            ]

        segments : List (Svg msg)
        segments =
            List.indexedMap
                (\idx chartData ->
                    let
                        offset =
                            donutOutput.cumulativeOffsets
                                |> List.Extra.getAt idx
                                |> Maybe.withDefault 0

                        randomColor =
                            Util.getHexColor idx

                        color =
                            case chartData.color of
                                Just color_ ->
                                    if color_ == randomColor then
                                        -- TODO fgure out how to make sure color is unique
                                        Util.getHexColor 0

                                    else
                                        color_

                                Nothing ->
                                    randomColor

                        isHovered =
                            model.hoveredSegment == Just chartData.uniqueVoteValue
                    in
                    Svg.g
                        [ SvgAttr.class "segment-group" ]
                        ([ Svg.circle
                            [ SvgAttr.class "donut-segment"
                            , SvgAttr.stroke color
                            , SvgAttr.cx "20"
                            , SvgAttr.cy "20"
                            , HE.onMouseOver <| toSelf <| SegmentHovered chartData.uniqueVoteValue
                            , HE.onMouseLeave <| toSelf <| SegmentUnhovered
                            , SvgAttr.r (String.fromFloat radius)
                            , SvgAttr.fill "transparent"
                            , SvgAttr.strokeWidth (String.fromFloat strokeWidth)
                            , SvgAttr.strokeDasharray (String.fromFloat chartData.percentage ++ " " ++ String.fromFloat (100 - chartData.percentage))
                            , SvgAttr.strokeDashoffset (String.fromFloat (100 - offset))
                            ]
                            []
                         ]
                            ++ (if isHovered then
                                    [ Svg.text_
                                        [ SvgAttr.x (String.fromFloat 0)
                                        , SvgAttr.y (String.fromFloat 0)
                                        , SvgAttr.class "tooltip-label"
                                        , SvgAttr.textAnchor "middle"
                                        , SvgAttr.dominantBaseline "middle"
                                        ]
                                        [ Svg.text chartData.uniqueVoteValue ]
                                    ]

                                else
                                    []
                               )
                        )
                 -- , innerView chartData.id chartData.uniqueVoteValue initialModel
                )
                donutOutput.chartData
    in
    Html.div
        [ HA.style "display" "flex" ]
        [ Svg.svg
            [ SvgAttr.viewBox "0 0 40 40"
            , SvgAttr.class "donut"
            ]
            (baseCircle ++ segments)
        , Html.ul
            []
            (donutOutput.chartData
                |> List.map
                    (\chartData ->
                        Html.li
                            [ HA.style "display" "flex" ]
                            [ Html.p [] [ Html.text chartData.uniqueVoteValue ]
                            , Html.p [] [ Html.text (chartData.numOfVoters |> String.fromFloat) ]
                            ]
                    )
            )
        ]
