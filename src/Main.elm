module Main exposing (main)

import Browser
import Donut.Chart as Chart
import Html exposing (Html)
import Html.Attributes as HA



-- MODEL


type alias Model =
    { count : Int, chart : Chart.Model }


initialModel : Model
initialModel =
    { count = 0, chart = Chart.init |> Tuple.first }



-- MESSAGES


type Msg
    = Increment
    | Decrement
    | ChartMsg Chart.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        ChartMsg msg_ ->
            let
                ( m, cmd ) =
                    Chart.update msg_ model.chart
            in
            ( { model | chart = m }, Cmd.map ChartMsg cmd )



-- VIEW


users =
    [ { label = "Name"
      , id = "123213123321"
      , groupBy = "5"
      , color = Just "red"
      }
    , { label = "Name 2"
      , id = "543534543543"
      , groupBy = "0.5"
      , color = Just "blue"
      }
    , { label = "Name 3"
      , id = "88667767676"
      , groupBy = "2"
      , color = Nothing
      }
    , { label = "Name 4"
      , id = "999989844322"
      , groupBy = "1"
      , color = Just "yellow"
      }
    ]


users2 =
    [ { label = "low"
      , id = "low"
      , amount = 30
      , color = Just "green"
      }
    , { label = "medium"
      , id = "medium"
      , amount = 50
      , color = Just "yellow"
      }
    , { label = "high"
      , id = "high"
      , amount = 20
      , color = Just "red"
      }
    ]


users1 =
    [ { label = "Name"
      , id = "123213123321"
      , groupBy = "apple"
      , color = Just "red"
      }
    , { label = "Name 2"
      , id = "543534543543"
      , groupBy = "cherry"
      , color = Just "blue"
      }
    , { label = "Name 3"
      , id = "88667767676"
      , groupBy = "pinneapple"
      , color = Just "green"
      }
    , { label = "Name 4"
      , id = "999989844322"
      , groupBy = "banana"
      , color = Nothing
      }
    , { label = "Name 5"
      , id = "999989844322"
      , groupBy = "blueberry"
      , color = Just "pink"
      }
    ]


view : Model -> Html Msg
view model =
    let
        donutInputs : List Chart.DonutInput
        donutInputs =
            [ Chart.GroupedBy users, Chart.GroupedBy users1, Chart.RawAmounts users2 ]
    in
    Html.div
        []
        [ Html.h1
            [ HA.class "text-center text-4xl m-2 text-cyan-400" ]
            [ Html.text "Pie Charts" ]
        , Html.ul
            [ HA.class "flex" ]
            (donutInputs
                |> List.map
                    (\inputs ->
                        Html.li
                            [ HA.class "text-red-500 flex w-[500px] h-[500px]" ]
                            [ Chart.view ChartMsg inputs model.chart ]
                    )
            )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
