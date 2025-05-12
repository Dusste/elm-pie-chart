# 🍩 elm-pie-chart

An interactive, accessible, and customizable pie/donut chart module for Elm 0.19.1. Designed for seamless integration, this module offers hover tooltips, animated transitions, and Tailwind CSS support.

![elm-pie-chart demo](https://user-images.githubusercontent.com/your-username/demo.gif)

---

## ✨ Features

- **Donut-style charts** with a transparent center
- **Hover tooltips** displaying segment details
- **Smooth transitions** on hover
- **Tailwind CSS** integration for easy styling
- **Responsive design** suitable for various screen sizes
- **Accessible** with semantic SVG elements

## 🛠 Usage

You can see how the chart is used in `Main.elm`.

Here’s the relevant usage pattern:

```elm
import Donut.Chart as Chart
import Html exposing (Html)
import Html.Attributes as HA

-- MODEL

type alias Model =
    { chart : Chart.Model }

-- UPDATE

type Msg
    = ChartMsg Chart.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChartMsg chartMsg ->
            let
                ( updatedChart, cmd ) =
                    Chart.update chartMsg model.chart
            in
            ( { model | chart = updatedChart }, Cmd.map ChartMsg cmd )

-- VIEW

view : Model -> Html Msg
view model =
    Html.div []
        [ Chart.view ChartMsg chartData model.chart ]

chartData : List Chart.DonutInput
chartData =
    [ { label = "Name", id = "1", groupBy = "5", color = Just "red" }
    , { label = "Name 2", id = "2", groupBy = "0.5", color = Just "blue" }
    , { label = "Name 3", id = "3", groupBy = "2", color = Nothing }
    ]

---

## 🚀 Demo

Explore the live demo: [https://dusste.github.io/elm-pie-chart](https://dusste.github.io/elm-pie-chart)
```
