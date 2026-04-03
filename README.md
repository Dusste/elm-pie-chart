# Elm Pie Chart: Customizable SVG Data Visualizations for Elm

![elm-pie-chart demo](https://dusste.github.io/elm-pie-chart/demo.gif)

[![Elm Package](https://img.shields.io/badge/elm--package-v1.0.0-blue.svg)](https://package.elm-lang.org/packages/Dusste/elm-pie-chart/latest/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)](https://github.com/Dusste/elm-pie-chart)

**Elm Pie Chart** is a lightweight, highly customizable library designed for creating responsive **SVG pie charts** and **donut charts** in the [Elm programming language](https://elm-lang.org/).

Built with performance and simplicity in mind, this package allows developers to render beautiful **data visualizations** with zero external CSS dependencies. Whether you need a simple breakdown or a complex dashboard component, this library provides a type-safe, "Elm-way" to handle circular data.

---

## 🚀 Key Features

- **Responsive SVG Rendering:** Charts scale perfectly to any container or screen size.
- **Donut Chart Support:** Easily switch between solid pie charts and donut-style layouts.
- **Custom Color Palettes:** Use your own Hex/RGB codes or standard Elm color modules.
- **Interactive Data Labels:** Support for custom tooltips and hover-state interactions.
- **Pure Elm:** No JavaScript interop required; 100% type-safe and reliable.

## 🚀 Demo

## Explore the live demo: [https://dusste.github.io/elm-pie-chart](https://dusste.github.io/elm-pie-chart)

## 🛠 Usage Example

Implementing a pie chart in your view is designed to be intuitive. Here is a basic code snippet to get you started:

```
import Donut.Chart as Chart

view : Model -> Html Msg
view model =
    let
        donutInputs : List Chart.DonutInput
        donutInputs =
            [ Chart.GroupedBy users
            , Chart.GroupedBy users1
            , Chart.RawAmounts users2
            ]
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
```
