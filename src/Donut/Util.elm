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
                -- Red colors
                if color == "bg-red-50" then
                    Just "#fef2f2"

                else if color == "bg-red-100" then
                    Just "#fee2e2"

                else if color == "bg-red-200" then
                    Just "#fecaca"

                else if color == "bg-red-300" then
                    Just "#fca5a5"

                else if color == "bg-red-400" then
                    Just "#f87171"

                else if color == "bg-red-500" then
                    Just "#ef4444"

                else if color == "bg-red-600" then
                    Just "#dc2626"

                else if color == "bg-red-700" then
                    Just "#b91c1c"

                else if color == "bg-red-800" then
                    Just "#991b1b"

                else if color == "bg-red-900" then
                    Just "#7f1d1d"

                else if color == "bg-red-950" then
                    Just "#450a0a"
                    -- Orange colors

                else if color == "bg-orange-50" then
                    Just "#fff7ed"

                else if color == "bg-orange-100" then
                    Just "#ffedd5"

                else if color == "bg-orange-200" then
                    Just "#fed7aa"

                else if color == "bg-orange-300" then
                    Just "#fdba74"

                else if color == "bg-orange-400" then
                    Just "#fb923c"

                else if color == "bg-orange-500" then
                    Just "#f97316"

                else if color == "bg-orange-600" then
                    Just "#ea580c"

                else if color == "bg-orange-700" then
                    Just "#c2410c"

                else if color == "bg-orange-800" then
                    Just "#9a3412"

                else if color == "bg-orange-900" then
                    Just "#7c2d12"

                else if color == "bg-orange-950" then
                    Just "#431407"
                    -- Amber colors

                else if color == "bg-amber-50" then
                    Just "#fffbeb"

                else if color == "bg-amber-100" then
                    Just "#fef3c7"

                else if color == "bg-amber-200" then
                    Just "#fde68a"

                else if color == "bg-amber-300" then
                    Just "#fcd34d"

                else if color == "bg-amber-400" then
                    Just "#fbbf24"

                else if color == "bg-amber-500" then
                    Just "#f59e0b"

                else if color == "bg-amber-600" then
                    Just "#d97706"

                else if color == "bg-amber-700" then
                    Just "#b45309"

                else if color == "bg-amber-800" then
                    Just "#92400e"

                else if color == "bg-amber-900" then
                    Just "#78350f"

                else if color == "bg-amber-950" then
                    Just "#451a03"
                    -- Yellow colors

                else if color == "bg-yellow-50" then
                    Just "#fefce8"

                else if color == "bg-yellow-100" then
                    Just "#fef9c3"

                else if color == "bg-yellow-200" then
                    Just "#fef08a"

                else if color == "bg-yellow-300" then
                    Just "#fde047"

                else if color == "bg-yellow-400" then
                    Just "#facc15"

                else if color == "bg-yellow-500" then
                    Just "#eab308"

                else if color == "bg-yellow-600" then
                    Just "#ca8a04"

                else if color == "bg-yellow-700" then
                    Just "#a16207"

                else if color == "bg-yellow-800" then
                    Just "#854d0e"

                else if color == "bg-yellow-900" then
                    Just "#713f12"

                else if color == "bg-yellow-950" then
                    Just "#422006"
                    -- Lime colors

                else if color == "bg-lime-50" then
                    Just "#f7fee7"

                else if color == "bg-lime-100" then
                    Just "#ecfccb"

                else if color == "bg-lime-200" then
                    Just "#d9f99d"

                else if color == "bg-lime-300" then
                    Just "#bef264"

                else if color == "bg-lime-400" then
                    Just "#a3e635"

                else if color == "bg-lime-500" then
                    Just "#84cc16"

                else if color == "bg-lime-600" then
                    Just "#65a30d"

                else if color == "bg-lime-700" then
                    Just "#4d7c0f"

                else if color == "bg-lime-800" then
                    Just "#3f6212"

                else if color == "bg-lime-900" then
                    Just "#365314"

                else if color == "bg-lime-950" then
                    Just "#1a2e05"
                    -- Green colors

                else if color == "bg-green-50" then
                    Just "#f0fdf4"

                else if color == "bg-green-100" then
                    Just "#dcfce7"

                else if color == "bg-green-200" then
                    Just "#bbf7d0"

                else if color == "bg-green-300" then
                    Just "#86efac"

                else if color == "bg-green-400" then
                    Just "#4ade80"

                else if color == "bg-green-500" then
                    Just "#22c55e"

                else if color == "bg-green-600" then
                    Just "#16a34a"

                else if color == "bg-green-700" then
                    Just "#15803d"

                else if color == "bg-green-800" then
                    Just "#166534"

                else if color == "bg-green-900" then
                    Just "#14532d"

                else if color == "bg-green-950" then
                    Just "#052e16"
                    -- Emerald colors

                else if color == "bg-emerald-50" then
                    Just "#ecfdf5"

                else if color == "bg-emerald-100" then
                    Just "#d1fae5"

                else if color == "bg-emerald-200" then
                    Just "#a7f3d0"

                else if color == "bg-emerald-300" then
                    Just "#6ee7b7"

                else if color == "bg-emerald-400" then
                    Just "#34d399"

                else if color == "bg-emerald-500" then
                    Just "#10b981"

                else if color == "bg-emerald-600" then
                    Just "#059669"

                else if color == "bg-emerald-700" then
                    Just "#047857"

                else if color == "bg-emerald-800" then
                    Just "#065f46"

                else if color == "bg-emerald-900" then
                    Just "#064e3b"

                else if color == "bg-emerald-950" then
                    Just "#022c22"
                    -- Teal colors

                else if color == "bg-teal-50" then
                    Just "#f0fdfa"

                else if color == "bg-teal-100" then
                    Just "#ccfbf1"

                else if color == "bg-teal-200" then
                    Just "#99f6e4"

                else if color == "bg-teal-300" then
                    Just "#5eead4"

                else if color == "bg-teal-400" then
                    Just "#2dd4bf"

                else if color == "bg-teal-500" then
                    Just "#14b8a6"

                else if color == "bg-teal-600" then
                    Just "#0d9488"

                else if color == "bg-teal-700" then
                    Just "#0f766e"

                else if color == "bg-teal-800" then
                    Just "#115e59"

                else if color == "bg-teal-900" then
                    Just "#134e4a"

                else if color == "bg-teal-950" then
                    Just "#042f2e"
                    -- Cyan colors

                else if color == "bg-cyan-50" then
                    Just "#ecfeff"

                else if color == "bg-cyan-100" then
                    Just "#cffafe"

                else if color == "bg-cyan-200" then
                    Just "#a5f3fc"

                else if color == "bg-cyan-300" then
                    Just "#67e8f9"

                else if color == "bg-cyan-400" then
                    Just "#22d3ee"

                else if color == "bg-cyan-500" then
                    Just "#06b6d4"

                else if color == "bg-cyan-600" then
                    Just "#0891b2"

                else if color == "bg-cyan-700" then
                    Just "#0e7490"

                else if color == "bg-cyan-800" then
                    Just "#155e75"

                else if color == "bg-cyan-900" then
                    Just "#164e63"

                else if color == "bg-cyan-950" then
                    Just "#083344"
                    -- Sky colors

                else if color == "bg-sky-50" then
                    Just "#f0f9ff"

                else if color == "bg-sky-100" then
                    Just "#e0f2fe"

                else if color == "bg-sky-200" then
                    Just "#bae6fd"

                else if color == "bg-sky-300" then
                    Just "#7dd3fc"

                else if color == "bg-sky-400" then
                    Just "#38bdf8"

                else if color == "bg-sky-500" then
                    Just "#0ea5e9"

                else if color == "bg-sky-600" then
                    Just "#0284c7"

                else if color == "bg-sky-700" then
                    Just "#0369a1"

                else if color == "bg-sky-800" then
                    Just "#075985"

                else if color == "bg-sky-900" then
                    Just "#0c4a6e"

                else if color == "bg-sky-950" then
                    Just "#082f49"
                    -- Blue colors

                else if color == "bg-blue-50" then
                    Just "#eff6ff"

                else if color == "bg-blue-100" then
                    Just "#dbeafe"

                else if color == "bg-blue-200" then
                    Just "#bfdbfe"

                else if color == "bg-blue-300" then
                    Just "#93c5fd"

                else if color == "bg-blue-400" then
                    Just "#60a5fa"

                else if color == "bg-blue-500" then
                    Just "#3b82f6"

                else if color == "bg-blue-600" then
                    Just "#2563eb"

                else if color == "bg-blue-700" then
                    Just "#1d4ed8"

                else if color == "bg-blue-800" then
                    Just "#1e40af"

                else if color == "bg-blue-900" then
                    Just "#1e3a8a"

                else if color == "bg-blue-950" then
                    Just "#172554"
                    -- Indigo colors

                else if color == "bg-indigo-50" then
                    Just "#eef2ff"

                else if color == "bg-indigo-100" then
                    Just "#e0e7ff"

                else if color == "bg-indigo-200" then
                    Just "#c7d2fe"

                else if color == "bg-indigo-300" then
                    Just "#a5b4fc"

                else if color == "bg-indigo-400" then
                    Just "#818cf8"

                else if color == "bg-indigo-500" then
                    Just "#6366f1"

                else if color == "bg-indigo-600" then
                    Just "#4f46e5"

                else if color == "bg-indigo-700" then
                    Just "#4338ca"

                else if color == "bg-indigo-800" then
                    Just "#3730a3"

                else if color == "bg-indigo-900" then
                    Just "#312e81"

                else if color == "bg-indigo-950" then
                    Just "#1e1b4b"
                    -- Violet colors

                else if color == "bg-violet-50" then
                    Just "#f5f3ff"

                else if color == "bg-violet-100" then
                    Just "#ede9fe"

                else if color == "bg-violet-200" then
                    Just "#ddd6fe"

                else if color == "bg-violet-300" then
                    Just "#c4b5fd"

                else if color == "bg-violet-400" then
                    Just "#a78bfa"

                else if color == "bg-violet-500" then
                    Just "#8b5cf6"

                else if color == "bg-violet-600" then
                    Just "#7c3aed"

                else if color == "bg-violet-700" then
                    Just "#6d28d9"

                else if color == "bg-violet-800" then
                    Just "#5b21b6"

                else if color == "bg-violet-900" then
                    Just "#4c1d95"

                else if color == "bg-violet-950" then
                    Just "#2e1065"
                    -- Purple colors

                else if color == "bg-purple-50" then
                    Just "#faf5ff"

                else if color == "bg-purple-100" then
                    Just "#f3e8ff"

                else if color == "bg-purple-200" then
                    Just "#e9d5ff"

                else if color == "bg-purple-300" then
                    Just "#d8b4fe"

                else if color == "bg-purple-400" then
                    Just "#c084fc"

                else if color == "bg-purple-500" then
                    Just "#a855f7"

                else if color == "bg-purple-600" then
                    Just "#9333ea"

                else if color == "bg-purple-700" then
                    Just "#7e22ce"

                else if color == "bg-purple-800" then
                    Just "#6b21a8"

                else if color == "bg-purple-900" then
                    Just "#581c87"

                else if color == "bg-purple-950" then
                    Just "#3b0764"
                    -- Fuchsia colors

                else if color == "bg-fuchsia-50" then
                    Just "#fdf4ff"

                else if color == "bg-fuchsia-100" then
                    Just "#fae8ff"

                else if color == "bg-fuchsia-200" then
                    Just "#f5d0fe"

                else if color == "bg-fuchsia-300" then
                    Just "#f0abfc"

                else if color == "bg-fuchsia-400" then
                    Just "#e879f9"

                else if color == "bg-fuchsia-500" then
                    Just "#d946ef"

                else if color == "bg-fuchsia-600" then
                    Just "#c026d3"

                else if color == "bg-fuchsia-700" then
                    Just "#a21caf"

                else if color == "bg-fuchsia-800" then
                    Just "#86198f"

                else if color == "bg-fuchsia-900" then
                    Just "#701a75"

                else if color == "bg-fuchsia-950" then
                    Just "#4a044e"
                    -- Pink colors

                else if color == "bg-pink-50" then
                    Just "#fdf2f8"

                else if color == "bg-pink-100" then
                    Just "#fce7f3"

                else if color == "bg-pink-200" then
                    Just "#fbcfe8"

                else if color == "bg-pink-300" then
                    Just "#f9a8d4"

                else if color == "bg-pink-400" then
                    Just "#f472b6"

                else if color == "bg-pink-500" then
                    Just "#ec4899"

                else if color == "bg-pink-600" then
                    Just "#db2777"

                else if color == "bg-pink-700" then
                    Just "#be185d"

                else if color == "bg-pink-800" then
                    Just "#9d174d"

                else if color == "bg-pink-900" then
                    Just "#831843"

                else if color == "bg-pink-950" then
                    Just "#500724"
                    -- Rose colors

                else if color == "bg-rose-50" then
                    Just "#fff1f2"

                else if color == "bg-rose-100" then
                    Just "#ffe4e6"

                else if color == "bg-rose-200" then
                    Just "#fecdd3"

                else if color == "bg-rose-300" then
                    Just "#fda4af"

                else if color == "bg-rose-400" then
                    Just "#fb7185"

                else if color == "bg-rose-500" then
                    Just "#f43f5e"

                else if color == "bg-rose-600" then
                    Just "#e11d48"

                else if color == "bg-rose-700" then
                    Just "#be123c"

                else if color == "bg-rose-800" then
                    Just "#9f1239"

                else if color == "bg-rose-900" then
                    Just "#881337"

                else if color == "bg-rose-950" then
                    Just "#4c0519"
                    -- Gray colors

                else if color == "bg-gray-50" then
                    Just "#f9fafb"

                else if color == "bg-gray-100" then
                    Just "#f3f4f6"

                else if color == "bg-gray-200" then
                    Just "#e5e7eb"

                else if color == "bg-gray-300" then
                    Just "#d1d5db"

                else if color == "bg-gray-400" then
                    Just "#9ca3af"

                else if color == "bg-gray-500" then
                    Just "#6b7280"

                else if color == "bg-gray-600" then
                    Just "#4b5563"

                else if color == "bg-gray-700" then
                    Just "#374151"

                else if color == "bg-gray-800" then
                    Just "#1f2937"

                else if color == "bg-gray-900" then
                    Just "#111827"

                else if color == "bg-gray-950" then
                    Just "#030712"
                    -- Slate colors

                else if color == "bg-slate-50" then
                    Just "#f8fafc"

                else if color == "bg-slate-100" then
                    Just "#f1f5f9"

                else if color == "bg-slate-200" then
                    Just "#e2e8f0"

                else if color == "bg-slate-300" then
                    Just "#cbd5e1"

                else if color == "bg-slate-400" then
                    Just "#94a3b8"

                else if color == "bg-slate-500" then
                    Just "#64748b"

                else if color == "bg-slate-600" then
                    Just "#475569"

                else if color == "bg-slate-700" then
                    Just "#334155"

                else if color == "bg-slate-800" then
                    Just "#1e293b"

                else if color == "bg-slate-900" then
                    Just "#0f172a"

                else if color == "bg-slate-950" then
                    Just "#020617"
                    -- Zinc colors

                else if color == "bg-zinc-50" then
                    Just "#fafafa"

                else if color == "bg-zinc-100" then
                    Just "#f4f4f5"

                else if color == "bg-zinc-200" then
                    Just "#e4e4e7"

                else if color == "bg-zinc-300" then
                    Just "#d4d4d8"

                else if color == "bg-zinc-400" then
                    Just "#a1a1aa"

                else if color == "bg-zinc-500" then
                    Just "#71717a"

                else if color == "bg-zinc-600" then
                    Just "#52525b"

                else if color == "bg-zinc-700" then
                    Just "#3f3f46"

                else if color == "bg-zinc-800" then
                    Just "#27272a"

                else if color == "bg-zinc-900" then
                    Just "#18181b"

                else if color == "bg-zinc-950" then
                    Just "#09090b"
                    -- Neutral colors

                else if color == "bg-neutral-50" then
                    Just "#fafafa"

                else if color == "bg-neutral-100" then
                    Just "#f5f5f5"

                else if color == "bg-neutral-200" then
                    Just "#e5e5e5"

                else if color == "bg-neutral-300" then
                    Just "#d4d4d4"

                else if color == "bg-neutral-400" then
                    Just "#a3a3a3"

                else if color == "bg-neutral-500" then
                    Just "#737373"

                else if color == "bg-neutral-600" then
                    Just "#525252"

                else if color == "bg-neutral-700" then
                    Just "#404040"

                else if color == "bg-neutral-800" then
                    Just "#262626"

                else if color == "bg-neutral-900" then
                    Just "#171717"

                else if color == "bg-neutral-950" then
                    Just "#0a0a0a"
                    -- Stone colors

                else if color == "bg-stone-50" then
                    Just "#fafaf9"

                else if color == "bg-stone-100" then
                    Just "#f5f5f4"

                else if color == "bg-stone-200" then
                    Just "#e7e5e4"

                else if color == "bg-stone-300" then
                    Just "#d6d3d1"

                else if color == "bg-stone-400" then
                    Just "#a8a29e"

                else if color == "bg-stone-500" then
                    Just "#78716c"

                else if color == "bg-stone-600" then
                    Just "#57534e"

                else if color == "bg-stone-700" then
                    Just "#44403c"

                else if color == "bg-stone-800" then
                    Just "#292524"

                else if color == "bg-stone-900" then
                    Just "#1c1917"

                else if color == "bg-stone-950" then
                    Just "#0c0a09"

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
        , ( 9, "bg-pink-700" )
        , ( 8, "bg-teal-700" )
        , ( 11, "bg-lime-900" )
        , ( 10, "bg-teal-800" )
        , ( 15, "bg-pink-200" )
        , ( 7, "bg-lime-300" )
        , ( 6, "bg-teal-500" )
        , ( 12, "bg-sky-600" )
        , ( 14, "bg-lime-700" )
        , ( 13, "Tw-teal-900" )
        , ( 16, "bg-lime-600" )
        , ( 17, "bg-pink-300" )

        -- , ( 18, Tw.sky_500 )
        -- , ( 19, Tw.teal_200 )
        -- , ( 20, Tw.pink_500 )
        -- , ( 21, Tw.teal_300 )
        -- , ( 22, Tw.lime_200 )
        -- , ( 23, Tw.sky_900 )
        -- , ( 24, Tw.pink_600 )
        -- , ( 25, Tw.teal_600 )
        -- , ( 26, Tw.sky_800 )
        -- , ( 27, Tw.pink_900 )
        -- , ( 28, Tw.sky_200 )
        -- , ( 29, Tw.lime_800 )
        -- , ( 30, Tw.pink_800 )
        -- , ( 31, Tw.sky_300 )
        ]
