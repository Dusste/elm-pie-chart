module Donut.SharedTypes exposing (ChartData, DataPoint, RawDataPoint)


type alias DataPoint msg =
    { label : String
    , groupBy : String
    , id : String
    , color : Maybe String
    , onClick : Maybe msg
    }


type alias RawDataPoint msg =
    { label : String
    , id : String
    , amount : Int
    , color : Maybe String
    , onClick : Maybe msg
    }


type alias ChartData msg =
    { uniqueVoteValue : String
    , percentage : Float
    , id : String
    , color : Maybe String
    , onClick : Maybe msg
    }
