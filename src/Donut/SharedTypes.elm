module Donut.SharedTypes exposing (ChartData, DataPoint, RawDataPoint)


type alias DataPoint =
    { label : String
    , groupBy : String
    , id : String
    , color : Maybe String
    }


type alias RawDataPoint =
    { label : String
    , id : String
    , amount : Int
    , color : Maybe String
    }


type alias ChartData =
    { uniqueVoteValue : String
    , percentage : Float
    , id : String
    , color : Maybe String
    }
