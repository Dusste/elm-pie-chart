module Donut.SharedTypes exposing (ChartData, DataPoint)


type alias DataPoint =
    { label : String
    , groupBy : String
    , id : String
    , color : Maybe String
    }


type VoteState
    = NotVoted
    | HiddenVote Float
    | Voted Float


type alias ChartData =
    { uniqueVoteValue : String
    , percentage : Float
    , id : String
    , color : Maybe String
    }
