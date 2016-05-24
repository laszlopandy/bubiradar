module Types exposing (..)

import Date exposing (Date)
import Geolocation


type alias Uid =
    String


type alias Meters =
    Int


type alias Location =
    { lat : Float
    , lng : Float
    }


type alias Station =
    { uid : Uid
    , location : Location
    , unique_name : String
    , name : String
    , num_bikes : Int
    , max_bikes : Int
    , distance : Maybe Meters
    }


type alias State =
    { flexSupported : Bool
    , stationView : Maybe Uid
    , stations : List Station
    , userLocation : Maybe Location
    , updateTime : Maybe Date
    , waitingForData : Bool
    , windowDimensions : ( Int, Int )
    }


type alias StationXml =
    { uid : String
    , lat : String
    , lng : String
    , unique_name : String
    , num_bikes : String
    , max_bikes : String
    }


type Action
    = ViewMap Uid
    | ViewList
    | Refresh
    | NoOp
    | BubiData ( String, Date )
    | StationsData (List StationXml)
    | UserLocation ( Geolocation.Location, Date )
