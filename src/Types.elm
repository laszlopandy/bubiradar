module Types where

import Date (Date)

type alias Uid = String

type alias Meters = Int

type alias Location = {
        lat : Float,
        lng : Float
    }

type alias Station = {
        uid : Uid,
        location : Location,
        unique_name : String,
        name : String,
        num_bikes : Int,
        max_bikes : Int,
        distance : Maybe Meters
    }

type alias State = {
        stations: List Station,
        userLocation : Maybe Location,
        stationView : Maybe Uid,
        updateTime : Date,
        waitingForData : Bool,
        flexSupported : Bool
    }
