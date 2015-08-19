module Types where

import Date exposing (Date)
import Signal

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
        stationView : Maybe Uid
    }

type Action
    = ViewMap Uid
    | ViewList

type alias RenderParams = {
        actionChannel : Signal.Address Action,
        refreshChannel : Signal.Address (),
        state : State,
        stations: List Station,
        userLocation : Maybe Location,
        updateTime : Date,
        waitingForData : Bool,
        flexSupported : Bool,
        windowDimensions : (Int, Int)
    }
