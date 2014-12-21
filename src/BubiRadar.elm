import Mouse
import Signal
import Signal ((<~), (~), Signal)
import Text (asText)
import Http
import Graphics.Element (flow, down)

port userLocation : Signal (Maybe Location)
port stationXmlIn : Signal (List StationXml) 

port stationXmlOut : Signal (Maybe String)
port stationXmlOut = getBubiData

port userLocationRequest : Signal ()
port userLocationRequest = Signal.constant ()

scene a b =
    flow down [
            asText a,
            asText b
        ]

main = scene <~ userLocation ~ stationXmlIn



type alias StationXml = {
        uid : String,
        lat : String,
        lng : String,
        unique_name : String,
        num_bikes : String,
        max_bikes : String
    }

type alias Location = {
        lat : Float,
        lng : Float
    }

type alias Uid = String
type alias Station = {
        uid : Uid,
        location : Location,
        unique_name : String,
        name : String,
        num_bikes : Int,
        max_bikes : Int,
        distance : Maybe Int
    }

type alias State = {
        stations: List Station,
        userLocation : Maybe Location,
        stationView : Maybe Uid,
        updateTime : Int,
        waitingForData : Bool
    }

getBubiData : Signal (Maybe String)
getBubiData =
    let url = "https://nextbike.net/maps/nextbike-live.xml?domains=mb"
        handleResp resp =
            case resp of
                Http.Success data -> Just data
                Http.Waiting -> Nothing
                Http.Failure _ _ -> Nothing
    in
        handleResp <~ Http.sendGet (Signal.constant url)
