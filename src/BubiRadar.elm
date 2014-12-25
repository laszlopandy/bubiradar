import Mouse
import Signal
import Signal ((<~), (~), Signal)
import Text (asText)
import Http
import Graphics.Element (flow, down)
import String
import Result
import List

{- Inward ports -}
port userLocation : Signal (Maybe Location)
port stationXmlIn : Signal (List StationXml) 
{- Outward ports -}
port stationXmlOut : Signal (Maybe String)
port stationXmlOut = getBubiData

port userLocationRequest : Signal ()
port userLocationRequest = Signal.constant ()

--resultMap2 : (a -> b -> c) -> Result e a -> Result e b -> Result e c
--resultMap2 f a b = (Result.map f a) `Result.andThen` (\x -> Result.map x b)

(<~~) : (a -> b) -> Result e a -> Result e b
(<~~) = Result.map
(~~~) : Result e (a -> b) -> Result e a -> Result e b
(~~~) f a = f `Result.andThen` (\x -> Result.map x a)


makeLocation : String -> String -> Result String Location
makeLocation lat lng =
    Location
        <~~ (String.toFloat lat)
        ~~~ (String.toFloat lng)

makeStation : StationXml -> Result String Station
makeStation xml =
    let makeRecord num_bikes max_bikes loc = {
                uid = xml.uid,
                location = loc,
                unique_name = xml.unique_name,
                name = xml.unique_name,
                num_bikes = num_bikes,
                max_bikes = max_bikes,
                distance = Nothing
            }
    in
        makeRecord
            <~~ String.toInt xml.num_bikes
            ~~~ String.toInt xml.max_bikes
            ~~~ makeLocation xml.lat xml.lng

stations : List StationXml -> List Station
stations list =
    List.filterMap (makeStation >> Result.toMaybe) list

main =
    Signal.map
        asText
        (Signal.map2
            (,)
            userLocation
            (Signal.map stations stationXmlIn))

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
