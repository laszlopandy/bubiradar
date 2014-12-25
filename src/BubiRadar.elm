import Mouse
import Signal
import Signal ((<~), (~), Signal)
import Text (asText)
import Http
import Graphics.Element (flow, down)
import String
import Result
import List
import Maybe
import Debug

{- Inward ports -}
port userLocation : Signal (Maybe Location)
port stationXmlIn : Signal (List StationXml) 
{- Outward ports -}
port stationXmlOut : Signal (Maybe String)
port stationXmlOut = getBubiData

port userLocationRequest : Signal ()
port userLocationRequest = Signal.constant ()


andThenMap : Result e (a -> b) -> Result e a -> Result e b
andThenMap func a = func `Result.andThen` (\x -> Result.map x a)


makeLocation : String -> String -> Result String Location
makeLocation lat lng =
    Location
        `Result.map` (String.toFloat lat)
        `andThenMap` (String.toFloat lng)


makePrettyName unique_name =
    let delim = String.slice 4 5 unique_name
    in
        if  | delim == "-" || delim == " " ->
                unique_name
                    |> String.dropLeft 5
                    |> String.trim
            | otherwise ->
                Debug.crash unique_name


toFixed : Int -> Float -> String
toFixed exp num =
    let factor = 10 ^ exp
        floatRound = toFloat << round
    in
        -- TODO: pad with zeros
        toString ((floatRound (num * factor)) / factor)


makePrettyDistance : Meters -> String
makePrettyDistance dist =
    if  | dist <= 10 ->
            "10m"
        | dist < 1000 ->
            toString (toFixed -1 dist) ++ "m"
        | otherwise ->
            let kilos = dist / 1000
                decimalPlaces = if kilos < 10.0 then 1 else 0
            in
                toString (toFixed decimalPlaces kilos) ++ "km"


makeStation : StationXml -> Result String Station
makeStation xml =
    let makeRecord num_bikes max_bikes loc = {
                uid = xml.uid,
                location = loc,
                unique_name = xml.unique_name,
                name = makePrettyName xml.unique_name,
                num_bikes = num_bikes,
                max_bikes = max_bikes,
                distance = Nothing
            }
    in
        makeRecord
            `Result.map` String.toInt xml.num_bikes
            `andThenMap` String.toInt xml.max_bikes
            `andThenMap` makeLocation xml.lat xml.lng


calcDistance : Location -> Location -> Meters
calcDistance a b =
    let cosDeg = cos << degrees
        dLat = sin <| (degrees (b.lat - a.lat)) / 2
        dLng = sin <| (degrees (b.lng - a.lng)) / 2
        x = (dLat * dLat) + (cosDeg a.lat * cosDeg b.lat * dLng * dLng)
        radius = 6371  -- Radius of the earth in km
        distKm = radius * 2 * (atan2 (sqrt x) (sqrt 1-x))
    in
        round (distKm * 1000)


updateStationDistance : Location -> Station -> Station
updateStationDistance userLocation station =
    { station |
        distance <- Just (calcDistance userLocation station.location)
    }


stations : List StationXml -> Maybe Location -> List Station
stations xmlList loc =
    let list =
            xmlList |> List.filterMap (makeStation >> Result.toMaybe)
    in
        case loc of
            Nothing ->
                list |> List.sortBy .name
            Just userLocation ->
                list
                    |> List.map (updateStationDistance userLocation)
                    |> List.sortBy (.distance >> Maybe.withDefault 0)


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


main =
    Signal.map
        asText
        (Signal.map2
            (,)
            userLocation
            (Signal.map2 stations stationXmlIn userLocation))


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

type alias Meters = Int

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
        updateTime : Int,
        waitingForData : Bool
    }
