import Date
import Http
import List
import Maybe
import Result
import Signal
import Signal ((<~), (~), Signal)
import String
import Time

import HtmlRender
import Types (Location, Station, RenderParams, State, Meters, Uid, Action(..))

{- Inward ports -}
port flexSupported : Signal Bool
port userLocation : Signal (Maybe Location)
port stationXmlIn : Signal (List StationXml)
{- Outward ports -}
port stationXmlOut : Signal (Maybe String)
port stationXmlOut = getBubiData

port userLocationRequest : Signal ()
port userLocationRequest = Signal.constant ()


map2 : (a -> b -> c) -> Result e a -> Result e b -> Result e c
map2 f a b =
    Result.map f a `Result.andThen` (\x -> Result.map x b)

map3 : (a -> b -> c -> d) -> Result e a -> Result e b -> Result e c -> Result e d
map3 f a b c =
    map2 f a b `Result.andThen` (\x -> Result.map x c)


makeLocation : String -> String -> Result String Location
makeLocation lat lng =
    map2
        Location
        (String.toFloat lat)
        (String.toFloat lng)


makePrettyName unique_name =
    let delim = String.slice 4 5 unique_name
    in
        if  | delim == "-" || delim == " " ->
                unique_name
                    |> String.dropLeft 5
                    |> String.trim
            | otherwise ->
                unique_name
                    |> String.trim


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
        map3
            makeRecord
            (String.toInt xml.num_bikes)
            (String.toInt xml.max_bikes)
            (makeLocation xml.lat xml.lng)


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


makeStationList : List StationXml -> Maybe Location -> List Station
makeStationList xmlList loc =
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
        urlSignal = Signal.map (always url) refreshSignal
    in
        handleResp <~ Http.sendGet urlSignal

initialState : State
initialState = {
        stationView = Nothing
    }

{-
main =
    Signal.map
        asText
        (Signal.map2
            (,)
            userLocation
            (Signal.map2 stations stationXmlIn userLocation))
-}

actionChannel : Signal.Channel Action
actionChannel = Signal.channel ViewList


refreshChannel : Signal.Channel ()
refreshChannel = Signal.channel ()


refreshSignal = Signal.subscribe refreshChannel

waitingForData : Signal Bool
waitingForData =
    Signal.merge
        (Signal.map (always True) refreshSignal)
        (Signal.map (always False) getBubiData)


updateState action oldState =
    case action of
        ViewMap uid ->
            { oldState | stationView <- Just uid }
        ViewList ->
            { oldState | stationView <- Nothing }


main =
    let state = Signal.foldp updateState initialState (Signal.subscribe actionChannel)
        stations = Signal.map2 makeStationList stationXmlIn userLocation
        updateTime = Signal.map (Date.fromTime << fst) (Time.timestamp stations)
        renderParams =
            (RenderParams actionChannel refreshChannel)
                <~ state
                ~ stations
                ~ userLocation
                ~ updateTime
                ~ waitingForData
                ~ flexSupported
    in
        Signal.map HtmlRender.render renderParams


type alias StationXml = {
        uid : String,
        lat : String,
        lng : String,
        unique_name : String,
        num_bikes : String,
        max_bikes : String
    }
