import Date
import Http
import List
import Maybe
import Result
import Signal exposing ((<~), (~), Signal)
import String
import Task exposing (Task)
import Time
import Text

import HtmlRender
import Types exposing (Location, Station, RenderParams, State, Meters, Uid, Action(..))

{- Inward ports -}
port flexSupported : Signal Bool
port userLocation : Signal (Maybe Location)
port stationXmlIn : Signal (List StationXml)
port windowDimensions : Signal (Int, Int)
{- Outward ports -}
port stationXmlOut : Signal (Maybe String)
port stationXmlOut = bubiDataMailbox.signal

port userLocationRequest : Signal ()
port userLocationRequest = Signal.constant ()

{- Task executing ports -}
port execGetBubiData : Signal (Task () ())
port execGetBubiData = Signal.map (always getBubiData) refreshMailbox.signal


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


getBubiData : Task () ()
getBubiData =
    let url = "https://nextbike.net/maps/nextbike-live.xml?domains=mb"
    in
        Signal.send waitingForData.address True
            |> task_andThen (\_ -> Http.getString url)
            |> Task.toMaybe
            |> task_andThen (\data -> Signal.send bubiDataMailbox.address data)
            |> task_andThen (\_ -> Signal.send waitingForData.address False)

task_andThen a f = Task.andThen f a

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

bubiDataMailbox : Signal.Mailbox (Maybe String)
bubiDataMailbox =
    Signal.mailbox Nothing


actionMailbox : Signal.Mailbox Action
actionMailbox =
    Signal.mailbox ViewList


refreshMailbox : Signal.Mailbox ()
refreshMailbox =
    Signal.mailbox ()


waitingForData : Signal.Mailbox Bool
waitingForData =
    Signal.mailbox False


updateState action oldState =
    case action of
        ViewMap uid ->
            { oldState | stationView <- Just uid }
        ViewList ->
            { oldState | stationView <- Nothing }


main =
    let state = Signal.foldp updateState initialState actionMailbox.signal
        stations = Signal.map2 makeStationList stationXmlIn userLocation
        updateTime = Signal.map (Date.fromTime << fst) (Time.timestamp stations)
        renderParams =
            (RenderParams actionMailbox.address refreshMailbox.address)
                <~ state
                ~ stations
                ~ userLocation
                ~ updateTime
                ~ waitingForData.signal
                ~ flexSupported
                ~ windowDimensions
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
