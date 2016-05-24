port module Main exposing (main)

import Date
import Geolocation
import Html exposing (Html)
import Html.App
import Http
import List
import Maybe
import Platform.Cmd exposing ((!), Cmd)
import Result
import String
import Task exposing (Task)
import Time

import HtmlRender
import Types exposing (Location, Station, StationXml, State, Meters, Uid, Action(..))

{- Inward ports -}
port stationXmlIn : (List StationXml -> msg) -> Sub msg
port windowDimensions : ((Int, Int) -> msg) -> Sub msg
{- Outward ports -}
port stationXmlOut : String -> Cmd msg


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
        if delim == "-" || delim == " " then
            unique_name
                |> String.dropLeft 5
                |> String.trim

        else
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
        distance = Just (calcDistance userLocation station.location)
    }


updateStationList : List Station -> Location -> List Station
updateStationList list userLocation =
    list
        |> List.map (updateStationDistance userLocation)
        |> List.sortBy (.distance >> Maybe.withDefault 0)

makeStationList : List StationXml -> Maybe Location -> List Station
makeStationList xmlList loc =
    let list =
            xmlList |> List.filterMap (makeStation >> Result.toMaybe)
    in
        case loc of
            Nothing ->
                list |> List.sortBy .name
            Just userLocation ->
                updateStationList list userLocation


getBubiData : Task Http.Error String
getBubiData =
    Http.getString "https://nextbike.net/maps/nextbike-live.xml?domains=mb"

--main =
--    let state = Signal.foldp updateState initialState actionMailbox.signal
--        stations = Signal.map2 makeStationList stationXmlIn userLocation
--        updateTime = Signal.map (Date.fromTime << fst) (Time.timestamp stations)
--        renderParams =
--            (RenderParams actionMailbox.address refreshMailbox.address)
--                <~ state
--                ~ stations
--                ~ userLocation
--                ~ updateTime
--                ~ waitingForData.signal
--                ~ flexSupported
--                ~ windowDimensions
--    in
--        Signal.map HtmlRender.render renderParams

init : Flags -> (State, Cmd Action)
init flags =
    {
        flexSupported = flags.flexSupported,
        stationView = Nothing,
        stations = [],
        userLocation = Nothing,
        updateTime = Nothing,
        waitingForData = True,
        windowDimensions = flags.windowDimensions
    } ! [
        Task.perform (\_ -> NoOp) BubiData getBubiData,
        Task.perform (\_ -> NoOp) UserLocation Geolocation.now
    ]

setUpdateTime : Cmd Action
setUpdateTime =
    Task.perform (\_ -> NoOp) UpdateTime Date.now

update : Action -> State -> (State, Cmd Action)
update action state =
    case action of
        NoOp ->
            state ! []

        ViewMap uid ->
            { state |
                stationView = Just uid
            } ! []
        ViewList ->
            { state |
                stationView = Nothing
            } ! []
        StationsData list ->
            { state |
                stations = makeStationList list state.userLocation
            } ! [
                setUpdateTime
            ]
        UserLocation geolocation ->
            let location = { lat = geolocation.latitude, lng = geolocation.longitude }
            in
                { state |
                    userLocation = Just location,
                    stations = updateStationList state.stations location
                } ! [
                    setUpdateTime
                ]
        UpdateTime date ->
            { state |
                updateTime = Just date
            } ! []

        Refresh ->
            { state |
                waitingForData = True
            } ! [
                Task.perform (\_ -> NoOp) BubiData getBubiData
            ]
        BubiData data ->
            { state |
                waitingForData = False
            } ! [
                stationXmlOut data
            ]


subscriptions : State -> Sub Action
subscriptions state =
    Sub.batch [
        stationXmlIn StationsData
    ]

main =
    Html.App.programWithFlags
        {
            init = init,
            update = update,
            subscriptions = subscriptions,
            view = HtmlRender.render
        }

type alias Flags = {
    flexSupported : Bool,
    windowDimensions : (Int, Int)
}
