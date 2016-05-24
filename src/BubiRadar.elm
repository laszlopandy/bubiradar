port module Main exposing (main)

import Date exposing (Date)
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

port stationXmlIn : (List StationXml -> msg) -> Sub msg
port stationXmlOut : String -> Cmd msg


makeLocation : String -> String -> Result String Location
makeLocation lat lng =
    Result.map2
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
        Result.map3
            makeRecord
            (String.toInt xml.num_bikes)
            (String.toInt xml.max_bikes)
            (makeLocation xml.lat xml.lng)


calcDistance : Location -> Location -> Meters
calcDistance a b =
    let aLat = cos (degrees a.lat)
        bLat = cos (degrees b.lat)
        dLat = sin <| (degrees (b.lat - a.lat)) / 2
        dLng = sin <| (degrees (b.lng - a.lng)) / 2
        x = (dLat * dLat) + (aLat * bLat * dLng * dLng)
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


getBubiData : Task Http.Error (String, Date)
getBubiData =
    Task.map2
        (,)
        (Http.getString "https://nextbike.net/maps/nextbike-live.xml?domains=mb")
        Date.now

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
        Task.perform (\_ -> NoOp) UserLocation getUserLocationWithDate
    ]

getUserLocationWithDate : Task Geolocation.Error (Geolocation.Location, Date)
getUserLocationWithDate =
    Task.map2
        (,)
        Geolocation.now
        Date.now

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
            } ! []
        UserLocation (geolocation, date) ->
            let location = { lat = geolocation.latitude, lng = geolocation.longitude }
            in
                { state |
                    userLocation = Just location,
                    updateTime = Just date,
                    stations = updateStationList state.stations location
                } ! []

        Refresh ->
            { state |
                waitingForData = True
            } ! [
                Task.perform (\_ -> NoOp) BubiData getBubiData
            ]
        BubiData (data, date) ->
            { state |
                waitingForData = False,
                updateTime = Just date
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
