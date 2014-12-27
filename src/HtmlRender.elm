module HtmlRender where

import Date
import Date (Date)
import Graphics.Element (Element)   
import Html
import Html (Html)
import Html.Attributes
import Html.Attributes (class, height, href, seamless, src, width)
import Html.Events (onClick)
import List
import Maybe
import Signal
import String

import Types (Action(..), Meters, RenderParams, Station, Uid)


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
            toFixed -1 dist ++ "m"
        | otherwise ->
            let kilos = dist / 1000
                decimalPlaces = if kilos < 10.0 then 1 else 0
            in
                toFixed decimalPlaces kilos ++ "km"


makePrettyTime : Date -> String
makePrettyTime t =
    let padded m = String.padLeft 2 '0' (toString m)
    in
        toString (Date.hour t) ++ ":" ++ padded (Date.minute t)


classList : List (String, Bool) -> Html.Attribute
classList list =
    list
        |> List.filter snd
        |> List.map fst
        |> String.join " "
        |> Html.Attributes.class


listFirstOf : (a -> Bool) -> List a -> Maybe a
listFirstOf pred list =
    case list of
        x :: xs ->
            if pred x
                then Just x
                else listFirstOf pred xs
        [] ->
            Nothing


getStationFromUid : Maybe Uid -> RenderParams -> Maybe Station
getStationFromUid maybeUid params =
    let findMatching uid station =
            station.uid == uid
        filterUid uid =
            listFirstOf (findMatching uid) params.stations
    in
        maybeUid `Maybe.andThen` filterUid


refreshButton params =
    Html.button
        [
            classList [
                ("refresh_button", True),
                ("no-flex", not params.flexSupported)
            ],
            onClick (Signal.send params.refreshChannel ())
        ]
        [
            Html.img
                [
                    classList [
                        ("refresh_image", True),
                        ("spinning", params.waitingForData)
                    ],
                    src "assets/refresh.svg",
                    height 15
                ]
                [],
            Html.text (makePrettyTime params.updateTime)
        ]


renderHeader params =
    Html.div
        [ class "header_container" ]
        [ Html.div
            [ class "header" ]
            [
                Html.text "Bubi Radar",
                refreshButton params
            ]
        ]


renderStation : RenderParams -> Station -> Html
renderStation params station =
    let containerClasses =
            classList [
                    ("list_item_container", True),
                    ("list_item_no_bikes", station.num_bikes == 0),
                    ("list_item_few_bikes", station.num_bikes <= 3 && station.num_bikes > 0)
                ]

        leftDiv =
            case station.distance of
                Nothing ->
                    Html.span [ class "station_name" ] [ Html.text station.name ]
                Just dist ->
                    Html.div
                        [ classList [
                                ("name_group", True),
                                ("no-flex", not params.flexSupported)
                            ]
                        ]
                        [
                            Html.div [ class "station_name" ] [ Html.text station.name ],
                            Html.div [ class "station_dist" ] [ Html.text (makePrettyDistance dist) ]
                        ]

        rightDiv =
            Html.span
                [
                    classList [
                            ("num_bikes", True),
                            ("no-flex", not params.flexSupported)
                        ]
                ]
                [
                    Html.text (toString station.num_bikes),
                    Html.img
                        [
                            class "bike_image",
                            src "assets/bike.svg"
                        ]
                        []
                ]
    in
        Html.li
            [ class "list_item" ]
            [
                Html.div
                    [ 
                        containerClasses,
                        onClick (Signal.send params.actionChannel (ViewMap station.uid))
                    ]
                    [ leftDiv, rightDiv ]
            ]


renderStations params =
    Html.ul
        [ class "station_list" ]
        (List.map (renderStation params) params.stations)


renderSpinner : Html
renderSpinner =
    Html.div
        [ class "spinner" ]
        [
            Html.div [ class "bounce1" ] [],
            Html.div [ class "bounce2" ] [],
            Html.div [ class "bounce3" ] []
        ]


renderAboutLink =
    let aboutLink = "https://github.com/laszlopandy/bubiradar/"
    in
        Html.div
            [ class "about_container" ]
            [ Html.a
                [ href aboutLink ]
                [ Html.text aboutLink ]
            ]


renderStationList params =
    Html.div
        [ class "container" ]
        [
            renderHeader params,
            renderStations params,
            renderAboutLink
        ]


renderStationView params station =
    let locationToString l = toString l.lat ++ "," ++ toString l.lng
        mapOrigin = Maybe.map locationToString params.userLocation |> Maybe.withDefault "Budapest"
        mapDest = locationToString station.location
        iframeUrl =
            "https://www.google.com/maps/embed/v1/directions" ++
                "?key=AIzaSyA1ko9DhkUQvO2N7mIwroQqFJ8ax1aRP6g" ++
                "&origin=" ++ mapOrigin ++
                "&destination=" ++ mapDest ++
                "&mode=walking"
        backButton =
            Html.div
                [ class "back_button_container" ]
                [
                    Html.button
                        [
                            class "back_button",
                            onClick (Signal.send params.actionChannel ViewList)
                        ]
                        [],
                    Html.img
                        [
                            src "assets/back-arrow.svg",
                            height 40
                        ]
                        []
                ]
        (windowWidth, windowHeight) = params.windowDimensions
        mapIFrame =
            Html.iframe
                [
                    width (min 400 windowWidth),
                    height windowHeight,
                    seamless True,
                    src iframeUrl
                ]
                []

    in
        Html.div
            [ class "container" ]
            [
                backButton,
                mapIFrame
            ]


renderHtml params =
    case getStationFromUid params.state.stationView params of
        Nothing ->
            renderStationList params
        Just station ->
            renderStationView params station


render : RenderParams -> Element
render params =
    Html.toElement 400 600 <|
        if List.isEmpty params.stations
            then renderSpinner
            else renderHtml params
