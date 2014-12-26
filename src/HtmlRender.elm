module HtmlRender where

import Date
import Date (Date)
import Graphics.Element (Element)   
import Html
import Html (Html)
import Html.Attributes
import Html.Attributes (class, src, height, href)
import List
import String

import Types (Station, RenderParams, Meters)


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


refreshButton state =
    Html.button
        [
            classList [
                ("refresh_button", True),
                ("no-flex", not state.flexSupported)
            ]
            -- TODO: onClick
        ]
        [
            Html.img
                [
                    classList [
                        ("refresh_image", True),
                        ("spinning", state.waitingForData)
                    ],
                    src "assets/refresh.svg",
                    height 15
                ]
                [],
            Html.text (makePrettyTime state.updateTime)
        ]


renderHeader state =
    Html.div
        [ class "header_container" ]
        [ Html.div
            [ class "header" ]
            [
                Html.text "Bubi Radar",
                refreshButton state
            ]
        ]


renderStation : Bool -> Station -> Html
renderStation flexSupported station =
    let backgroundDiv =
            Html.div
                [
                    classList [
                            ("list_item_container", True),
                            ("list_item_no_bikes", station.num_bikes == 0),
                            ("list_item_few_bikes", station.num_bikes <= 3 && station.num_bikes > 0)
                        ]
                    -- TODO: onClick setStationView station.uid
                ]
                []

        leftDiv =
            case station.distance of
                Nothing ->
                    Html.span [ class "station_name" ] [ Html.text station.name ]
                Just dist ->
                    Html.div
                        [ classList [
                                ("name_group", True),
                                ("no-flex", not flexSupported)
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
                            ("no-flex", not flexSupported)
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
                backgroundDiv,
                leftDiv,
                rightDiv
            ]


renderStations state =
    Html.ul
        [ class "station_list" ]
        (List.map (renderStation state.flexSupported) state.stations)


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


renderHtml state =
    Html.div
        [ class "container" ]
        [
            renderHeader state,
            renderStations state,
            renderAboutLink
        ]


render : RenderParams -> Element
render state =
    Html.toElement 400 600 <|
        if List.isEmpty state.stations
            then renderSpinner
            else renderHtml state
