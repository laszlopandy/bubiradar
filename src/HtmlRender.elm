module HtmlRender where

import Graphics.Element (Element)   
import Html
import Html (Html)
import Html.Attributes
import Html.Attributes (class, src, height)
import String
import Date
import Date (Date)
import List
import Types (State, Meters)


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


classNameList : List (String, Bool) -> Html.Attribute
classNameList list =
    list
        |> List.filter snd
        |> List.map fst
        |> String.join " "
        |> Html.Attributes.class


renderHeader : State -> Html
renderHeader state =
    let refreshButton =
            Html.button
                [
                    classNameList [
                        ("refresh_button", True),
                        ("no-flex", not state.flexSupported)
                    ]
                    -- TODO: onClick
                ]
                [
                    Html.img
                        [
                            classNameList [
                                ("refresh_image", True),
                                ("spinning", state.waitingForData)
                            ],
                            src "assets/refresh.svg",
                            height 15
                        ]
                        [],
                    Html.text (makePrettyTime state.updateTime)
                ]
    in
        Html.div
            [ class "header_container" ]
            [ Html.div
                [ class "header" ]
                [
                    Html.text "Bubi Radar",
                    refreshButton
                ]
            ]


state : State
state = {
        stations = [],
        userLocation = Nothing,
        stationView = Nothing,
        updateTime = Date.fromTime 0,
        waitingForData = True,
        flexSupported = False
    }


renderHtml : Html
renderHtml =
    Html.div
        [ class "container" ]
        [ renderHeader state ]


render : Element
render = Html.toElement 400 600 renderHtml
