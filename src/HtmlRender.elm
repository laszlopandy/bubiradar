module HtmlRender where

import Graphics.Element (Element)   
import Html
import Html (Html)
import Html.Attributes (class)
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


renderHtml : Html
renderHtml =
    Html.div
        [ class "test" ]
        []


render : Element
render = Html.toElement 400 600 renderHtml
