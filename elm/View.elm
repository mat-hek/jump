module View exposing (view)

import Actions exposing (Action)
import Collage exposing (..)
import Color exposing (rgb)
import Element exposing (..)
import Html exposing (Html)
import Model exposing (Model, Hero, Enemy, State)

view : Model -> Html Action
view model =
    let
        ( w, h ) =
            ( toFloat model.windowSize.width
            , toFloat model.windowSize.height
            )

        groundY = 380 - h / 2

    in
        collage
            model.windowSize.width
            model.windowSize.height
            [ renderSky w h
            , renderGrass w 250 ( 125 - h / 2)
            , renderHero model.hero groundY
            ]
        |> Element.toHtml

renderSky : Float -> Float -> Form
renderSky w h =
    rect w h
    |> filled ( rgb 174 238 238 )

renderGrass : Float -> Float -> Float -> Form
renderGrass w h offset =
    rect w h
    |> filled ( rgb 74 167 43 )
    |> move ( 0, offset )

renderHero : Hero -> Float -> Form
renderHero hero offset =
    let
        action =
            if hero.y > 0 then
                "jump"
            else if hero.vx > 0 then
                "move"
            else
                "stand"

        heroImgSrc =
            "/resources/" ++ action ++ "_right.gif"

        heroImg =
            image hero.size.x hero.size.y heroImgSrc

    in
        heroImg
        |> toForm
        |> move ( hero.x, hero.y + offset )

renderEnemies : List Enemy -> List Form
renderEnemies enemies =
    []

renderTitle : State -> Maybe Form
renderTitle state =
    Nothing