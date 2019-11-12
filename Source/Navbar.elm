module Navbar exposing (..)

import Html exposing (..)
import Bootstrap.Navbar as Navbar

menuView : Model -> Html Msg
menuView model =
    Navbar.config NavMsg
        |> Navbar.dark
        |> Navbar.brand
            [ Attrs.href "#", Attrs.style "font-size" "2rem" ]
            [ text "Sweaty Bacon Ducks" ]
        |> Navbar.items []
        |> Navbar.view model.navState
