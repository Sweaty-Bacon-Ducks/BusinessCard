module Main exposing (..)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Grid as Grid
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)


type alias Flags =
    {}


type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    }


type Page
    = Home
    | AboutUs
    | Projects
    | TeamMembers
    | NotFound


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = Home, modalVisibility = Modal.hidden }
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }
            , Cmd.none
            )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map AboutUs (UrlParser.s "about-us")
        , UrlParser.map Projects (UrlParser.s "projects")
        , UrlParser.map TeamMembers (UrlParser.s "our-members")
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Sweaty Bacon Ducks"
    , body =
        [ Html.div
            [ Attrs.style "font-family" "VT323"
            ]
            [ menuView model
            , mainContentView model
            , footerView
            ]
        ]
    }


menuView : Model -> Html Msg
menuView model =
    Navbar.config NavMsg
        |> Navbar.dark
        |> Navbar.brand
            [ Attrs.href "#", Attrs.style "font-size" "2rem" ]
            [ text "Sweaty Bacon Ducks" ]
        |> Navbar.items []
        |> Navbar.view model.navState


mainContentView : Model -> Html Msg
mainContentView model =
    Grid.container [ Attrs.style "padding-top" "40vh" ] <|
        case model.page of
            Home ->
                homePage

            AboutUs ->
                aboutUsPage model

            Projects ->
                projectsPage model

            TeamMembers ->
                contactPage model

            NotFound ->
                notFoundPage


homePage : List (Html Msg)
homePage =
    [ Grid.row []
        [ Grid.col []
            (cardFactory "AboutUs" "static/about-us-icon.png")
        , Grid.col []
            (cardFactory "Projects" "static/projects-icon.png")
        , Grid.col []
            (cardFactory "Contact" "static/contact-icon.png")
        ]
    ]


cardFactory : String -> String -> List (Html msg)
cardFactory title iconPath =
    [ Card.config [ Card.align Text.alignXsCenter ]
        |> Card.block []
            [ CardBlock.titleH3 [] [ text title ]
            , CardBlock.custom
                (img
                    [ Attrs.src iconPath
                    , Attrs.style "height" "auto"
                    , Attrs.style "width" "50%"
                    ]
                    []
                )
            ]
        |> Card.view
    ]


aboutUsCardHeaderContent : List (Html msg)
aboutUsCardHeaderContent =
    [ img
        [ Attrs.src "static/user.png"
        , Attrs.style "height" "auto"
        , Attrs.style "width" "40%"
        ]
        []
    ]


aboutUsCardBlockContent : List (CardBlock.Item msg)
aboutUsCardBlockContent =
    [ CardBlock.titleH3 [] [ text "AboutUs" ]
    , CardBlock.custom
        (img
            [ Attrs.src "static/user.png"
            , Attrs.style "height" "auto"
            , Attrs.style "width" "50%"
            ]
            []
        )
    ]


projectsCardContent : List (CardBlock.Item msg)
projectsCardContent =
    [ CardBlock.titleH3 [] [ text "Projects" ]
    ]


teamMembersContent : List (CardBlock.Item msg)
teamMembersContent =
    [ CardBlock.titleH3 [] [ text "Our Team" ]
    ]


aboutUsPage : Model -> List (Html Msg)
aboutUsPage model =
    [ Html.h1 [] [ text "AboutUs" ] ]


projectsPage : Model -> List (Html Msg)
projectsPage model =
    [ Html.h1 [] [ text "Projects" ] ]


contactPage : Model -> List (Html Msg)
contactPage model =
    [ Html.h1 [] [ text "TeamMembers" ] ]


notFoundPage : List (Html Msg)
notFoundPage =
    [ Html.h1 [] [ text "TeamMembers" ] ]


footerView : Html Msg
footerView =
    Html.footer
        [ Attrs.class "container-fluid"
        , Attrs.class "fixed-bottom"
        , Attrs.style "color" "white"
        , Attrs.style "background-color" "gray"
        ]
        [ Html.h5 [] [ text "All rights reserved (2019)" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }
