module Main exposing (GifResult(..), Model, Msg(..), getRandomGif, gifDecoder, init, main, subscriptions, update, view, viewGif, when)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)


type GifResult
    = Failure
    | Loading
    | Success String


type alias Model =
    { searchTerm : String
    , gif : Maybe GifResult
    }


type Msg
    = LoadGif
    | UpdateSearchTerm String
    | GiphyResponded (Result Http.Error String)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchTerm = "", gif = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadGif ->
            ( { model | gif = Just Loading }, getRandomGif model.searchTerm )

        UpdateSearchTerm searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        GiphyResponded result ->
            case result of
                Ok url ->
                    ( { model | gif = Just (Success url) }, Cmd.none )

                Err _ ->
                    ( { model | gif = Just Failure }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Random Gifs" ]
        , input [ placeholder "Search", value model.searchTerm, onInput UpdateSearchTerm ] []
        , button [ onClick LoadGif ] [ text "Search" ]
        , when model.gif viewGif
        ]


viewGif : GifResult -> Html Msg
viewGif gif =
    case gif of
        Failure ->
            text "I could not load a random gif for some reason."

        Loading ->
            text "Loading..."

        Success url ->
            div [] [ img [ src url ] [] ]


getRandomGif : String -> Cmd Msg
getRandomGif searchTerm =
    Http.get
        { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ searchTerm
        , expect = Http.expectJson GiphyResponded gifDecoder
        }


gifDecoder : Decoder String
gifDecoder =
    field "data" (field "image_url" string)


when : Maybe a -> (a -> Html Msg) -> Html Msg
when maybe f =
    case maybe of
        Nothing ->
            text ""

        Just a ->
            f a
