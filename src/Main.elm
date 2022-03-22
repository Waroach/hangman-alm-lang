module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Set exposing (Set)



-- https://www.youtube.com/watch?v=BktE_8qYXqI&t=1886s&ab_channel=MichaelJones
---- MODEL ----
-- STATE


type Model
    = Loading
    | Running GameState
    | Error


type alias GameState =
    { phrase : String
    , guesses : Set String
    }



-- initial version of State
-- only used on page load or refresh. Otherwise ignored


init : ( Model, Cmd Msg )
init =
    ( Loading
    , fetchWord
    )



---- UPDATE ----


type Msg
    = Guess String
    | Restart
    | NewPhrase (Result Http.Error String)



-- change the state update it


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess char ->
            case model of
                Running gameState ->
                    ( Running { gameState | guesses = Set.insert char gameState.guesses }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Restart ->
            ( Loading, fetchWord )

        NewPhrase result ->
            case result of
                Ok phrase ->
                    ( Running { phrase = phrase, guesses = Set.empty }, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )


fetchWord : Cmd Msg
fetchWord =
    Http.get
        { url = "https://snapdragon-fox.glitch.me/word"
        , expect = Http.expectJson NewPhrase wordDecoder
        }

testTwofetchWord : Cmd Msg
testTwofetchWord =
    Http.get
        { url = "https://words.devonzara.com/api"
        , expect = Http.expectJson NewPhrase wordDecoder
        }

testOnefetchWord : Cmd Msg
testOnefetchWord =
    Http.get
        { url = "https://random-words-api.vercel.app/word"
        , expect = Http.expectJson NewPhrase wordDecoder
        }


{--}
wordDecoder : Decoder String
wordDecoder =
    Decode.field "word" Decode.string
--}



-- https://github.com/andrewsuzuki/elm-todo-rest-api/blob/master/src/Todos/Commands.elm
{--
wordDecoder : Decoder String
wordDecoder =
    field "name" (field "name" string)
--}
---- VIEW ----
-- takes model and creates Virtual DOM


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [ class "clicked" ] [ text "LOADING" ]

        Running gameState ->
            viewGameState gameState

        Error ->
            div [ class "clicked" ] [ text "ERROR" ]


viewGameState gameState =
    let
        phraseHtml =
            gameState.phrase
                |> String.toUpper
                |> String.split ""
                |> List.map
                    (\char ->
                        if char == " " then
                            "-"

                        else if Set.member char gameState.guesses then
                            char

                        else
                            "_"
                    )
                |> List.map
                    (\char ->
                        if char == "-" then
                            span [ class "space" ] [ text char ]

                        else
                            span [ class "char" ] [ text char ]
                    )
                |> div []

        phraseSet =
            gameState.phrase
                |> String.toUpper
                |> String.split ""
                |> Set.fromList

        failuresHtml =
            gameState.guesses
                |> Set.toList
                |> List.filter (\char -> not <| Set.member char phraseSet)
                |> List.map (\char -> span [] [ text char ])
                |> div []

        buttonsHtml =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                |> String.split ""
                |> List.map
                    (\char ->
                        -- Create a IF statment to gray out button if already clicked
                        if Set.member char phraseSet && Set.member char gameState.guesses then
                            button [ class "button correct" ] [ text char ]

                        else if Set.member char gameState.guesses then
                            button [ class "button fail" ] [ text char ]

                        else
                            button [ class "button Clicked", onClick <| Guess char ] [ text char ]
                    )
                |> div []

        _ =
            Debug.log "guesses" gameState.phrase
    in
    div []
        [ phraseHtml
        , buttonsHtml
        , failuresHtml
        , button [ onClick Restart ] [ text "Restart" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
