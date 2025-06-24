module Main exposing (main)

import Colors
import MarkdownText exposing (MarkdownText(..))
import Style
import Widgets exposing (MarkdownMsg(..))

import Browser
import Browser.Navigation as Navigation
import Element as UI
import Element.Font as UI_Font
import Json.Decode
import Json.Encode
import Http
import List.Extra
import Platform.Cmd as Cmd
import Platform.Cmd as Cmd
import Url exposing (Url)
import Url.Parser exposing ((</>))

main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view >> Widgets.layout
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = Msg_UrlRequest
        , onUrlChange = Msg_UrlChange
        }

type Msg
    = Msg_Noop
    | Msg_UrlRequest Browser.UrlRequest
    | Msg_UrlChange Url
    | Msg_FileContent String
    | Msg_Error String
    | Msg_Run { language : String, code : String }

type State
    = State_Loading { title : String, path : String }
    | State_Loaded { title : String, path : String, body : MarkdownText }
    | State_NotFound

type alias Model = 
    { navigation_key : Navigation.Key
    , token : String
    , state : State
    }

init : Json.Decode.Value -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags url key =
    let
        token = flags
            |> Json.Decode.decodeValue (Json.Decode.field "token" Json.Decode.string) 
            |> Result.withDefault "0"

        parser = Url.Parser.oneOf
            [ Url.Parser.s "file" </> Url.Parser.string
            ]

        parsed_file = Url.Parser.parse parser url
    in
        case parsed_file of
            Just encoded_path ->
                let
                    path = Url.percentDecode encoded_path |> Maybe.withDefault encoded_path
                in
                    (
                        { navigation_key = key
                        , token = token
                        , state = State_Loading 
                            { title = path_filename path
                            , path = path
                            }
                        }
                        , Http.get { url = "/api/file/" ++ encoded_path, expect = Http.expectString (\result -> case result of
                            Ok content -> Msg_FileContent content
                            Err _ -> Msg_Error "Something went wrong"
                        ) }
                    )
            Nothing ->
                (
                    { navigation_key = key
                    , token = token
                    , state = State_NotFound
                    }
                    , Cmd.none
                )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Msg_Noop -> (model, Cmd.none)
    Msg_UrlRequest request -> case request of
        Browser.Internal url -> (model, Navigation.pushUrl model.navigation_key (Url.toString url))
        Browser.External url -> (model, Navigation.load url)
    Msg_UrlChange url -> init (Json.Encode.object [ ("token", Json.Encode.string model.token) ]) url model.navigation_key
    Msg_FileContent content -> ({ model | state = complete_state model.state content }, Cmd.none)
    Msg_Error _ -> (model, Cmd.none)
    Msg_Run run -> case model.state of
        State_Loaded state -> 
            ( model
            , Http.post 
                { url = "/api/run"
                , body = Http.jsonBody <| Json.Encode.object 
                    [ ("token", Json.Encode.string model.token)
                    , ("path", Json.Encode.string state.path)
                    , ("language", Json.Encode.string run.language)
                    , ("code", Json.Encode.string run.code)
                    ]
                , expect = Http.expectWhatever 
                    (\result -> case result of
                        Ok _ -> Msg_Noop
                        Err _ -> Msg_Error "TODO: Error to string"
                    )
                }
            )
        _ -> (model, Cmd.none)

view : Model -> (String, UI.Element Msg)
view model = case model.state of
    State_Loading _ -> ("Loading...", UI.none)
    State_Loaded state -> 
        ( state.title
        , view_content state
        )
    State_NotFound -> ("Not found", UI.none)

complete_state : State -> String -> State
complete_state prev_state content = case prev_state of
    State_Loading state -> State_Loaded { title = state.title, path = state.path, body = MarkdownText content }
    State_Loaded state -> State_Loaded { title = state.title, path = state.path, body = MarkdownText content }
    State_NotFound -> State_NotFound


path_filename : String -> String
path_filename path = path
    |> String.split "/"
    |> List.Extra.last
    |> Maybe.withDefault path
    |> String.split "\\"
    |> List.Extra.last
    |> Maybe.withDefault path

view_content : { title : String, path : String, body : MarkdownText } -> UI.Element Msg
view_content state = UI.column
    [ UI.spacing Style.spacingBeetweenParagraphs
    , UI.width UI.fill
    ]
    ( 
        [ Widgets.complexHeading [] 1 "" [ UI.text state.title ]
        , UI.paragraph [ UI_Font.italic, UI_Font.color Colors.dateText, UI_Font.size Style.smallFontSize ] [ UI.text state.path ]
        ] ++ (Widgets.markdownBody state.body)
    )
    |> UI.map (\msg -> case msg of
        MarkdownMsg_Run code -> Msg_Run code
    )
