module Bingo exposing (..)

import Chat
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder, field)
import Json.Encode as Encode
import Phoenix.Channel exposing (Channel)
import Phoenix.Push
import Phoenix.Socket exposing (Socket)
import Players


-- MODEL


type alias Model =
    { channelTopic : String
    , gameSummary : Game.GameSummary
    , chatMessageInput : String
    , chatMessages : List Chat.ChatMessage
    , phxSocket : Socket Msg
    , presences : Players.PresenceState
    , error : Maybe String
    }


initialModel : String -> Socket Msg -> Model
initialModel channelTopic socket =
    { channelTopic = channelTopic
    , gameSummary = Game.initialSummary
    , chatMessageInput = ""
    , chatMessages = []
    , phxSocket = socket
    , presences = Players.initialPresences
    , error = Nothing
    }



-- UPDATE


type Msg
    = NoOp
    | ReceiveGameSummary Decode.Value
    | SendMark String
    | SetChatMessageInput String
    | SendChatMessage
    | ReceiveChatMessage Decode.Value
    | ReceivePresenceState Decode.Value
    | ReceivePresenceDiff Decode.Value
    | ReceiveError Decode.Value
    | DelegatedPhxMsg (Phoenix.Socket.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceiveGameSummary payload ->
            case Game.decodeGameSummary payload of
                Ok gameSummary ->
                    ( { model | gameSummary = gameSummary }, Cmd.none )

                Err error ->
                    ( setError error model, Cmd.none )

        SendMark phrase ->
            let
                payload =
                    Encode.object [ ( "phrase", Encode.string phrase ) ]

                pushMsg =
                    Phoenix.Push.init "mark_square" model.channelTopic
                        |> Phoenix.Push.withPayload payload
                        |> Phoenix.Push.onError ReceiveError

                ( newSocket, phxCmd ) =
                    Phoenix.Socket.push pushMsg model.phxSocket
            in
            ( { model | phxSocket = newSocket }
            , Cmd.map DelegatedPhxMsg phxCmd
            )

        SetChatMessageInput message ->
            ( { model | chatMessageInput = message }, Cmd.none )

        SendChatMessage ->
            let
                payload =
                    Chat.encodeChatMessage model.chatMessageInput

                pushMsg =
                    Phoenix.Push.init "new_chat_message" model.channelTopic
                        |> Phoenix.Push.withPayload payload
                        |> Phoenix.Push.onError ReceiveError

                ( newSocket, phxCmd ) =
                    Phoenix.Socket.push pushMsg model.phxSocket
            in
            ( { model | chatMessageInput = "", phxSocket = newSocket }
            , Cmd.map DelegatedPhxMsg phxCmd
            )

        ReceiveChatMessage payload ->
            case Chat.decodeChatMessage payload of
                Ok message ->
                    ( { model | chatMessages = message :: model.chatMessages }
                    , Chat.scrollToMessage NoOp
                    )

                Err error ->
                    ( setError error model, Cmd.none )

        ReceivePresenceState payload ->
            case Players.syncState model.presences payload of
                Ok presences ->
                    ( { model | presences = presences }, Cmd.none )

                Err error ->
                    ( setError error model, Cmd.none )

        ReceivePresenceDiff payload ->
            case Players.syncDiff model.presences payload of
                Ok presences ->
                    ( { model | presences = presences }, Cmd.none )

                Err error ->
                    ( setError error model, Cmd.none )

        ReceiveError payload ->
            case Decode.decodeValue errorDecoder payload of
                Ok message ->
                    ( { model | error = Just message }, Cmd.none )

                Err error ->
                    ( setError error model, Cmd.none )

        {- Delegates handling of `ChannelJoined`, `ChannelClosed`, 
        `ChannelErrored` and `Heartbeat` messages to the 
        `Phoenix.Socket.update` function
        -}
        DelegatedPhxMsg msg ->
            let
                ( newSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
            ( { model | phxSocket = newSocket }
            , Cmd.map DelegatedPhxMsg phxCmd
            )


setError : String -> Model -> Model
setError error model =
    { model | error = Just (toString error) }


errorDecoder : Decoder String
errorDecoder =
    field "reason" Decode.string



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "content" ]
        [ viewErrorMaybe model
        , viewBingo model
        ]


viewErrorMaybe : Model -> Html Msg
viewErrorMaybe model =
    case model.error of
        Just message ->
            p [ class "alert alert-danger" ]
                [ text message ]

        Nothing ->
            text ""


viewBingo : Model -> Html Msg
viewBingo model =
    div [ class "row" ]
        [ div [ class "col-xs-8" ]
            [ Game.viewGame SendMark model.gameSummary ]
        , div [ class "col-xs-4" ]
            [ Players.viewPlayers
                model.gameSummary.scores
                model.presences
            , Chat.viewChatMessages
                model.chatMessages
            , Chat.viewChatMessageForm
                SendChatMessage
                SetChatMessageInput
                model.chatMessageInput
            ]
        ]



-- SUBSCRIPTIONS


{-| Listens for messages. 

Messages such as `ChannelJoined`, `ChannelClosed`, `ChannelErrored`, and `Heartbeat` are tagged with `DelegatedPhxMsg` and fed into the `Phoenix.Socket.update` function.

Messages produced by an event handler such as `ReceiveGameSummary`, 
`ReceiveChatMessage`, `ReceivePresenceState`, `ReceivePresenceDiff`,
and `ReceiveError` are NOT tagged. These messages are fed into the `update` function, just like the messages produced by a button click.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket DelegatedPhxMsg



-- PHOENIX SOCKET AND CHANNEL INITIALIZATION


initPhoenixSocket : String -> String -> Socket Msg
initPhoenixSocket socketUrl topic =
    Phoenix.Socket.init socketUrl
        |> Phoenix.Socket.on "game_summary" topic ReceiveGameSummary
        |> Phoenix.Socket.on "new_chat_message" topic ReceiveChatMessage
        |> Phoenix.Socket.on "presence_state" topic ReceivePresenceState
        |> Phoenix.Socket.on "presence_diff" topic ReceivePresenceDiff
        |> Phoenix.Socket.withoutHeartbeat
        |> Phoenix.Socket.withDebug


initPhoenixChannel : String -> Channel Msg
initPhoenixChannel topic =
    Phoenix.Channel.init topic
        |> Phoenix.Channel.onError ReceiveError
        |> Phoenix.Channel.onJoinError ReceiveError


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        socketUrl =
            flags.wsUrl ++ "?token=" ++ flags.authToken

        channelTopic =
            "games:" ++ flags.gameName

        socket =
            initPhoenixSocket socketUrl channelTopic

        channel =
            initPhoenixChannel channelTopic

        ( newSocket, phxCmd ) =
            Phoenix.Socket.join channel socket

        model =
            initialModel channelTopic newSocket
    in
    ( model, Cmd.map DelegatedPhxMsg phxCmd )
    -- runs a command that produces `DelegatedPhxMsg ChannelJoined`


-- MAIN


type alias Flags =
    { gameName : String
    , authToken : String
    , wsUrl : String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
