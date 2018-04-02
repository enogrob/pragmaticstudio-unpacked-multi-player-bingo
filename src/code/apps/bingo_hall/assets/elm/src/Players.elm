module Players
    exposing
        ( PresenceState
        , initialPresences
        , syncDiff
        , syncState
        , viewPlayers
        )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder, field)
import Phoenix.Presence


type alias PresenceState =
    Phoenix.Presence.PresenceState PresenceMetaData


type alias PresenceDiff =
    Phoenix.Presence.PresenceDiff PresenceMetaData


type alias PresenceStateMetaWrapper =
    Phoenix.Presence.PresenceStateMetaWrapper PresenceMetaData


type alias PresenceMetaData =
    { color : String
    , online_at : String
    }


type alias PlayerPresence =
    { name : String
    , color : String
    , online_at : String
    , score : Int
    }


type alias Scores =
    Dict String Int


initialPresences : PresenceState
initialPresences =
    Dict.empty



-- SYNCING STATE


syncState : PresenceState -> Decode.Value -> Result String PresenceState
syncState presences payload =
    case decodePresenceState payload of
        Ok newState ->
            Ok (Phoenix.Presence.syncState newState presences)

        Err error ->
            Err error


syncDiff : PresenceState -> Decode.Value -> Result String PresenceState
syncDiff presences payload =
    case decodePresenceDiff payload of
        Ok diff ->
            Ok (Phoenix.Presence.syncDiff diff presences)

        Err error ->
            Err error



-- VIEW


viewPlayers : Scores -> PresenceState -> Html msg
viewPlayers scores presences =
    let
        playerPresences =
            toPlayerPresences scores presences
    in
    div [ class "panel panel-default" ]
        [ div [ class "panel-heading" ]
            [ text "Who's Playing" ]
        , div [ class "panel-body" ]
            [ ul [ id "players", class "list-group" ]
                (List.map viewPlayer playerPresences)
            ]
        ]


viewPlayer : PlayerPresence -> Html msg
viewPlayer player =
    li [ class "list-group-item" ]
        [ span
            [ class "player-color"
            , style [ ( "background-color", player.color ) ]
            ]
            [ text "" ]
        , span [ class "player-name" ] [ text player.name ]
        , span [ class "player-score" ] [ text (toString player.score) ]
        ]


toPlayerPresences : Scores -> PresenceState -> List PlayerPresence
toPlayerPresences scores presences =
    presences
        |> Dict.toList
        |> List.map (toPlayerPresence scores)


{-| Converts a player's first presence metadata to a `PlayerPresence`,
which includes the player's score fetched from the specified `scores`.

Here's an example `metaWrapper` record:

    { metas =
        [
            { phx_ref = "eKAtLgf+lM4="
            , payload = { color = "#a4deff", online_at = "1519945937" }
            }
        ]
    }

-}
toPlayerPresence : Scores -> ( String, PresenceStateMetaWrapper ) -> PlayerPresence
toPlayerPresence scores ( name, metaWrapper ) =
    case List.head metaWrapper.metas of
        Just metaData ->
            let
                color =
                    metaData.payload.color

                online_at =
                    metaData.payload.online_at

                score =
                    Maybe.withDefault 0 (Dict.get name scores)
            in
            PlayerPresence name color online_at score

        Nothing ->
            PlayerPresence name "" "" 0



-- DECODERS


decodePresenceState : Decode.Value -> Result String PresenceState
decodePresenceState payload =
    Decode.decodeValue
        (Phoenix.Presence.presenceStateDecoder presenceMetaDataDecoder)
        payload


decodePresenceDiff : Decode.Value -> Result String PresenceDiff
decodePresenceDiff payload =
    Decode.decodeValue
        (Phoenix.Presence.presenceDiffDecoder presenceMetaDataDecoder)
        payload


presenceMetaDataDecoder : Decoder PresenceMetaData
presenceMetaDataDecoder =
    Decode.map2 PresenceMetaData
        (field "color" Decode.string)
        (field "online_at" Decode.string)
