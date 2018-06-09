port module Main exposing (..)

import Date
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline as Pipe exposing (decode, hardcoded, optional, required)
import Json.Encode
import List.Extra
import QRCode
import RemoteData exposing (..)
import Set exposing (Set)


port openWindow : String -> Cmd msg


port saveData : String -> Cmd msg



---- MODEL ----


type alias Model =
    { user : Maybe UserData
    , events : List Event
    , pastActions : List ActionTaken
    , currentView : View
    , currentEvent : Maybe Event
    , menuOpen : Bool
    , dataUrl : String
    , destinationEmail : String
    , selectedTopics : Set String
    }


type View
    = TopicList
    | TopicSent
    | ContactDisplay
    | ContactEdit
    | EventSelector
    | DataImporter
    | Loading
    | Error


type alias UserData =
    { name : String
    , email : String
    , site : String
    , phone : String
    , socialNetworks : List ( String, String ) -- decoder list <| map2 (,) (index 0 string) (index 1 string)
    }


type alias Event =
    { name : String
    , dateStart : Int
    , dateEnd : Int
    , topics : Dict String String
    }


type alias UserAndEvents =
    { user : UserData
    , events : List Event
    }


type Action
    = ActionTopicSent String String
    | ActionContactShown


type alias ActionTaken =
    { date : Int
    , action : Action
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        dataUrl =
            "/data.json"

        hasFlags =
            Json.Decode.decodeValue decodeUserAndEvents flags

        ( user, currentEvent, currentView, nextCmd ) =
            case hasFlags of
                Err x ->
                    let
                        d =
                            Debug.log "No saved data" x
                    in
                    ( Nothing, Nothing, Loading, getData dataUrl )

                Ok f ->
                    let
                        d =
                            Debug.log "Saved data" f
                    in
                    ( Just f.user, List.Extra.getAt 0 f.events, TopicList, Cmd.none )
    in
    ( { user = user
      , events = []
      , pastActions = []
      , currentView = currentView
      , currentEvent = currentEvent
      , menuOpen = False
      , dataUrl = dataUrl
      , destinationEmail = ""
      , selectedTopics = Set.empty
      }
    , nextCmd
    )


getData : String -> Cmd Msg
getData url =
    Http.get url decodeUserAndEvents
        |> RemoteData.sendRequest
        |> Cmd.map DataReceived


getTopic : String -> String -> Cmd Msg
getTopic key url =
    Http.getString url
        |> RemoteData.sendRequest
        |> Cmd.map (TopicReceived key)


getTopics : Dict String String -> Cmd Msg
getTopics topics =
    let
        keys =
            Dict.keys topics

        topicsCmd =
            List.map (\k -> getTopic k <| Maybe.withDefault "" <| Dict.get k topics) keys
    in
    Cmd.batch topicsCmd


userDecoder : Decoder UserData
userDecoder =
    let
        socialNetworkDecoder =
            Json.Decode.map2 (,) (index 0 string) (index 1 string)
    in
    decode UserData
        |> Pipe.required "name" string
        |> Pipe.required "email" string
        |> Pipe.optional "site" string ""
        |> Pipe.optional "phone" string ""
        |> Pipe.optional "socialNetworks" (Json.Decode.list socialNetworkDecoder) []


eventDecoder : Decoder Event
eventDecoder =
    decode Event
        |> Pipe.required "name" string
        |> Pipe.required "dateStart" int
        |> Pipe.required "dateEnd" int
        |> Pipe.required "topics" (dict string)


decodeUserAndEvents : Decoder UserAndEvents
decodeUserAndEvents =
    decode UserAndEvents
        |> Pipe.required "user" userDecoder
        |> Pipe.optional "events" (Json.Decode.list eventDecoder) []


sendTopicsByEmail : String -> String -> String -> Cmd Msg
sendTopicsByEmail eventName email content =
    let
        subject =
            Http.encodeUri <| "[" ++ eventName ++ "]"

        body =
            Http.encodeUri content
    in
    openWindow <| "mailto:" ++ email ++ "?subject=" ++ subject ++ "&body=" ++ body


saveUserAndEventsData : UserData -> List Event -> Cmd Msg
saveUserAndEventsData user events =
    let
        string =
            Json.Encode.string

        object =
            Json.Encode.object

        list =
            Json.Encode.list

        encode =
            Json.Encode.encode

        int =
            Json.Encode.int

        tupleEncoder ( k, v ) =
            list [ string k, string v ]

        userObject =
            object
                [ ( "name", string user.name )
                , ( "email", string user.email )
                , ( "site", string user.site )
                , ( "phone", string user.phone )
                , ( "socialNetworks", list <| List.map tupleEncoder user.socialNetworks )
                ]

        topicEncoder ( label, content ) =
            ( label, string content )

        eventEncoder event =
            object
                [ ( "name", string event.name )
                , ( "dateStart", int event.dateStart )
                , ( "dateEnd", int event.dateEnd )
                , ( "topics", object <| List.map topicEncoder <| Dict.toList event.topics )
                ]

        eventsObject =
            list <| List.map eventEncoder events

        userAndEventsObject =
            object
                [ ( "user", userObject )
                , ( "events", eventsObject )
                ]

        data =
            encode 4 userAndEventsObject
    in
    saveData data



---- UPDATE ----


type Msg
    = NoOp
    | ToggleMenu
    | GoView View
    | DataUrlChanged String
    | ImportData
    | DataReceived (WebData UserAndEvents)
    | TopicReceived String (WebData String)
    | SendTopics
    | EmailChanged String
    | TopicSelectionChanged String Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }, Cmd.none )

        GoView v ->
            ( { model | currentView = v, menuOpen = False }, Cmd.none )

        DataUrlChanged s ->
            ( { model | dataUrl = s }, Cmd.none )

        ImportData ->
            let
                d =
                    Debug.log "URL" model.dataUrl
            in
            ( { model | currentView = Loading }, getData model.dataUrl )

        DataReceived response ->
            case response of
                RemoteData.Success data ->
                    let
                        currentEvent =
                            List.Extra.getAt 0 data.events

                        nextCmd =
                            case currentEvent of
                                Just e ->
                                    getTopics e.topics

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model
                        | user = Just data.user
                        , events = data.events
                        , currentEvent = currentEvent
                        , currentView = Loading
                      }
                    , nextCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TopicReceived key content ->
            case content of
                RemoteData.Success data ->
                    let
                        topics =
                            case currentEvent of
                                Just event ->
                                    event.topics

                                Nothing ->
                                    Dict.empty

                        newTopics =
                            Dict.insert key data topics

                        currentEvent =
                            model.currentEvent

                        newCurrentEvent =
                            case currentEvent of
                                Just event ->
                                    Just { event | topics = newTopics }

                                Nothing ->
                                    Nothing

                        newModel =
                            { model | currentEvent = newCurrentEvent }

                        nextCmd =
                            case model.user of
                                Just u ->
                                    case newCurrentEvent of
                                        Just ev ->
                                            saveUserAndEventsData u [ ev ]

                                        Nothing ->
                                            Cmd.none

                                Nothing ->
                                    Cmd.none
                    in
                    ( { newModel | currentView = TopicList }, nextCmd )

                _ ->
                    ( model, Cmd.none )

        TopicSelectionChanged topic isSelected ->
            if isSelected then
                ( { model | selectedTopics = Set.insert topic model.selectedTopics }, Cmd.none )
            else
                ( { model | selectedTopics = Set.remove topic model.selectedTopics }, Cmd.none )

        SendTopics ->
            let
                isTopicSelected topic =
                    Set.member topic model.selectedTopics

                content =
                    case model.currentEvent of
                        Just event ->
                            Dict.filter (\k v -> isTopicSelected k) event.topics
                                |> Dict.values
                                |> String.join (String.fromChar '\n')

                        Nothing ->
                            ""

                nextCmd =
                    case model.currentEvent of
                        Just event ->
                            sendTopicsByEmail event.name model.destinationEmail content

                        Nothing ->
                            Cmd.none
            in
            ( model, nextCmd )

        EmailChanged email ->
            ( { model | destinationEmail = email }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


header : Maybe Event -> Bool -> Html Msg
header event menuOpen =
    let
        eventSubtitle dateStart dateEnd =
            let
                s =
                    Date.fromTime (toFloat dateStart)

                e =
                    Date.fromTime (toFloat dateEnd)

                ss =
                    (toString <| Date.day s) ++ "/" ++ (toString <| Date.month s)

                ee =
                    (toString <| Date.day e) ++ "/" ++ (toString <| Date.month e)
            in
            ss ++ " - " ++ ee

        ( title, subtitle ) =
            case event of
                Nothing ->
                    ( ".Engage", "Go talk with people!" )

                Just e ->
                    ( e.name, eventSubtitle e.dateStart e.dateEnd )

        menuItem label view =
            a
                [ class "navbar-item"
                , onClick (GoView view)
                ]
                [ text label ]
    in
    section [ class "hero is-primary is-bold" ]
        [ div [ class "hero-head" ]
            [ nav [ class "navbar" ]
                [ div
                    [ class "container" ]
                    [ div [ class "navbar-brand" ]
                        [ a
                            [ class "navbar-item" ]
                            [ img
                                [ style
                                    [ ( "color", "#FFF" )
                                    , ( "margin", "0" )
                                    , ( "width", "42px" )
                                    ]
                                , src "/logo.svg"
                                ]
                                []
                            ]
                        , span
                            [ classList
                                [ ( "navbar-burger", True )
                                , ( "burger", True )
                                , ( "is-active", menuOpen )
                                ]
                            , attribute "data-target" "appMenu"
                            , onClick ToggleMenu
                            ]
                            [ span [] []
                            , span [] []
                            , span [] []
                            ]
                        ]
                    , div
                        [ id "appMenu"
                        , classList
                            [ ( "navbar-menu", True )
                            , ( "is-active", menuOpen )
                            ]
                        ]
                        [ div [ class "navbar-end" ]
                            [ menuItem "Contact" ContactDisplay
                            , menuItem "Topics" TopicList
                            , menuItem "Settings" DataImporter
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text title ]
                , h2 [ class "subtitle" ] [ text subtitle ]
                ]
            ]
        ]


loadingView : Html Msg
loadingView =
    section [ class "section" ]
        [ div [ class "container" ]
            [ i [ class "fas fa-spinner fa-5x fa-spin" ] []
            , p [] [ text "Loading" ]
            ]
        ]


dataImporterView : String -> Html Msg
dataImporterView dataUrl =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Import data" ]
            , div [ class "field" ]
                [ div [ class "label" ] [ text "URL" ]
                , div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder "URL with data"
                        , onInput DataUrlChanged
                        , Html.Attributes.value dataUrl
                        ]
                        []
                    ]
                , p [ class "help" ]
                    [ text "Point the app to a JSON with the data" ]
                ]
            , button
                [ class "button is-primary"
                , onClick ImportData
                ]
                [ text "Import" ]
            ]
        ]


qrCodeView : String -> Html msg
qrCodeView message =
    QRCode.encode message
        |> Result.map QRCode.toSvg
        |> Result.withDefault
            (Html.text "Error while encoding to QRCode.")


mecard : UserData -> String
mecard user =
    "MECARD:N:" ++ user.name ++ ";TEL:" ++ user.phone ++ ";EMAIL:" ++ user.email ++ ";URL:" ++ user.site ++ ";;"


contactDisplayView : UserData -> Html Msg
contactDisplayView user =
    section [ class "section" ]
        [ div [ class "container" ]
            [ qrCodeView <| mecard user
            ]
        ]


topicListView : Event -> Html Msg
topicListView event =
    let
        isDisabled key =
            String.length (Maybe.withDefault "" (Dict.get key event.topics)) < 30

        singleTopicDisplay topic =
            div [ class "field" ]
                [ div [ class "control" ]
                    [ label [ class "checkbox" ]
                        [ input
                            [ type_ "checkbox"
                            , disabled (isDisabled topic)
                            , onCheck (TopicSelectionChanged topic)
                            ]
                            []
                        , text (" " ++ topic) -- this is a fucking hack!
                        ]
                    ]
                ]

        topics =
            div [ class "container" ]
                (List.map singleTopicDisplay <| Dict.keys event.topics)

        emailInput =
            div [ class "field" ]
                [ div [ class "label" ] [ text "Email" ]
                , div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder "Email"
                        , onInput EmailChanged
                        ]
                        []
                    ]
                ]

        submitButton =
            button
                [ class "button is-primary"
                , onClick SendTopics
                ]
                [ text "Send topics" ]
    in
    section [ class "section" ]
        [ div [ class "form" ]
            [ emailInput
            , topics
            , submitButton
            ]
        ]


view : Model -> Html Msg
view model =
    let
        currentView =
            case model.currentView of
                Loading ->
                    loadingView

                DataImporter ->
                    dataImporterView model.dataUrl

                ContactDisplay ->
                    case model.user of
                        Just u ->
                            contactDisplayView u

                        Nothing ->
                            dataImporterView model.dataUrl

                TopicList ->
                    case model.currentEvent of
                        Nothing ->
                            dataImporterView model.dataUrl

                        Just e ->
                            topicListView e

                _ ->
                    loadingView
    in
    div []
        [ header model.currentEvent model.menuOpen
        , currentView
        ]



---- PROGRAM ----


main : Program Value Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
