port module Main exposing (main)
import Browser
import Html exposing (Html, div, label, input, h1, p, section, text, strong, button, table, thead, tbody, abbr, tr, th, td)
import Html.Attributes exposing (class, id, step, min, max, type_, value, title)
import Html.Events exposing (onInput, onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Html.Parser.Util exposing (toVirtualDom)
import Html.Parser exposing (run)

-- MAIN

main : Program (Float, Float) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> placesFound ResultsReceived
        }

port findPlaces : Encode.Value -> Cmd msg
port placesFound : (Decode.Value -> msg) -> Sub msg

-- MODEL

type ResultsModel
    = NotStarted
    | Loading
    | FailedToLoad Decode.Error
    | Loaded (List Place)

type alias Model =
    { minRating : Float
    , minPrice : Int
    , maxPrice : Int
    , radius : Int
    , minUserRatings : Int
    , location : (Float, Float)
    , results : ResultsModel
    }

encodeModel : Model -> Encode.Value
encodeModel model =
    let (lat, lng) = model.location in
    Encode.object
        [ ("minRating", Encode.float model.minRating)
        , ("minPrice", Encode.int model.minPrice)
        , ("maxPrice", Encode.int model.maxPrice)
        , ("radius", Encode.int model.radius)
        , ("minUserRatings", Encode.int model.minUserRatings)
        , ("location", Encode.list identity [ Encode.float lat, Encode.float lng ])
        ]

type alias Place =
    { name : String
    , rating : Float
    , price_level : Maybe Int
    , address : String 
    }

type PlacesResultStatus
    = Ok
    | ZeroResults
    | OverQueryLimit
    | RequestDenied
    | InvalidRequest
    | UnknownError

type alias PlacesResult = 
    { hasNextPage : Bool 
    , results : List Place
    , status : PlacesResultStatus
    }

mainDecoder : Decode.Decoder PlacesResult
mainDecoder =
    Decode.map3 PlacesResult
        (Decode.field "hasNextPage" Decode.bool)
        (Decode.field "results" (Decode.list placeDecoder))
        (Decode.field "status" statusDecoder)

statusDecoder : Decode.Decoder PlacesResultStatus
statusDecoder =
    Decode.string
        |> Decode.andThen (\str ->
            case str of
                "OK" ->
                    Decode.succeed Ok
                "ZERO_RESULTS" ->
                    Decode.succeed ZeroResults
                "OVER_QUERY_LIMIT" ->
                    Decode.succeed OverQueryLimit
                "REQUEST_DENIED" ->
                    Decode.succeed RequestDenied
                "INVALID_REQUEST" ->
                    Decode.succeed InvalidRequest
                "UNKNOWN_ERROR" ->
                    Decode.succeed UnknownError
                somethingElse ->
                    Decode.fail <| "Unknown status type: " ++ somethingElse
        )

placeDecoder : Decode.Decoder Place
placeDecoder =
    Decode.map4 Place
        (Decode.field "name" Decode.string)
        (Decode.field "rating" Decode.float)
        (Decode.maybe (Decode.field "price_level" Decode.int))
        (Decode.field "vicinity" Decode.string)

init : (Float, Float) -> (Model, Cmd Msg)
init (lat, long) =
    (Model 4.0 0 2 3000 10 (lat, long) NotStarted, Cmd.none)

-- UPDATE

type Msg
    = Location (Float, Float)
    | MinRating Float
    | MinPrice Int
    | MaxPrice Int
    | Radius Int
    | MinUserRatings Int
    | FindPlaces
    | CloseModal
    | ResultsReceived Decode.Value

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Location loc ->
            ({ model | location = loc }, Cmd.none)

        MinRating rating ->
            ({ model | minRating = rating }, Cmd.none)

        MinPrice price ->
            if price <= model.maxPrice then
                ({ model | minPrice = price }, Cmd.none)
            else
                (model, Cmd.none)
        
        MaxPrice price ->
            if price >= model.minPrice then
                ({ model | maxPrice = price }, Cmd.none)
            else
                (model, Cmd.none)
        
        Radius r ->
            ({ model | radius = r }, Cmd.none)

        MinUserRatings minUserRatings ->
            ({ model | minUserRatings = minUserRatings }, Cmd.none)
        
        FindPlaces ->
            ({ model | results = Loading }, getResults model)
        
        CloseModal ->
            ({ model | results = NotStarted }, Cmd.none)
        
        ResultsReceived v ->
            case Decode.decodeValue mainDecoder v of
                Result.Ok rs ->
                    ({ model | results = Loaded rs.results }, Cmd.none)
                Result.Err err ->
                    ({ model | results = FailedToLoad err }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  section [ class "section" ]
    [ div [ class "level" ] (titles ++ [
            mainForm model
        ])
    , div [ class "level" ] (tablePart model.results)
    ]

tablePart : ResultsModel -> List (Html Msg)
tablePart rm =
    case rm of
        NotStarted ->
            []
        Loading ->
            [ div [ class "container" ]
                [ div [ class "loader-wrapper is-active" ]
                    [ div [ class "loader is-loading" ] []
                    ]
                ]
            ]
        FailedToLoad err ->
            [ div [ class "container" ]
                [ div [ class "modal is-active" ]
                    [ div [ class "modal-background" ] []
                    , div [ class "modal-content" ] [ p [] [ text (Decode.errorToString err) ] ]
                    , button [ class "modal-close is-large", onClick CloseModal ] []
                    ]
                ]
            ]
        Loaded rs ->
            [ div [ id "results", class "table-container" ]
                [ table [ class "table is-striped is-fullwidth" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Name" ]
                            , th [] [ abbr [ title "Rating" ] [ text "⭐" ] ]
                            , th [] [ abbr [ title "Price level" ] [ text "£" ] ]
                            , th [] [ abbr [ title "Address" ] [ text "Addr" ] ]
                            ]
                        ]
                    , tbody [] (List.map placeToRow rs)
                    ]
                ]
            ]

placeToRow : Place -> Html Msg
placeToRow place =
    tr []
        [ td [] [ text place.name ]
        , td [] [ text (String.fromFloat place.rating) ]
        , td [] [ text (String.fromInt <| Maybe.withDefault 0 place.price_level) ]
        , td [] (viewAddr place.address)
        ]

viewAddr : String -> List (Html Msg)
viewAddr addr =
    case run addr of
        Result.Ok nodes ->
            toVirtualDom nodes
        Result.Err _ ->
            [ text "No address found" ]

titles : List (Html Msg)
titles =
    [ h1 [ class "title" ] [ text "Find Cheap Restaurants" ]
    , p [ class "subtitle" ]
        [ text "Helps you find "
        , strong [] [ text "good" ]
        , text ", cheap restaurants in an area."
        ]
    ]

mainForm : Model -> Html Msg
mainForm model =
    Html.form []
        [ formField "Minimum price" (slider "minPrice" 1 0 4 (String.fromInt model.minPrice) (MinPrice << Maybe.withDefault 0 << String.toInt)) (p [] [ text (String.fromInt model.minPrice) ])
        , formField "Maximum price" (slider "maxPrice" 1 0 4 (String.fromInt model.maxPrice) (MaxPrice << Maybe.withDefault 2 << String.toInt)) (p [] [ text (String.fromInt model.maxPrice) ])
        , formField "Minimum rating" (slider "minRating" 0.01 0 4.99 (String.fromFloat model.minRating) (MinRating << Maybe.withDefault 4.0 << String.toFloat)) (p [] [ text (String.fromFloat model.minRating) ])
        , formField "Radius" (slider "radius" 1000 1000 50000 (String.fromInt model.radius) (Radius << Maybe.withDefault 3000 << String.toInt)) (p [] [ text (String.fromInt model.radius) ])
        , formField "Minimum user ratings" (slider "minUserRatings" 1 0 5000 (String.fromInt model.minUserRatings) (MinUserRatings << Maybe.withDefault 10 << String.toInt)) (p [] [ text (String.fromInt model.minUserRatings)])
        , formField "" (button [ class "button is-primary", onClick FindPlaces, type_ "button" ] [ text "Find restaurants near me!" ]) (div [] [])
        ]

formField : String -> Html Msg -> Html Msg -> Html Msg
formField name input output =
    div [ class "field is-horizontal" ]
        [ label [ class "field-label is-normal" ] [ text name ]
        , div [ class "field-body" ]
            [ div [ class "control" ] [ input ]
            , output
            ]
        ]

slider : String -> Float -> Float -> Float -> String -> (String -> msg) -> Html msg
slider id_ step_ min_ max_ v toMsg =
    let
        stepStr = String.fromFloat step_
        minStr = String.fromFloat min_
        maxStr = String.fromFloat max_
    in
        input [ id id_, step stepStr, Html.Attributes.min minStr, Html.Attributes.max maxStr, type_ "range", class "slider has-output is-fullwidth", onInput toMsg, value v ] []

-- HTTP

getResults : Model -> Cmd Msg
getResults model = findPlaces (encodeModel model)
