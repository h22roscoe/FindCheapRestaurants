module Main exposing (main)
import Browser
import Html exposing (Html, div, label, input, h1, p, section, text, strong, button, table, thead, tbody, abbr, tr, th, td)
import Html.Attributes exposing (class, id, step, min, max, type_, value)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (Decoder, field, string, list, float, int, nullable, map4)

-- MAIN

main : Program (Float, Float) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type ResultsModel
    = NotStarted
    | Loading
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

type alias Place =
    { name : String
    , rating : Float
    , price_level : Int
    , address : String 
    }

mainDecoder : Decoder (List (Maybe Place))
mainDecoder = list (nullable placeDecoder)

placeDecoder : Decoder Place
placeDecoder =
    map4 Place
        (field "name" string)
        (field "rating" float)
        (field "price_level" int)
        (field "adr_address" string)

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
    | ResultsReceived (Result Http.Error (List (Maybe Place)))

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
        
        ResultsReceived result ->
            case result of
                Ok rs ->
                    let
                        noNulls = List.filterMap identity rs
                    in 
                        ({ model | results = Loaded noNulls }, Cmd.none)
                Err _ ->
                    ({ model | results = Loading }, Cmd.none)
            

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

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
        Loaded rs ->
            [ div [ id "results", class "table-container" ]
                [ table []
                    [ thead []
                        [ tr []
                            [ th [] [ text "Name" ]
                            , th [] [ abbr [] [ text "Rating" ], text "⭐" ]
                            , th [] [ abbr [] [ text "Price level" ], text "£" ]
                            , th [] [ abbr [] [ text "Address" ], text "Addr" ]
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
        , td [] [ text (String.fromInt place.price_level) ]
        , td [] [ text place.address ]
        ]

titles : List (Html Msg)
titles =
    [ h1 [ class "title" ] [ text "Find Cheap Restaurants" ]
    , p [ class "subtitle" ]
        [ text "Helps you find "
        , strong [] [ text "good" ]
        , text ",cheap restaurants in an area."
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
getResults model =
    let
        (lat, long) = model.location
        locationAsString = String.join "," (List.map String.fromFloat [lat, long])
        params = String.join "&" (List.map (String.join "=") 
            [ [ "location", locationAsString ]
            , [ "minRating", String.fromFloat model.minRating ]
            , [ "minPrice", String.fromInt model.minPrice ]
            , [ "maxPrice", String.fromInt model.maxPrice ]
            , [ "radius", String.fromInt model.radius ]
            , [ "minUserRatings", String.fromInt model.minUserRatings ]
            ])
    in   
        Http.get
            { url = "https://europe-west1-findcheaprestaurants.cloudfunctions.net/restaurants?" ++ params
            , expect = Http.expectJson ResultsReceived mainDecoder
            }