module GitHubStats exposing (..)

import Html exposing (..)
import Http
import Json.Decode exposing (at, string, field, decodeString)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, requiredAt)

type alias Model =
    { message : String
    , edges : Maybe Users
    }

type alias Users =
    { edges : List Node }

type alias User =
    { id:  String
    , login : String
    }

type alias Node =
    { node: User
    }

-- UPDATE

type Msg
    = FetchGHData (Result Http.Error String)
    | None

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGHData (Ok res) ->
            case decodeString decodeLogin res of
                Ok res ->
                    ( { model | edges = Just res }, Cmd.none )
                Err error ->
                    ( { model | message = error, edges = Nothing }, Cmd.none )

        FetchGHData (Err res) ->
            ( { model | edges = Nothing }, Cmd.none )
        
        None ->
            ( model , Cmd.none )

decodeLogin =
    decode Users
        |> requiredAt ["data", "organization", "team", "members", "edges"] (Json.Decode.list decodeNode)

decodeNode =
    decode Node
        |> required "node" decodeUser

decodeUser =
    decode User
        |> required "id" string
        |> required "login" string

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ div [] [ text model.message]
    , displayUsers model.edges
    ]

displayUsers users =
    case users of 
        Just users ->
            div [] [ text (toString users) ]
        Nothing ->
            div [][]

request : Http.Request String
request =
    let
        headers =
            [ Http.header "Authorization" auth
            ]
    in
        Http.request
            { method = "POST"
            , headers = headers
            , url = baseUrl
            , body = Http.jsonBody (Encode.object [("query", Encode.string ghQuery)])
            , expect = Http.expectString
            , timeout = Nothing
            , withCredentials = False
            }

ghQuery : String
ghQuery =
    """
    query {
      organization(login: "FracturedAtlas") {
        team(slug: "fa-dev") {
          members(first:2) {
            edges {
              node {
                id
                login
              }
            }
          }
        }
      }
    }
    """

baseUrl : String
baseUrl =
    "https://api.github.com/graphql"

auth : String
auth =
    "Bearer <your token>"

init : (Model, Cmd Msg)
init =
    ( initialModel, Http.send FetchGHData request )

initialModel : Model
initialModel =
    { edges = Nothing
    , message = "Waiting for a response..." 
    }

main : Program Never Model Msg
main =
    Html.program
        { init = init 
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

-- https://github.com/dillonkearns/graphqelm
-- http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest
