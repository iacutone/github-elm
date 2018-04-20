module GitHubStats exposing (..)

import Html exposing (..)
import Http
import Json.Decode exposing (at, string, field, decodeString)
import Json.Encode as Encode

type alias Model =
    { message : String
    , response : Maybe (List User)
    }

type alias User =
    { id: String
    , login: String
    }

-- UPDATE

type Msg
    = FetchGHData (Result Http.Error String)
    | None

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGHData (Ok res) ->
            let
                result = decodeLogin res
            in
                case result of
                    Just result ->
                        ( { model | response = result }, Cmd.none )
                    Nothing ->
                        ( { model | response = Nothing }, Cmd.none )



        FetchGHData (Err res) ->
            ( { model | response = toString res }, Cmd.none )
        
        None ->
            ( model , Cmd.none )

-- decodeLogin : Json.Decode.Decoder String -> String a
decodeLogin response =
    at ["data", "organization", "team", "member", "edges"] (login response)

-- login : Json.Decode.Decoder String -> String
login login =
        id = Result.withDefault "" (field "id" string)
        login =  Result.withDefault "" (field "login" string)
        list = []


-- VIEW


view : Model -> Html Msg
view model =
    let
        response = model.response
    in
        case response of
            Just response ->
                div [] (List.map user model.response)
            Nothing ->
                div [][]

user user =
    div [][ text user.id ]

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
    -- query {
    --   organization(login: "FracturedAtlas") {
    --     name
    --     id
    --     repositories(last: 5, orderBy: {field: PUSHED_AT, direction: ASC}) {
    --       edges {
    --         node {
    --           name
    --           pullRequests(last: 10, states: MERGED, author: "jgaskins") {
    --             edges {
    --               node {
    --                 number
    --                 title
    --                 url
    --                 author {
    --                   login
    --                 }
    --                 additions
    --                 deletions
    --                 createdAt
    --               }
    --             }
    --           }
    --         }
    --       }
    --     }
    --   }
    -- }
    -- """

baseUrl : String
baseUrl =
    "https://api.github.com/graphql"

auth : String
auth =

init : (Model, Cmd Msg)
init =
    ( initialModel, Http.send FetchGHData request )

initialModel : Model
initialModel =
    { response = Nothing
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
