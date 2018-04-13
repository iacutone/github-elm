module GitHubStats exposing (..)

import Html exposing (..)
import Http
import Json.Decode
import Json.Encode as Encode

type alias Model =
    { response: String }

-- UPDATE

type Msg
    = FetchGHData (Result Http.Error String)
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGHData (Ok res) ->
            ( { model | response = res }, Cmd.none )

        FetchGHData (Err res) ->
            ( { model | response = toString res }, Cmd.none )
        
        None ->
            ( model , Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text model.response ]

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
        name
        id
        repositories(last: 1) {
          edges {
            node {
              name
              pullRequests(last: 10) {
                edges {
                  node {
                    number
                  }
                }
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
    "Bearer <your api key here>" 

init : (Model, Cmd Msg)
init =
    ( initialModel, Http.send FetchGHData request )

initialModel : Model
initialModel =
    { response = "Waiting for a response..." }

main : Program Never Model Msg
main =
    Html.program
        { init = init 
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

-- https://github.com/dillonkearns/graphqelm
