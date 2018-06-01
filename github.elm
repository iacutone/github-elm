module GitHubStats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (at, string, field, decodeString, int)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, requiredAt)
import Regex exposing (replace, regex)

type alias Model =
    { message : String
    , users : Maybe Users
    , pullRequests : Maybe PullRequests
    }

type alias Users =
    { edges : List Node }

type alias Node =
    { node: User
    }

type alias User =
    { id:  String
    , login : String
    }

type alias PullRequests =
    { nodes : List PullRequest }

type alias PullRequest =
    { typename: String
    , prNumber : Int
    , title : String
    , createdAt : String
    , additions : Int
    , deletions : Int
    , repoName : String
    }

-- UPDATE

type Msg
    = ParseUsersJson (Result Http.Error String)
    | ParsePullRequestJson (Result Http.Error String)
    | DisplayData String
    | None

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParseUsersJson (Ok res) ->
            case decodeString decodeLogin res of
                Ok res ->
                    ( { model | users = Just res, message = "" }, Cmd.none )
                Err error ->
                    ( { model | message = error, users = Nothing }, Cmd.none )

        ParseUsersJson (Err res) ->
            ( { model | users = Nothing }, Cmd.none )

        DisplayData user ->
            let
                json = Regex.replace Regex.All (Regex.regex "user") (\_ -> user) getUserPullRequests
            in
                ( model, Http.send ParsePullRequestJson (request json) )

        ParsePullRequestJson (Ok res) ->
            case decodeString decodePullRequests res of
                Ok res ->
                    ( { model | pullRequests = Just res }, Cmd.none )
                Err error ->
                    ( { model | message = error, users = Nothing }, Cmd.none )

        ParsePullRequestJson (Err res) ->
            ( { model | users = Nothing, message = toString res }, Cmd.none )

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

decodePullRequests =
    decode PullRequests
        |> requiredAt ["data", "search", "nodes"] (Json.Decode.list decodePullRequest)

decodePullRequest =
    decode PullRequest
        |> required "__typename" string
        |> required "number" int
        |> required "title" string
        |> required "createdAt" string
        |> required "additions" int
        |> required "deletions" int
        |> requiredAt [ "repository", "name" ] string

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ div [] [ displayUsers model ]
    , div [] [ displayPullRequests model ] 
    ]

displayUsers : Model -> Html Msg
displayUsers model =
    let 
        users = model.users
    in
        case users of
            Just users ->
                let
                    nodes = List.map .node users.edges
                in
                    div [] [ ul [] (List.map displayUser nodes)
                        , div [] [ text model.message ]
                        ]

            Nothing ->
                div [] []

displayUser : User -> Html Msg
displayUser user =
    li [] [ a [ href "#", onClick (DisplayData user.login) ] [ text user.login ] ]

displayPullRequests : Model -> Html Msg
displayPullRequests model =
    let 
        pullRequests = model.pullRequests
    in
        case pullRequests of
            Just pullRequests ->
                div [] [ ul [] (List.map displayPullRequest pullRequests.nodes)
                    , div [] [ text model.message ]
                    ]
            Nothing ->
                div [] []

displayPullRequest : PullRequest -> Html Msg
displayPullRequest pr =
    li [] [ text pr.title ]

request : String -> Http.Request String
request query =
    let
        headers =
            [ Http.header "Authorization" auth
            ]
    in
        Http.request
            { method = "POST"
            , headers = headers
            , url = baseUrl
            , body = Http.jsonBody (Encode.object [("query", Encode.string query)])
            , expect = Http.expectString
            , timeout = Nothing
            , withCredentials = False
            }

getUsers : String
getUsers =
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

getUserPullRequests : String
getUserPullRequests =
    """
    query {
      search(type: ISSUE, query: "org:fracturedatlas type:PR is:MERGED author:user", last: 10) {
        nodes {
          __typename
          ... on PullRequest {
            number
            title
            createdAt
            additions
            deletions
            repository {
              name
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
    -- "Bearer <your token>"

init : (Model, Cmd Msg)
init =
    ( initialModel, Http.send ParseUsersJson (request getUsers))

initialModel : Model
initialModel =
    { users = Nothing
    , pullRequests = Nothing
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

