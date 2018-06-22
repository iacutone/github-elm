module GitHubStats exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (at, string, field, decodeString, int)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, requiredAt)
import Regex exposing (replace, regex)

type alias Model =
    { message : String
    , organization : Maybe Organization
    , pullRequests : Maybe PullRequests
    , currentUser : String
    }

type alias Organization =
    { users : List Users
    , repos : List Repos
    }

type alias Users =
    { node : User }

type alias Repos =
    { node : Repo }

type alias User =
    { id :  String
    , login : String
    }

type alias Repo =
    { id : String
    , name : String 
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
    , url : String
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
            case decodeString decodeOrganization res of
                Ok res ->
                    ( { model | organization = Just res, message = "" }, Cmd.none )
                Err error ->
                    ( { model | message = error, organization = Nothing }, Cmd.none )

        ParseUsersJson (Err res) ->
            ( { model | organization = Nothing }, Cmd.none )

        DisplayData user ->
            let
                json = Regex.replace Regex.All (Regex.regex "user") (\_ -> user) getUserPullRequests
            in
                ( { model | currentUser = user }, Http.send ParsePullRequestJson (request json) )

        ParsePullRequestJson (Ok res) ->
            case decodeString decodePullRequests res of
                Ok res ->
                    ( { model | pullRequests = Just res }, Cmd.none )
                Err error ->
                    ( { model | message = error}, Cmd.none )

        ParsePullRequestJson (Err res) ->
            ( { model | message = toString res }, Cmd.none )

        None ->
            ( model , Cmd.none )

decodeOrganization =
    decode Organization
        |> requiredAt ["data", "organization", "team", "members", "edges"] (Json.Decode.list decodeUsers)
        |> requiredAt ["data", "organization", "repositories", "edges"] (Json.Decode.list decodeRepos)

decodeUsers =
    decode Users
        |> required "node" decodeUser

decodeRepos =
    decode Repos
        |> required "node" decodeRepo

decodeUser =
    decode User
        |> required "id" string
        |> required "login" string

decodeRepo =
    decode Repo
        |> required "id" string
        |> required "name" string

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
        |> required "url" string

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ div [] [ displayOrganization model ]
    , div [] [ displayPullRequests model ] 
    ]

displayOrganization : Model -> Html Msg
displayOrganization model =
    let
        organization = model.organization
    in
        case organization of
            Just organization ->
                let 
                    users = organization.users
                    repos = organization.repos
                    userNodes = List.map .node users
                    repoNodes = List.map .node repos
                in
                    div [] [ ul [] (List.map displayUser userNodes)
                        , ul [] (List.map displayRepo repoNodes)
                        , div [] [ text model.message ]
                        ]

            Nothing ->
                div [] []

displayUser : User -> Html Msg
displayUser user =
    li [] [ a [ href "#", onClick (DisplayData user.login) ] [ text user.login ] ]

displayRepo : Repo -> Html Msg
displayRepo repo =
    li [] [ a [ href "#" ] [ text repo.name ] ]

displayPullRequests : Model -> Html Msg
displayPullRequests model =
    let 
        pullRequests = model.pullRequests
    in
        case pullRequests of
            Just pullRequests ->
                div [ class "table-wrapper" ] [ table [] [ caption [] [ h1 [] [ text ("Lines of Code per Pull Request by " ++ model.currentUser) ] ] ]
                    , thead [] [ tr [] [ th [] [ text "PR" ]
                                        , th [] [ text "Title" ]
                                        , th [] [ text "Repo" ]
                                        , th [] [ text "Date Created" ]
                                        , th [] [ text "LoC" ] 
                                        ] 
                                ]
                    , tbody [] (List.map pullRequestDataRow pullRequests.nodes)
                    ]
            Nothing ->
                div [] []

pullRequestDataRow : PullRequest -> Html Msg
pullRequestDataRow pullRequest =
    tr [] [ th [] [ a [ href pullRequest.url, target "_blank" ] [ text (toString pullRequest.prNumber) ] ]
        , td [] [  text pullRequest.title ]
        , td [] [ text pullRequest.repoName ]
        , td [] [ text pullRequest.createdAt ]
        , td [] [ ul [ class "list-reset" ] [ li [] [ text (toString pullRequest.additions) ] ] 
        , li [] [ text (toString pullRequest.deletions) ]]
        , li [] [ text (locChange pullRequest.additions pullRequest.deletions) ]
        ]

locChange : Int -> Int -> String
locChange additions deletions =
    let
        diff = additions - deletions
    in
        toString diff

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

getUsersAndRepos : String
getUsersAndRepos =
    """
    query {
      organization(login: "FracturedAtlas") {
        repositories(last:3) {
          edges {
            node {
              id
              name
            }
          }
        }
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
            url
            repository {
              name
            }
          }
        }
      }
    }
    """

getRepoPullRequests : String
getRepoPullRequests =
    """
    query {
      repository(owner: "FracturedAtlas", name: "FundingWorks") {
        pullRequests(last: 3) {
          nodes {
            number
            title
            createdAt
            additions
            deletions
            url
            author {
              login
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
    ( initialModel, Http.send ParseUsersJson (request getUsersAndRepos))

initialModel : Model
initialModel =
    { organization = Nothing
    , pullRequests = Nothing
    , message = "Waiting for a response..." 
    , currentUser = ""
    }

main : Program Never Model Msg
main =
    Html.program
        { init = init 
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

