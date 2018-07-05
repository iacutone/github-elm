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
    , userPullRequests : Maybe UserPullRequests
    , repoPullRequests : Maybe RepoPullRequests
    , currentUser : String
    , currentRepo : String
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

type alias UserPullRequests =
    { nodes : List UserPullRequest }

type alias RepoPullRequests =
    { nodes : List RepoPullRequest }

type alias UserPullRequest =
    { typename: String
    , prNumber : Int
    , title : String
    , createdAt : String
    , additions : Int
    , deletions : Int
    , repoName : String
    , url : String
    }

type alias RepoPullRequest =
    { prNumber : Int
    , title : String
    , createdAt : String
    , additions : Int
    , deletions : Int
    , author : String
    , url : String
    }

type PullRequestButton = 
    UserPullRequestButton
    | RepoPullRequestButton

-- UPDATE

type Msg
    = ParseUsersJson (Result Http.Error String)
    | ParseUserPullRequestJson (Result Http.Error String)
    | ParseRepoPullRequestJson (Result Http.Error String)
    | DisplayUserData String
    | DisplayRepoData String
    | UpdateUserPullRequestQuantity String
    | UpdateRepoPullRequestQuantity String
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

        DisplayUserData user ->
            let
                json = Regex.replace Regex.All (Regex.regex "user") (\_ -> user) getUserPullRequests
            in
                ( { model | currentUser = user }, Http.send ParseUserPullRequestJson (request json) )

        DisplayRepoData repo ->
            let
                json = Regex.replace Regex.All (Regex.regex "repoName") (\_ -> repo) getRepoPullRequests
            in
                ( { model | currentRepo = repo }, Http.send ParseRepoPullRequestJson (request json) )

        ParseUserPullRequestJson (Ok res) ->
            case decodeString decodeUserPullRequests res of
                Ok res ->
                    ( { model | userPullRequests = Just res }, Cmd.none )
                Err error ->
                    ( { model | message = error }, Cmd.none )

        ParseUserPullRequestJson (Err res) ->
            ( { model | message = toString res }, Cmd.none )

        ParseRepoPullRequestJson (Ok res) ->
            case decodeString decodeRepoPullRequests res of
                Ok res ->
                    ( { model | repoPullRequests = Just res }, Cmd.none )
                Err error ->
                    ( { model | message = error }, Cmd.none )

        ParseRepoPullRequestJson (Err res) ->
            ( { model | message = toString res }, Cmd.none )

        UpdateUserPullRequestQuantity num ->
            let
                json = Regex.replace Regex.All (Regex.regex "10") (\_ -> num) getUserPullRequests
                updated = Regex.replace Regex.All (Regex.regex "user") (\_ -> model.currentUser) json
            in
                ( model, Http.send ParseUserPullRequestJson (request updated) )

        UpdateRepoPullRequestQuantity num ->
            let
                json = Regex.replace Regex.All (Regex.regex "10") (\_ -> num) getRepoPullRequests
                updated = Regex.replace Regex.All (Regex.regex "repoName") (\_ -> model.currentRepo) json
            in
                ( model, Http.send ParseRepoPullRequestJson (request updated) )

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

decodeUserPullRequests =
    decode UserPullRequests
        |> requiredAt ["data", "search", "nodes"] (Json.Decode.list decodeUserPullRequest)

decodeRepoPullRequests =
    decode RepoPullRequests
        |> requiredAt ["data", "repository", "pullRequests", "nodes"] (Json.Decode.list decodeRepoPullRequest)

decodeUserPullRequest =
    decode UserPullRequest
        |> required "__typename" string
        |> required "number" int
        |> required "title" string
        |> required "createdAt" string
        |> required "additions" int
        |> required "deletions" int
        |> requiredAt [ "repository", "name" ] string
        |> required "url" string

decodeRepoPullRequest =
    decode RepoPullRequest
        |> required "number" int
        |> required "title" string
        |> required "createdAt" string
        |> required "additions" int
        |> required "deletions" int
        |> requiredAt [ "author", "login" ] string
        |> required "url" string

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ div [] [ displayOrganization model ]
    , div [] [ displayUserPullRequests model ] 
    , div [] [ displayRepoPullRequests model ]
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
    li [] [ a [ href "#", onClick (DisplayUserData user.login) ] [ text user.login ] ]

displayRepo : Repo -> Html Msg
displayRepo repo =
    li [] [ a [ href "#", onClick (DisplayRepoData repo.name) ] [ text repo.name ] ]

displayUserPullRequests : Model -> Html Msg
displayUserPullRequests model =
    let 
        pullRequests = model.userPullRequests
    in
        case pullRequests of
            Just pullRequests ->
                div [ class "table-wrapper" ] [ div [] [ pullRequestQuantityButtons UserPullRequestButton ]
                    , table [] [ caption [] [ h1 [] [ text ("Pull Requests by " ++ model.currentUser) ] ]
                    , thead [] [ tr [] [ th [] [ text "PR" ]
                                        , th [] [ text "Title" ]
                                        , th [] [ text "Repo" ]
                                        , th [] [ text "Date Created" ]
                                        , th [] [ text "LoC" ] 
                                        ] 
                                ]
                    , tbody [] (List.map userPullRequestDataRow pullRequests.nodes)
                    ]
                ]
            Nothing ->
                div [] []

pullRequestQuantityButtons : PullRequestButton -> Html Msg
pullRequestQuantityButtons pullRequestsButton =
    case pullRequestsButton of
        UserPullRequestButton ->
            div [] [ button [ onClick (UpdateUserPullRequestQuantity "10") ] [ text "10" ]
                , button [ onClick (UpdateUserPullRequestQuantity "20") ] [ text "20" ]
                , button [ onClick (UpdateUserPullRequestQuantity "50") ] [ text "50" ]
                ]
        RepoPullRequestButton ->
            div [] [ button [ onClick (UpdateRepoPullRequestQuantity "10") ] [ text "10" ]
                , button [ onClick (UpdateRepoPullRequestQuantity "20") ] [ text "20" ]
                , button [ onClick (UpdateRepoPullRequestQuantity "50") ] [ text "50" ]
                ]

displayRepoPullRequests : Model -> Html Msg
displayRepoPullRequests model =
    let 
        pullRequests = model.repoPullRequests
    in
        case pullRequests of
            Just pullRequests ->
                div [ class "table-wrapper" ] [ div [][ pullRequestQuantityButtons RepoPullRequestButton ]
                    , table [] [ caption [] [ h1 [] [ text ("Pull Requests in " ++ model.currentRepo) ] ]
                    , thead [] [ tr [] [ th [] [ text "PR" ]
                                        , th [] [ text "Title" ]
                                        , th [] [ text "Author" ]
                                        , th [] [ text "Date Created" ]
                                        , th [] [ text "LoC" ] 
                                        ] 
                                ]
                    , tbody [] (List.map repoPullRequestDataRow pullRequests.nodes)
                    ]
                ]
            Nothing ->
                div [] []

userPullRequestDataRow : UserPullRequest -> Html Msg
userPullRequestDataRow pullRequest =
    tr [] [ th [] [ a [ href pullRequest.url, target "_blank" ] [ text (toString pullRequest.prNumber) ] ]
        , td [] [  text pullRequest.title ]
        , td [] [ text pullRequest.repoName ]
        , td [] [ text pullRequest.createdAt ]
        , td [] [ ul [ class "list-reset" ] [ li [] [ text (toString pullRequest.additions) ] ] 
        , li [] [ text (toString pullRequest.deletions) ]]
        , li [] [ text (locChange pullRequest.additions pullRequest.deletions) ]
        ]

repoPullRequestDataRow : RepoPullRequest -> Html Msg
repoPullRequestDataRow pullRequest =
    tr [] [ th [] [ a [ href pullRequest.url, target "_blank" ] [ text (toString pullRequest.prNumber) ] ]
        , td [] [  text pullRequest.title ]
        , td [] [ text pullRequest.author ]
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
        repositories(last: 3, orderBy: {field: PUSHED_AT, direction: ASC}) {
          edges {
            node {
              id
              name
            }
          }
        }
        team(slug: "fa-dev") {
          members(first: 2, orderBy: {field: LOGIN, direction: ASC}) {
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
      search(type: ISSUE, query: "org:fracturedatlas type:PR is:MERGED author:user sort:updated-desc", last: 10) {
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
      repository(owner: "FracturedAtlas", name: "repoName") {
        pullRequests(first: 10, orderBy: {field: CREATED_AT, direction: DESC}) {
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
    , userPullRequests = Nothing
    , repoPullRequests = Nothing
    , message = "Waiting for a response..." 
    , currentUser = ""
    , currentRepo = ""
    }

main : Program Never Model Msg
main =
    Html.program
        { init = init 
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

