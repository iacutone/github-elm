module GitHubStats exposing (CurrentData(..), Model, Msg(..), Organization, PullRequestButton(..), Repo, RepoPullRequest, RepoPullRequests, Repos, User, UserPullRequest, UserPullRequests, Users, baseUrl, bearerTokenForm, decodeOrganization, decodeRepo, decodeRepoPullRequest, decodeRepoPullRequests, decodeRepos, decodeUser, decodeUserPullRequest, decodeUserPullRequests, decodeUsers, displayOrganization, displayRepo, displayRepoPullRequests, displayTable, displayUser, displayUserPullRequests, getRepoPullRequests, getUserPullRequests, getUsersAndRepos, init, initialModel, locChange, main, pullRequestQuantityButtons, repoPullRequestDataRow, request, site, update, userPullRequestDataRow, view)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, scope, target, title)
import Html.Attributes.A11y exposing (columnHeader, group, labelledBy, search)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (at, decodeString, field, int, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required, requiredAt)
import Json.Encode as Encode
import Regex exposing (regex, replace)


type alias Model =
    { message : String
    , organization : Maybe Organization
    , userPullRequests : Maybe UserPullRequests
    , repoPullRequests : Maybe RepoPullRequests
    , currentData : CurrentData
    , currentUser : String
    , currentRepo : String
    , bearerToken : Maybe String
    , bearerTokenInput : String
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
    { id : String
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
    { typename : String
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


type CurrentData
    = ByUser
    | ByRepo


type PullRequestButton
    = UserPullRequestButton
    | RepoPullRequestButton



-- UPDATE


type Msg
    = ParseOrgJson (Result Http.Error String)
    | ParseUserPullRequestJson (Result Http.Error String)
    | ParseRepoPullRequestJson (Result Http.Error String)
    | DisplayUserData String
    | DisplayRepoData String
    | UpdateUserPullRequestQuantity String
    | UpdateRepoPullRequestQuantity String
    | UpdateBearerToken String
    | UpdateBearerTokenInput String
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParseOrgJson (Ok res) ->
            case decodeString decodeOrganization res of
                Ok res ->
                    ( { model | organization = Just res, message = "" }, Cmd.none )

                Err error ->
                    ( { model | message = error, organization = Nothing }, Cmd.none )

        ParseOrgJson (Err res) ->
            ( { model | organization = Nothing }, Cmd.none )

        DisplayUserData user ->
            let
                json =
                    Regex.replace Regex.All (Regex.regex "user") (\_ -> user) getUserPullRequests

                token =
                    model.bearerToken
            in
            case token of
                Just token ->
                    ( { model | currentUser = user, currentData = ByUser }, Http.send ParseUserPullRequestJson (request token json) )

                Nothing ->
                    ( { model | bearerToken = Nothing }, Cmd.none )

        DisplayRepoData repo ->
            let
                json =
                    Regex.replace Regex.All (Regex.regex "repoName") (\_ -> repo) getRepoPullRequests

                token =
                    model.bearerToken
            in
            case token of
                Just token ->
                    ( { model | currentRepo = repo, currentData = ByRepo }, Http.send ParseRepoPullRequestJson (request token json) )

                Nothing ->
                    ( { model | bearerToken = Nothing }, Cmd.none )

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
                json =
                    Regex.replace Regex.All (Regex.regex "10") (\_ -> num) getUserPullRequests

                updated =
                    Regex.replace Regex.All (Regex.regex "user") (\_ -> model.currentUser) json

                token =
                    model.bearerToken
            in
            case token of
                Just token ->
                    ( model, Http.send ParseUserPullRequestJson (request token updated) )

                Nothing ->
                    ( { model | bearerToken = Nothing }, Cmd.none )

        UpdateRepoPullRequestQuantity num ->
            let
                json =
                    Regex.replace Regex.All (Regex.regex "10") (\_ -> num) getRepoPullRequests

                updated =
                    Regex.replace Regex.All (Regex.regex "repoName") (\_ -> model.currentRepo) json

                token =
                    model.bearerToken
            in
            case token of
                Just token ->
                    ( model, Http.send ParseRepoPullRequestJson (request token updated) )

                Nothing ->
                    ( { model | bearerToken = Nothing }, Cmd.none )

        UpdateBearerToken token ->
            let
                bearerToken =
                    "Bearer " ++ token
            in
            ( { model | bearerToken = Just bearerToken }, Http.send ParseOrgJson (request bearerToken getUsersAndRepos) )

        UpdateBearerTokenInput input ->
            ( { model | bearerTokenInput = input }, Cmd.none )

        None ->
            ( model, Cmd.none )


decodeOrganization =
    decode Organization
        |> requiredAt [ "data", "organization", "team", "members", "edges" ] (Json.Decode.list decodeUsers)
        |> requiredAt [ "data", "organization", "repositories", "edges" ] (Json.Decode.list decodeRepos)


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
        |> requiredAt [ "data", "search", "nodes" ] (Json.Decode.list decodeUserPullRequest)


decodeRepoPullRequests =
    decode RepoPullRequests
        |> requiredAt [ "data", "repository", "pullRequests", "nodes" ] (Json.Decode.list decodeRepoPullRequest)


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
    div [ class "site-wrapper" ] (site model)


site model =
    let
        token =
            model.bearerToken
    in
    case token of
        Just token ->
            [ displayOrganization model
            , main_ []
                [ displayTable model ]
            ]

        Nothing ->
            [ bearerTokenForm model ]


bearerTokenForm : Model -> Html Msg
bearerTokenForm model =
    div []
        [ input
            [ Html.Attributes.type_ "text"
            , onInput UpdateBearerTokenInput
            ]
            []
        , button
            [ onClick (UpdateBearerToken model.bearerTokenInput) ]
            [ text "Go!" ]
        ]


displayOrganization : Model -> Html Msg
displayOrganization model =
    let
        organization =
            model.organization
    in
    case organization of
        Just organization ->
            let
                users =
                    organization.users

                repos =
                    organization.repos

                userNodes =
                    List.map .node users

                repoNodes =
                    List.map .node repos
            in
            header [ class "is-visible", search ]
                [ div [ class "wrapper" ]
                    [ h2 [] [ text "Filter By" ]
                    , section [ class "list-authors" ]
                        [ h3 [] [ text "Author" ]
                        , ul [ class "list-reset" ] (List.map displayUser userNodes)
                        ]
                    , section [ class "list-repos" ]
                        [ h3 [] [ text "Repo" ]
                        , ul [ class "list-reset" ] (List.map displayRepo repoNodes)
                        ]
                    ]
                ]

        Nothing ->
            div [] []


displayUser : User -> Html Msg
displayUser user =
    li [] [ a [ href "#", onClick (DisplayUserData user.login) ] [ text user.login ] ]


displayRepo : Repo -> Html Msg
displayRepo repo =
    li [] [ a [ href "#", onClick (DisplayRepoData repo.name) ] [ text repo.name ] ]


displayTable : Model -> Html Msg
displayTable model =
    case model.currentData of
        ByUser ->
            displayUserPullRequests model

        ByRepo ->
            displayRepoPullRequests model


displayUserPullRequests : Model -> Html Msg
displayUserPullRequests model =
    let
        pullRequests =
            model.userPullRequests
    in
    case pullRequests of
        Just pullRequests ->
            div [ class "table-wrapper" ]
                [ pullRequestQuantityButtons UserPullRequestButton
                , table [ labelledBy "table-header" ]
                    [ caption [ id "table-header" ] [ h1 [] [ text ("Pull Requests by " ++ model.currentUser) ] ]
                    , thead []
                        [ tr []
                            [ th [ class "th-prno", columnHeader, scope "col" ] [ text "PR" ]
                            , th [ class "th-title", columnHeader, scope "col" ] [ text "Title" ]
                            , th [ class "th-repo", columnHeader, scope "col" ] [ text "Repo" ]
                            , th [ class "th-date", columnHeader, scope "col" ] [ text "Date Created" ]
                            , th [ class "th-loc", columnHeader, scope "col" ] [ text "LoC" ]
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
            div [ class "button-wrapper", group ]
                [ label [] [ text "Rows " ]
                , button [ class "btn", onClick (UpdateUserPullRequestQuantity "10") ] [ text "10" ]
                , button [ class "btn", onClick (UpdateUserPullRequestQuantity "20") ] [ text "20" ]
                , button [ class "btn", onClick (UpdateUserPullRequestQuantity "50") ] [ text "50" ]
                ]

        RepoPullRequestButton ->
            div [ class "button-wrapper", group ]
                [ label [] [ text "Rows " ]
                , button [ class "btn", onClick (UpdateRepoPullRequestQuantity "10") ] [ text "10" ]
                , button [ class "btn", onClick (UpdateRepoPullRequestQuantity "20") ] [ text "20" ]
                , button [ class "btn", onClick (UpdateRepoPullRequestQuantity "50") ] [ text "50" ]
                ]


displayRepoPullRequests : Model -> Html Msg
displayRepoPullRequests model =
    let
        pullRequests =
            model.repoPullRequests
    in
    case pullRequests of
        Just pullRequests ->
            div [ class "table-wrapper" ]
                [ pullRequestQuantityButtons RepoPullRequestButton
                , table [ labelledBy "table-header" ]
                    [ caption [ id "table-header" ] [ h1 [] [ text ("Pull Requests in " ++ model.currentRepo) ] ]
                    , thead []
                        [ tr []
                            [ th [ class "th-prno", columnHeader, scope "col" ] [ text "PR" ]
                            , th [ class "th-title", columnHeader, scope "col" ] [ text "Title" ]
                            , th [ class "th-author", columnHeader, scope "col" ] [ text "Author" ]
                            , th [ class "th-date", columnHeader, scope "col" ] [ text "Date Created" ]
                            , th [ class "th-loc", columnHeader, scope "col" ] [ text "LoC" ]
                            ]
                        ]
                    , tbody [] (List.map repoPullRequestDataRow pullRequests.nodes)
                    ]
                ]

        Nothing ->
            div [] []


userPullRequestDataRow : UserPullRequest -> Html Msg
userPullRequestDataRow pullRequest =
    tr []
        [ th [ attribute "data-title" "pr-no", scope "row" ] [ a [ href pullRequest.url, target "_blank", title "View this PR on GitHub" ] [ text (toString pullRequest.prNumber) ] ]
        , td [ attribute "data-title" "pr-title" ] [ text pullRequest.title ]
        , td [ attribute "data-title" "repo-title" ] [ text pullRequest.repoName ]
        , td [ attribute "data-title" "pr-date" ] [ text pullRequest.createdAt ]
        , td [ attribute "data-title" "pr-loc" ]
            [ ul [ class "list-reset" ]
                [ li [ attribute "aria-label" "LoC Differences in this PR", title "Difference" ] [ text (locChange pullRequest.additions pullRequest.deletions) ]
                , li [ attribute "aria-label" "LoC Additions in this PR", title "Additions" ] [ text (toString pullRequest.additions) ]
                , li [ attribute "aria-label" "LoC Deletions in this PR", title "Deletions" ] [ text (toString pullRequest.deletions) ]
                ]
            ]
        ]


repoPullRequestDataRow : RepoPullRequest -> Html Msg
repoPullRequestDataRow pullRequest =
    tr []
        [ th [ attribute "data-title" "pr-no", scope "row" ] [ a [ href pullRequest.url, target "_blank", title "View this PR on GitHub" ] [ text (toString pullRequest.prNumber) ] ]
        , td [ attribute "data-title" "pr-title" ] [ text pullRequest.title ]
        , td [ attribute "data-title" "pr-author" ] [ text pullRequest.author ]
        , td [ attribute "data-title" "pr-date" ] [ text pullRequest.createdAt ]
        , td [ attribute "data-title" "pr-loc" ]
            [ ul [ class "list-reset" ]
                [ li [ attribute "aria-label" "LoC Differences in this PR", title "Difference" ] [ text (locChange pullRequest.additions pullRequest.deletions) ]
                , li [ attribute "aria-label" "LoC Additions in this PR", title "Additions" ] [ text (toString pullRequest.additions) ]
                , li [ attribute "aria-label" "LoC Deletions in this PR", title "Deletions" ] [ text (toString pullRequest.deletions) ]
                ]
            ]
        ]


locChange : Int -> Int -> String
locChange additions deletions =
    let
        diff =
            additions - deletions
    in
    toString diff


request : String -> String -> Http.Request String
request token query =
    let
        headers =
            [ Http.header "Authorization" token
            ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = baseUrl
        , body = Http.jsonBody (Encode.object [ ( "query", Encode.string query ) ])
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


getUsersAndRepos : String
getUsersAndRepos =
    """
    query {
      organization(login: "FracturedAtlas") {
        repositories(first: 11, orderBy: {field: UPDATED_AT, direction: DESC}) {
          edges {
            node {
              id
              name
            }
          }
        }
        team(slug: "fa-dev") {
          members(first: 10, orderBy: {field: LOGIN, direction: ASC}) {
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


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { organization = Nothing
    , userPullRequests = Nothing
    , repoPullRequests = Nothing
    , message = "Waiting for a response..."
    , currentData = ByUser
    , currentUser = ""
    , currentRepo = ""
    , bearerToken = Nothing
    , bearerTokenInput = ""
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
