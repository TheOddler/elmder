module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport, getViewport, setViewport)
import Browser.Navigation as Nav
import Either exposing (Either(..))
import Generated.Backend as Backend exposing (Impression(..), UserExtendedInfo, UserID, UserOverviewInfo)
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Http
import List.Extra as List
import Ports exposing (swiperSlideNext)
import Routing exposing (Route(..), routeToUrl, urlToRoute)
import StringExtra as String
import Swiper
import Task exposing (perform, succeed)
import Url exposing (Url)
import User exposing (UserInteractions)


type Screen
    = ScreenInitialLoading -- The very first loading of the app
    | ScreenLoading Screen
      -- ^ When loading a new screen, we keep the old one around so we can render a nicer transition
    | ScreenSearch (List UserOverviewInfo)
    | ScreenLikesAndSuperLikes (List UserOverviewInfo)
    | ScreenDislikes (List UserOverviewInfo)
    | ScreenDecideLater (List UserOverviewInfo)
    | ScreenMatches (List UserOverviewInfo)
    | ScreenAdmirers (List UserOverviewInfo)
    | ScreenMyProfile
    | ScreenOtherUser UserOverviewInfo UserExtendedInfo
    | ScreenError (List String)


isSameScreen : Screen -> Screen -> Bool
isSameScreen a b =
    case ( a, b ) of
        ( ScreenInitialLoading, ScreenInitialLoading ) ->
            True

        ( ScreenLoading a_, ScreenLoading b_ ) ->
            isSameScreen a_ b_

        ( ScreenSearch _, ScreenSearch _ ) ->
            True

        ( ScreenLikesAndSuperLikes _, ScreenLikesAndSuperLikes _ ) ->
            True

        ( ScreenDislikes _, ScreenDislikes _ ) ->
            True

        ( ScreenDecideLater _, ScreenDecideLater _ ) ->
            True

        ( ScreenMatches _, ScreenMatches _ ) ->
            True

        ( ScreenAdmirers _, ScreenAdmirers _ ) ->
            True

        ( ScreenMyProfile, ScreenMyProfile ) ->
            True

        ( ScreenOtherUser infoA _, ScreenOtherUser infoB _ ) ->
            infoA.userId == infoB.userId

        ( ScreenError _, ScreenError _ ) ->
            True

        -- Don't use a default case here, so that we get a compiler warning if we add a new screen
        ( ScreenInitialLoading, _ ) ->
            False

        ( ScreenLoading _, _ ) ->
            False

        ( ScreenSearch _, _ ) ->
            False

        ( ScreenLikesAndSuperLikes _, _ ) ->
            False

        ( ScreenDislikes _, _ ) ->
            False

        ( ScreenDecideLater _, _ ) ->
            False

        ( ScreenMatches _, _ ) ->
            False

        ( ScreenAdmirers _, _ ) ->
            False

        ( ScreenMyProfile, _ ) ->
            False

        ( ScreenOtherUser _ _, _ ) ->
            False

        ( ScreenError _, _ ) ->
            False


routeToScreen : AppSettings -> Route -> Cmd (Result Http.Error Screen)
routeToScreen settings route =
    case route of
        RouteSearch ->
            Backend.getUserSearch settings.backendUrl (Result.map ScreenSearch)

        RouteLikesAndSuperLikes ->
            Backend.postUserImpressions settings.backendUrl [ ImpressionLike, ImpressionSuperLike ] (Result.map ScreenLikesAndSuperLikes)

        RouteDislikes ->
            Backend.postUserImpressions settings.backendUrl [ ImpressionDislike ] (Result.map ScreenDislikes)

        RouteDecideLater ->
            Backend.postUserImpressions settings.backendUrl [ ImpressionDecideLater ] (Result.map ScreenDecideLater)

        RouteMatches ->
            Backend.getUserMatches settings.backendUrl (Result.map ScreenMatches)

        RouteAdmirers ->
            Backend.getUserAdmirers settings.backendUrl (Result.map ScreenAdmirers)

        RouteMyProfile ->
            perform Ok (succeed ScreenMyProfile)

        RouteOtherUser userID ->
            Backend.getUserByUserIDInfo settings.backendUrl
                userID
                (Result.map (\allInfo -> ScreenOtherUser allInfo.userAllOverviewInfo allInfo.userAllExtendedInfo))


type alias ScrollPosition =
    Float


type alias Model =
    { navKey : Nav.Key
    , settings : AppSettings
    , currentScreen : Screen
    , screenStack : List ( Screen, ScrollPosition )
    }


type Msg
    = NoOp
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | NavigateTo Route
    | ScreenLoadingResult Route (Result Http.Error Screen)
    | SetUserImpression UserID Impression
    | GotSetUserImpressionResult UserID (Result Http.Error ())
    | SaveScrollPositionAndContinue (Cmd Msg) Viewport


type alias AppSettings =
    { backendUrl : String
    }


main : Program AppSettings Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : AppSettings -> Url -> Nav.Key -> ( Model, Cmd Msg )
init settings url navKey =
    ( { navKey = navKey
      , settings = settings
      , currentScreen = ScreenInitialLoading
      , screenStack = []
      }
    , case urlToRoute url of
        Nothing ->
            perform identity (succeed <| NavigateTo RouteSearch)

        Just route ->
            Cmd.map (ScreenLoadingResult route) (routeToScreen settings route)
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


updateImpression : UserID -> Maybe Impression -> Model -> Model
updateImpression userID impression model =
    let
        updateUser user =
            if user.userId == userID then
                { user | userImpression = impression }

            else
                user

        newScreen =
            case model.currentScreen of
                ScreenSearch users ->
                    ScreenSearch <| List.map updateUser users

                ScreenLikesAndSuperLikes users ->
                    ScreenLikesAndSuperLikes <| List.map updateUser users

                ScreenDislikes users ->
                    ScreenDislikes <| List.map updateUser users

                ScreenDecideLater users ->
                    ScreenDecideLater <| List.map updateUser users

                ScreenMatches users ->
                    ScreenMatches <| List.map updateUser users

                ScreenAdmirers users ->
                    ScreenAdmirers <| List.map updateUser users

                ScreenOtherUser info extInfo ->
                    ScreenOtherUser (updateUser info) extInfo

                _ ->
                    model.currentScreen
    in
    { model | currentScreen = newScreen }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        existingErrors =
            case model.currentScreen of
                ScreenError errors ->
                    errors

                _ ->
                    []

        handlError error =
            ( { model | currentScreen = ScreenError (error :: existingErrors) }
            , Cmd.none
            )

        handlHttpError prefix error =
            handlError <| prefix ++ ": " ++ httpErrorToString error
    in
    case message of
        NoOp ->
            ( model, Cmd.none )

        SaveScrollPositionAndContinue cmd vp ->
            let
                cleanedScreenStack =
                    List.filter (not << isSameScreen model.currentScreen << Tuple.first) model.screenStack

                newScreenStack =
                    ( model.currentScreen, vp.viewport.y ) :: cleanedScreenStack
            in
            ( { model | screenStack = newScreenStack }, cmd )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    -- TODO: Consider showing a "you are leaving the app, are you sure?" page
                    ( model, Nav.load href )

        OnUrlChange url ->
            case urlToRoute url of
                Nothing ->
                    handlError "Unknown route."

                Just route ->
                    ( { model | currentScreen = ScreenLoading model.currentScreen }
                    , Cmd.map (ScreenLoadingResult route) (routeToScreen model.settings route)
                    )

        NavigateTo route ->
            ( model
            , Task.perform (SaveScrollPositionAndContinue <| Nav.pushUrl model.navKey (routeToUrl route)) getViewport
            )

        ScreenLoadingResult _ (Ok newScreen) ->
            ( { model | currentScreen = newScreen }
            , case List.find (isSameScreen newScreen << Tuple.first) model.screenStack of
                Nothing ->
                    Cmd.none

                Just ( _, pos ) ->
                    Task.perform (\() -> NoOp) (setViewport 0 pos)
            )

        ScreenLoadingResult route (Err error) ->
            let
                prefix =
                    "Failed navigating to "
                        ++ (case route of
                                RouteSearch ->
                                    "the search page"

                                RouteLikesAndSuperLikes ->
                                    "the likes page"

                                RouteDislikes ->
                                    "the dislikes page"

                                RouteDecideLater ->
                                    "the decide later page"

                                RouteMatches ->
                                    "the matches page"

                                RouteAdmirers ->
                                    "the admirers page"

                                RouteMyProfile ->
                                    "my profile"

                                RouteOtherUser _ ->
                                    "an other user's page"
                           )
            in
            handlHttpError prefix error

        SetUserImpression userID impression ->
            ( updateImpression userID (Just impression) model
            , Cmd.batch
                [ Backend.postUserImpressionByOtherUserID model.settings.backendUrl userID impression (GotSetUserImpressionResult userID)
                , swiperSlideNext <| Just "search-swiper"
                ]
            )

        GotSetUserImpressionResult _ (Ok ()) ->
            ( model
              -- The user was already optimistically updated
            , Cmd.none
            )

        GotSetUserImpressionResult userID (Err error) ->
            let
                ( newModel, nextCmd ) =
                    handlHttpError "Failed registering user impression" error
            in
            ( updateImpression userID Nothing newModel
            , nextCmd
            )


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "BadUrl: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus code ->
            "BadStatus: " ++ String.fromInt code

        Http.BadBody body ->
            "BadBody: " ++ body


view : Model -> Browser.Document Msg
view model =
    let
        titleSuffixFor screen =
            case screen of
                ScreenInitialLoading ->
                    "Loading..."

                ScreenSearch _ ->
                    "Search"

                ScreenLikesAndSuperLikes _ ->
                    "Likes"

                ScreenDislikes _ ->
                    "Dislikes"

                ScreenDecideLater _ ->
                    "Decide Later"

                ScreenMatches _ ->
                    "Matches"

                ScreenAdmirers _ ->
                    "Admirers"

                ScreenMyProfile ->
                    "My profile"

                ScreenOtherUser user _ ->
                    user.userName

                ScreenLoading prev ->
                    titleSuffixFor prev

                ScreenError _ ->
                    "Error"
    in
    { title = "Elmder" ++ " - " ++ titleSuffixFor model.currentScreen
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    let
        viewScreen screen =
            case screen of
                ScreenError errors ->
                    div []
                        [ h1 [] [ text "We got one or more errors:" ]
                        , ul [] <| List.map (\err -> li [] [ text err ]) errors
                        , button [ onClick (NavigateTo RouteSearch) ] [ text "Clear errors" ]
                        ]

                ScreenInitialLoading ->
                    div [ class "center-content" ]
                        [ text "Loading..." ]

                ScreenLoading prev ->
                    viewScreen prev

                ScreenSearch foundUsers ->
                    viewSearch foundUsers

                ScreenLikesAndSuperLikes users ->
                    viewScreenUserOverview users

                ScreenDislikes users ->
                    viewScreenUserOverview users

                ScreenDecideLater users ->
                    viewScreenUserOverview users

                ScreenMatches users ->
                    viewScreenUserOverview users

                ScreenAdmirers users ->
                    viewScreenUserOverview users

                ScreenMyProfile ->
                    div [ class "center-content" ] <|
                        text "Your profile will come here. But for now there's just this palceholder and the attributions."
                            :: h1 [] [ text "We use:" ]
                            :: List.map (\( label, url ) -> a [ href url ] [ text label ])
                                [ ( "loading.io", "https://loading.io/" )
                                , ( "Font Awesome", "https://fontawesome.com/" )
                                ]

                ScreenOtherUser info extInfo ->
                    User.viewProfile userInteractions info extInfo

        viewNavBar =
            let
                routeIcon route =
                    case route of
                        RouteSearch ->
                            "fa-solid fa-magnifying-glass"

                        RouteLikesAndSuperLikes ->
                            "fa-regular fa-heart"

                        RouteDislikes ->
                            "fa-solid fa-heart-broken"

                        RouteDecideLater ->
                            "fa-solid fa-clock"

                        RouteMatches ->
                            "fa-solid fa-heart"

                        RouteAdmirers ->
                            "fa-solid fa-hand-holding-heart"

                        RouteMyProfile ->
                            "fa-solid fa-user"

                        -- Not actually used, but we need to cover all cases
                        RouteOtherUser _ ->
                            "fa-regular fa-user"

                screenRoute screen =
                    case screen of
                        ScreenInitialLoading ->
                            RouteSearch

                        ScreenSearch _ ->
                            RouteSearch

                        ScreenLikesAndSuperLikes _ ->
                            RouteLikesAndSuperLikes

                        ScreenDislikes _ ->
                            RouteDislikes

                        ScreenDecideLater _ ->
                            RouteDecideLater

                        ScreenMatches _ ->
                            RouteMatches

                        ScreenAdmirers _ ->
                            RouteAdmirers

                        ScreenMyProfile ->
                            RouteMyProfile

                        ScreenOtherUser info _ ->
                            RouteOtherUser info.userId

                        ScreenLoading prev ->
                            screenRoute prev

                        ScreenError _ ->
                            RouteSearch

                alwaysVisible route =
                    case route of
                        RouteSearch ->
                            True

                        RouteLikesAndSuperLikes ->
                            False

                        RouteDislikes ->
                            False

                        RouteDecideLater ->
                            False

                        RouteMatches ->
                            True

                        RouteAdmirers ->
                            False

                        RouteMyProfile ->
                            True

                        RouteOtherUser _ ->
                            False

                isLoveRoute route =
                    case route of
                        RouteSearch ->
                            False

                        RouteLikesAndSuperLikes ->
                            True

                        RouteDislikes ->
                            True

                        RouteDecideLater ->
                            True

                        RouteMatches ->
                            True

                        RouteAdmirers ->
                            True

                        RouteMyProfile ->
                            False

                        RouteOtherUser _ ->
                            False

                mkButton route =
                    { icon = routeIcon route
                    , onSelect = NavigateTo route
                    , isSelected = route == screenRoute model.currentScreen
                    , isVisible =
                        alwaysVisible route
                            || (isLoveRoute route && isLoveRoute (screenRoute model.currentScreen))
                    }
            in
            navbar
                [ mkButton RouteSearch
                , mkButton RouteDislikes
                , mkButton RouteLikesAndSuperLikes
                , mkButton RouteMatches
                , mkButton RouteAdmirers
                , mkButton RouteDecideLater
                , mkButton RouteMyProfile
                ]
    in
    Keyed.node "div"
        [ class "root" ]
    <|
        [ ( "screen", viewScreen model.currentScreen )
        , ( "loading"
          , case model.currentScreen of
                ScreenLoading _ ->
                    div [ class "loading-overlay" ] [ text "Loading..." ]

                _ ->
                    text ""
          )
        , ( "navbar", viewNavBar )
        ]


userInteractions : UserInteractions Msg
userInteractions =
    { setImpression = SetUserImpression
    , viewProfile = NavigateTo << RouteOtherUser
    }


viewSearch : List UserOverviewInfo -> Html Msg
viewSearch users =
    let
        viewSlide userWithImpr =
            User.viewCardAsSwiperSlide
                userInteractions
                userWithImpr
    in
    div [ class "search-view" ]
        [ Swiper.container
            [ Swiper.id "search-swiper"
            , Swiper.effect Swiper.EffectCards
            ]
            (List.map viewSlide users)
        ]


viewScreenUserOverview : List UserOverviewInfo -> Html Msg
viewScreenUserOverview users =
    div
        [ class "user-overview" ]
        (List.map
            (\user ->
                User.viewCard
                    userInteractions
                    [ onClick <| userInteractions.viewProfile user.userId ]
                    user
            )
            users
        )
