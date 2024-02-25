module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport, getViewport, setViewport)
import Browser.Navigation as Nav
import Either exposing (Either(..))
import Enums exposing (allImpressions)
import Generated.Backend as Backend exposing (Impression(..), UserExtendedInfo, UserID, UserOverviewInfo)
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Http
import List.Extra as List
import Maybe.Extra exposing (isJust)
import Ports exposing (swiperSlideNext)
import Routing exposing (Route(..), impressionToUrlSegement, routeToUrl, urlToRoute)
import StringExtra as String
import Swiper
import Task exposing (perform, succeed)
import Url exposing (Url)
import User exposing (UserInteractions, UserWithImpression)


type Screen
    = ScreenInitialLoading -- The very first loading of the app
    | ScreenLoading Screen
      -- ^ When loading a new screen, we keep the old one around so we can render a nicer transition
    | ScreenSearch (List UserWithImpression)
    | ScreenImpression Impression (List UserWithImpression)
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

        ( ScreenImpression imprA _, ScreenImpression imprB _ ) ->
            imprA == imprB

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

        ( ScreenImpression _ _, _ ) ->
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
            Backend.getUserSearch settings.backendUrl
                (Result.map (ScreenSearch << List.map (\u -> { user = u, impression = Nothing })))

        RouteImpression impression ->
            Backend.getUserImpressionsByImpression settings.backendUrl
                impression
                (Result.map (ScreenImpression impression << List.map (\u -> { user = u, impression = Just impression })))

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
        updateUser userWithImpression =
            if userWithImpression.user.userId == userID then
                { userWithImpression | impression = impression }

            else
                userWithImpression

        newScreen =
            case model.currentScreen of
                ScreenSearch users ->
                    ScreenSearch <| List.map updateUser users

                ScreenImpression impr users ->
                    ScreenImpression impr <| List.map updateUser users

                ScreenOtherUser info extInfo ->
                    if info.userId == userID then
                        ScreenOtherUser info { extInfo | userExtImpression = impression }

                    else
                        model.currentScreen

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
                                    "search page"

                                RouteImpression impression ->
                                    impressionToUrlSegement impression ++ " page"

                                RouteMyProfile ->
                                    "my profile"

                                RouteOtherUser _ ->
                                    "other user's page"
                           )
            in
            handlHttpError prefix error

        SetUserImpression userID impression ->
            ( updateImpression userID (Just impression) model
            , Cmd.batch
                [ Backend.postUserByImpressionByOtherUserID model.settings.backendUrl impression userID (GotSetUserImpressionResult userID)
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

                ScreenImpression impression _ ->
                    case impression of
                        ImpressionLike ->
                            "Likes"

                        ImpressionDislike ->
                            "Dislikes"

                        ImpressionDecideLater ->
                            "Decide Later"

                        ImpressionSuperLike ->
                            "Super-Likes"

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

                ScreenImpression _ users ->
                    viewScreenImpression users

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
                imprIcon impr =
                    case impr of
                        ImpressionLike ->
                            "fa-solid fa-heart"

                        ImpressionDislike ->
                            "fa-solid fa-heart-broken"

                        ImpressionDecideLater ->
                            "fa-solid fa-clock"

                        ImpressionSuperLike ->
                            "fa-solid fa-hand-holding-heart"

                searchButton =
                    { icon = "fa-solid fa-magnifying-glass"
                    , onSelect = NavigateTo RouteSearch
                    , isSelected =
                        case model.currentScreen of
                            ScreenSearch _ ->
                                True

                            _ ->
                                False
                    , isVisible = True
                    }

                impressionsButtons =
                    let
                        currentImpression =
                            case model.currentScreen of
                                ScreenImpression impr _ ->
                                    Just impr

                                ScreenLoading (ScreenImpression impr _) ->
                                    Just impr

                                _ ->
                                    Nothing
                    in
                    let
                        mkImpressionButton impr =
                            { icon = imprIcon impr
                            , onSelect = NavigateTo <| RouteImpression impr
                            , isSelected = Just impr == currentImpression
                            , isVisible = isJust currentImpression || impr == ImpressionLike
                            }
                    in
                    List.map mkImpressionButton allImpressions

                myProfileButton =
                    { icon = "fa-solid fa-user"
                    , onSelect = NavigateTo RouteMyProfile
                    , isSelected =
                        case model.currentScreen of
                            ScreenMyProfile ->
                                True

                            _ ->
                                False
                    , isVisible = True
                    }
            in
            navbar <|
                searchButton
                    :: impressionsButtons
                    ++ [ myProfileButton ]
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


viewSearch : List UserWithImpression -> Html Msg
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


viewScreenImpression : List UserWithImpression -> Html Msg
viewScreenImpression users =
    div
        [ class "user-overview" ]
        (List.map
            (\{ user, impression } ->
                User.viewCard
                    userInteractions
                    [ onClick <| userInteractions.viewProfile user.userId ]
                    { user = user, impression = impression }
            )
            users
        )
