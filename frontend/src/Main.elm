module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Either exposing (Either(..))
import Enums exposing (allImpressions)
import Generated.Backend as Backend exposing (Impression(..), UserExtendedInfo, UserID, UserOverviewInfo)
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (class, href)
import Html.Components exposing (navbar)
import Html.Events exposing (onClick)
import Http
import Ports exposing (swiperSlideNext)
import Routing exposing (Route(..), impressionToUrlSegement, routeToUrl, urlToRoute)
import StringExtra as String
import Swiper
import Task exposing (perform, succeed)
import Url exposing (Url)
import User exposing (UserInteractions, UserWithImpression)


type Screen
    = ScreenLoading
    | ScreenSearch (List UserWithImpression)
    | ScreenImpression Impression (List UserOverviewInfo)
    | ScreenMyProfile
    | ScreenOtherUser UserExtendedInfo
    | ScreenError (List String)


routeToScreen : AppSettings -> Route -> Cmd (Result Http.Error Screen)
routeToScreen settings route =
    case route of
        RouteSearch ->
            Backend.getUserSearch settings.backendUrl
                (Result.map (ScreenSearch << List.map (\u -> { user = u, impression = Nothing })))

        RouteImpression impression ->
            Backend.getUserImpressionsByImpression settings.backendUrl
                impression
                (Result.map (ScreenImpression impression))

        RouteMyProfile ->
            perform Ok (succeed ScreenMyProfile)

        RouteOtherUser userID ->
            Backend.getUserByUserIDProfile settings.backendUrl
                userID
                (Result.map ScreenOtherUser)


type alias Model =
    { navKey : Nav.Key
    , settings : AppSettings
    , currentScreen : Screen
    }


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | NavigateTo Route
    | ScreenLoadingResult Route (Result Http.Error Screen)
    | SetUserImpression UserID Impression
    | GotSetUserImpressionResult UserID (Result Http.Error ())


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
      , currentScreen = ScreenLoading
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
        OnUrlRequest _ ->
            ( model, Cmd.none )

        OnUrlChange url ->
            case urlToRoute url of
                Nothing ->
                    handlError "Unknown route."

                Just route ->
                    ( model
                      -- { model | currentScreen = ScreenLoading }
                    , Cmd.map (ScreenLoadingResult route) (routeToScreen model.settings route)
                    )

        NavigateTo route ->
            ( model
              -- { model | currentScreen = ScreenLoading }
            , Nav.pushUrl model.navKey (routeToUrl route)
            )

        ScreenLoadingResult _ (Ok newScreen) ->
            ( { model | currentScreen = newScreen }, Cmd.none )

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
        suffix =
            case model.currentScreen of
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

                ScreenOtherUser _ ->
                    -- TODO: Once I update the user profile code, add the name of the user here
                    "Other user"

                ScreenLoading ->
                    "Loading..."

                ScreenError _ ->
                    "Error"
    in
    { title = "Elmder" ++ " - " ++ suffix
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [ class "root" ]
        [ text "header placeholder"
        , case model.currentScreen of
            ScreenError errors ->
                div []
                    [ h1 [] [ text "We got one or more errors:" ]
                    , ul [] <| List.map (\err -> li [] [ text err ]) errors
                    , button [ onClick (NavigateTo RouteSearch) ] [ text "Clear errors" ]
                    ]

            ScreenLoading ->
                div [ class "center-content" ]
                    [ text "Loading..." ]

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

            ScreenOtherUser extInfo ->
                User.viewProfile userInteractions extInfo
        , let
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
                }

            impressionsButtons =
                case model.currentScreen of
                    ScreenImpression impression _ ->
                        let
                            mkImpressionButton impr =
                                { icon = imprIcon impr
                                , onSelect = NavigateTo <| RouteImpression impr
                                , isSelected = impr == impression
                                }
                        in
                        List.map mkImpressionButton allImpressions

                    _ ->
                        [ { icon = "fa-solid fa-heart-pulse"
                          , onSelect = NavigateTo <| RouteImpression ImpressionLike
                          , isSelected = False -- This button is only shown on the other screens
                          }
                        ]

            myProfileButton =
                { icon = "fa-solid fa-user"
                , onSelect = NavigateTo RouteMyProfile
                , isSelected =
                    case model.currentScreen of
                        ScreenMyProfile ->
                            True

                        _ ->
                            False
                }
          in
          navbar <|
            searchButton
                :: impressionsButtons
                ++ [ myProfileButton ]
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


viewScreenImpression : List UserOverviewInfo -> Html Msg
viewScreenImpression users =
    div
        [ class "user-overview scrollable" ]
        (List.map
            (\userInfo ->
                User.viewCard
                    userInteractions
                    [ onClick <| userInteractions.viewProfile userInfo.userId ]
                    { user = userInfo, impression = Nothing }
            )
            users
        )
