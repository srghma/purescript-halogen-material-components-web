module HalogenMWC.Fab.Extended
    ( Config, config




    , fab
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA






type Config r i
    =
        { icon :: Maybe String
        , trailingIcon :: Boolean
        , exited :: Boolean
        , onClick :: Maybe r i
        , additionalAttributes :: Array (IProp r i)
        }



config :: Config r i
config =
    Config
        { icon = Nothing
        , trailingIcon = False
        , exited = False
        , onClick = Nothing
        , additionalAttributes = []
        }



setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }



setTrailingIcon :: Boolean -> Config r i -> Config r i
setTrailingIcon trailingIcon (Config config_) =
    Config { config_ | trailingIcon = trailingIcon }



setExited :: Boolean -> Config r i -> Config r i
setExited exited (Config config_) =
    Config { config_ | exited = exited }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }



fab :: Config r i -> String -> Html r i
fab ((Config { additionalAttributes }) as config_) label =
    Html.node "mdc-fab"
        (Array.filterMap identity
            [ rootCs
            , extendedFabCs
            , exitedCs config_
            , clickHandler config_
            , tabIndexProp 0
            ]
            ++ additionalAttributes
        )
        (Array.filterMap identity
            [ rippleElt
            , leadingIconElt config_
            , labelElt label
            , trailingIconElt config_
            ]
        )


tabIndexProp :: Int -> Maybe (Html.Attribute r i)
tabIndexProp tabIndex =
    Just (Html.Attributes.property "tabIndex" (Encode.int tabIndex))


extendedFabCs :: Maybe (Html.Attribute r i)
extendedFabCs =
    Just (HP.class_ "mdc-fab mdc-fab--extended")


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (Html.div [ HP.class_ mdc_fab__ripple ] [])


leadingIconElt :: Config r i -> Maybe (Html r i)
leadingIconElt (Config { icon, trailingIcon }) =
    case ( icon, trailingIcon ) of
        ( Just iconName, False ) ->
            Just
                (Html.span [ HP.class_ material_icons, HP.class_ mdc_fab__icon ]
                    [ text iconName ]
                )

        _ ->
            Nothing


labelElt :: String -> Maybe (Html r i)
labelElt label =
    Just (Html.span [ HP.class_ mdc_fab__label ] [ text label ])


trailingIconElt :: Config r i -> Maybe (Html r i)
trailingIconElt (Config { icon, trailingIcon }) =
    case ( icon, trailingIcon ) of
        ( Just iconName, True ) ->
            Just
                (Html.span [ HP.class_ material_icons, HP.class_ mdc_fab__icon ]
                    [ text iconName ]
                )

        _ ->
            Nothing


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ mdc_fab)


exitedCs :: Config r i -> Maybe (Html.Attribute r i)
exitedCs (Config { exited }) =
    if exited then
        Just (HP.class_ "mdc-fab--exited")

    else
        Nothing


clickHandler :: Config r i -> Maybe (Html.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick
