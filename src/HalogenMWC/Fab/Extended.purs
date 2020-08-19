module HalogenMWC.Fab.Extended where

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

defaultConfig :: Config r i
defaultConfig =
        { icon: Nothing
        , trailingIcon: False
        , exited: False
        , onClick: Nothing
        , additionalAttributes: []
        }

fab :: Config r i -> String -> Html r i
fab (config_@{ additionalAttributes }) label =
    HH.element "mdc-fab"
        (Array.filterMap identity
            [ rootCs
            , HP.classes [mdc_fab mdc_fab____extended]
            , exitedCs config_
            , clickHandler config_
            , tabIndexProp 0
            ]
            <> additionalAttributes
        )
        (Array.filterMap identity
            [ rippleElt
            , leadingIconElt config_
            , labelElt label
            , trailingIconElt config_
            ]
        )

tabIndexProp :: Int -> Maybe (IProp r i)
tabIndexProp tabIndex =
    Just (HH.Attributes.property "tabIndex" (Encode.int tabIndex))

rippleElt :: Maybe (Html r i)
rippleElt =
    Just (HH.div [ HP.class_ mdc_fab__ripple ] [])

leadingIconElt :: Config r i -> Maybe (Html r i)
leadingIconElt { icon, trailingIcon } =
    case Tuple icon trailingIcon of
        Just (Tuple iconName False)  ->
            Just
                (HH.span [ HP.class_ material_icons, HP.class_ mdc_fab__icon ]
                    [ text iconName ]
                )

        _ ->
            Nothing

labelElt :: String -> Maybe (Html r i)
labelElt label =
    Just (HH.span [ HP.class_ mdc_fab__label ] [ text label ])

trailingIconElt :: Config r i -> Maybe (Html r i)
trailingIconElt { icon, trailingIcon } =
    case (Tuple icon trailingIcon) of
        ( Just iconName, True ) ->
            Just
                (HH.span [ HP.class_ material_icons, HP.class_ mdc_fab__icon ]
                    [ text iconName ]
                )

        _ ->
            Nothing

rootCs :: Maybe (IProp r i)
rootCs =
    Just (HP.class_ mdc_fab)

exitedCs :: Config r i -> Maybe (IProp r i)
exitedCs { exited } =
    if exited then
        Just (HP.class_ mdc_fab____exited)

    else
        Nothing

clickHandler :: Config r i -> Maybe (IProp r i)
clickHandler { onClick } =
    map HH.Events.onClick onClick
