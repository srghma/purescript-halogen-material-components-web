module HalogenMWC.Fab
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
        { mini :: Boolean
        , exited :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        }

config :: Config r i
config =
    Config
        { mini = False
        , exited = False
        , onClick = Nothing
        , additionalAttributes = []
        }

fab :: Config r i -> String -> Html r i
fab (config_@{ additionalAttributes }) iconName =
    HH.node "mdc-fab"
        (Array.filterMap identity
            [ rootCs
            , miniCs config_
            , exitedCs config_
            , clickHandler config_
            , tabIndexProp 0
            ]
            <> additionalAttributes
        )
        [ rippleElt
        , iconElt iconName
        ]

tabIndexProp :: Int -> Maybe (HH.Attribute r i)
tabIndexProp tabIndex =
    Just (HH.Attributes.property "tabIndex" (Encode.int tabIndex))

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_fab)

miniCs :: Config r i -> Maybe (HH.Attribute r i)
miniCs { mini } =
    if mini then
        Just (HP.class_ mdc_fab____mini)

    else
        Nothing

exitedCs :: Config r i -> Maybe (HH.Attribute r i)
exitedCs { exited } =
    if exited then
        Just (HP.class_ mdc_fab____exited)

    else
        Nothing

rippleElt :: Html r i
rippleElt =
    HH.div [ HP.class_ mdc_fab__ripple ] []

iconElt :: String -> Html r i
iconElt iconName =
    HH.span [ HP.class_ material_icons, HP.class_ mdc_fab__icon ] [ text iconName ]

clickHandler :: Config r i -> Maybe (HH.Attribute r i)
clickHandler { onClick } =
    map HH.Events.onClick onClick
