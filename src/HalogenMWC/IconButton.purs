module HalogenMWC.IconButton
    ( Config, config

    , iconButton
    , custom
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { disabled :: Boolean
    , label :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    }

defaultConfig :: Config r i
defaultConfig =
        { disabled: False
        , label: Nothing
        , additionalAttributes: []
        , onClick: Nothing
        }

iconButton :: Config r i -> String -> Html r i
iconButton config_ iconName =
    HH.node "mdc-icon-button"
        (Array.filterMap identity
            [ rootCs
            , materialIconsCs
            , tabIndexProp
            , clickHandler config_
            ]
            <> config_.additionalAttributes
        )
        [ text iconName ]

custom :: Config r i -> Array (Html r i) -> Html r i
custom config_ nodes =
    HH.node "mdc-icon-button"
        (Array.filterMap identity
            [ rootCs
            , tabIndexProp
            , clickHandler config_
            ]
            <> config_.additionalAttributes
        )
        nodes

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_icon_button)

materialIconsCs :: Maybe (HH.Attribute r i)
materialIconsCs =
    Just (HP.class_ material_icons)

tabIndexProp :: Maybe (HH.Attribute r i)
tabIndexProp =
    Just (HH.Attributes.tabindex 0)

clickHandler :: Config r i -> Maybe (HH.Attribute r i)
clickHandler config_ =
    map HH.Events.onClick config_.onClick
