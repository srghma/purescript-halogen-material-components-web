module HalogenMWC.IconButton where

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
  { disabled: false
  , label: Nothing
  , additionalAttributes: []
  , onClick: Nothing
  }

iconButton :: Config r i -> String -> HH.HTML w i
iconButton config_ iconName =
  HH.element "mdc-icon-button"
    ( Array.catMaybes
        [ rootCs
        , materialIconsCs
        , tabIndexProp
        , clickHandler config_
        ]
        <> config_.additionalAttributes
    )
    [ text iconName ]

custom :: Config r i -> Array (HH.HTML w i) -> HH.HTML w i
custom config_ nodes =
  HH.element "mdc-icon-button"
    ( Array.catMaybes
        [ rootCs
        , tabIndexProp
        , clickHandler config_
        ]
        <> config_.additionalAttributes
    )
    nodes

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_icon_button)

materialIconsCs :: Maybe (IProp r i)
materialIconsCs = Just (HP.class_ material_icons)

tabIndexProp :: Maybe (IProp r i)
tabIndexProp = Just (HH.Attributes.tabindex 0)

clickHandler :: Config r i -> Maybe (IProp r i)
clickHandler config_ = map HH.Events.onClick config_.onClick
