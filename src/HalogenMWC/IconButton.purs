module HalogenMWC.IconButton where

import Prelude
import DOM.HTML.Indexed as I
import Data.Maybe (Maybe(..))
import Halogen (ClassName, ElemName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.IconButton (mdc_icon_button)
import MaterialIconsFont.Classes (material_icons)

type Config i
  = { disabled :: Boolean
    , label :: Maybe String
    , additionalClasses :: Array ClassName
    , additionalAttributes :: Array (IProp I.HTMLbutton i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { disabled: false
  , label: Nothing
  , additionalClasses: []
  , additionalAttributes: []
  }

iconButton :: forall w i. Config i -> Array (HH.HTML w i) -> HH.HTML w i
iconButton config =
  HH.element (ElemName "mdc-icon-button")
    ( [ HP.classes $ [ mdc_icon_button ] <> config.additionalClasses
      , HP.tabIndex 0
      ]
        <> config.additionalAttributes
    )

iconButtonMaterialIcons :: forall w i. Config i -> String -> HH.HTML w i
iconButtonMaterialIcons config iconName =
  HH.element (ElemName "mdc-icon-button")
    ( [ HP.classes $ [ mdc_icon_button, material_icons ] <> config.additionalClasses
      , HP.tabIndex 0
      ]
        <> config.additionalAttributes
    )
    [ HH.text iconName ]
