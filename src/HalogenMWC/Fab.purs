module HalogenMWC.Fab where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes (material_icons)
import Data.Array as Array
import Halogen (ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.Fab (mdc_fab, mdc_fab____exited, mdc_fab____mini, mdc_fab__icon, mdc_fab__ripple)
import Web.UIEvent.MouseEvent (MouseEvent)

type Config i
  = { mini :: Boolean
    , exited :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onClick :: Maybe (MouseEvent -> i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { mini: false
  , exited: false
  , onClick: Nothing
  , additionalAttributes: []
  }

fab :: forall w i. Config i -> String -> HH.HTML w i
fab config iconName =
  HH.element (ElemName "mdc-fab")
    ( Array.catMaybes
        [ Just $ HP.classes
            $ [ mdc_fab ]
            <> if config.mini then
                [ mdc_fab____mini ]
              else
                []
                  <> if config.exited then [ mdc_fab____exited ] else []
        , Just $ HP.prop (PropName "tabIndex") 0
        , map HE.onClick config.onClick
        ]
        <> config.additionalAttributes
    )
    [ rippleElt
    , iconEltMaterialIcons iconName
    ]

rippleElt :: forall w i. HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_fab__ripple ] []

iconEltMaterialIcons :: forall w i. String -> HH.HTML w i
iconEltMaterialIcons iconName = HH.span [ HP.class_ material_icons, HP.class_ mdc_fab__icon ] [ HH.text iconName ]
