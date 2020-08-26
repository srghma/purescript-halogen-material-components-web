module HalogenMWC.Switch where

import Protolude
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes
import Web.Event.Event
import Data.Array as Array
import Data.Maybe as Maybe

import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.Switch
import DOM.HTML.Indexed.InputType

type Config i =
  { checked :: Boolean
    , disabled :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onChange :: Maybe (String -> i)
    }

defaultConfig :: forall i . Config i
defaultConfig =
  { checked: false
  , disabled: false
  , additionalAttributes: []
  , onChange: Nothing
  }

switch :: forall w i . Config i -> HH.HTML w i
switch config =
  HH.element (ElemName "mdc-switch")
    ( [ HP.class_ mdc_switch
      , HP.prop (PropName "checked") config.checked
      , HP.prop (PropName "disabled") config.disabled
      ]
      <> config.additionalAttributes
    )
    [ trackElt
    , thumbUnderlayElt config
    ]

trackElt :: forall w i . HH.HTML w i
trackElt = HH.div [ HP.class_ mdc_switch__track ] []

thumbUnderlayElt :: forall w i . Config i -> HH.HTML w i
thumbUnderlayElt config = HH.div [ HP.class_ mdc_switch__thumb_underlay ] [ thumbElt config ]

thumbElt :: forall w i . Config i -> HH.HTML w i
thumbElt config = HH.div [ HP.class_ mdc_switch__thumb ] [ nativeControlElt config ]

nativeControlElt :: forall w i . Config i -> HH.HTML w i
nativeControlElt config =
  HH.input
    ( [ HP.class_ mdc_switch__native_control
      , HP.type_ InputCheckbox
      , HP.attr (AttrName "role") "switch"
      , HP.prop (PropName "checked") config.checked
      ] <> Array.catMaybes [ map HE.onValueChange config.onChange ]
    )
