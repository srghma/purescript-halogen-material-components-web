module HalogenMWC.Switch where

import Protolude (Maybe(..), map, (<>))
import DOM.HTML.Indexed as I
import Data.Array as Array

import Halogen (AttrName(..), ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.Switch (mdc_switch, mdc_switch__native_control, mdc_switch__thumb, mdc_switch__thumb_underlay, mdc_switch__track)
import DOM.HTML.Indexed.InputType (InputType(..))

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
