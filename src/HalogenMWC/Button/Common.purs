module HalogenMWC.Button.Common where

import DOM.HTML.Indexed (HTMLbutton) as I
import Data.Array as Array
import Halogen (AttrName(..), ClassName)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.Button (mdc_button, mdc_button____outlined, mdc_button____raised, mdc_button____touch, mdc_button____unelevated, mdc_button__icon, mdc_button__label, mdc_button__ripple, mdc_button__touch, mdc_touch_target_wrapper)
import MaterialIconsFont.Classes (material_icons)
import Protolude (Maybe(..))

defaultConfig
  :: forall props
   . { disabled :: Boolean
     , variant :: Variant
     , touch :: Boolean
     , additionalAttributes :: Array props
     }
defaultConfig =
  { disabled: false
  , variant: Text
  , touch: true
  , additionalAttributes: []
  }

------------------

data Variant
  = Text
  | Raised
  | Unelevated
  | Outlined

wrapTouch :: forall w i . Boolean -> HH.HTML w i -> HH.HTML w i
wrapTouch touch node =
  if touch then
    HH.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]
  else
    node

commonClasses :: Variant -> Boolean -> Array ClassName
commonClasses variant touch =
  Array.concat
    [ [ mdc_button ]
    , variantCs variant
    , if touch then [ mdc_button____touch ] else []
    ]

commonHtml :: forall w i . Boolean -> Array (HH.HTML w i) -> Array (HH.HTML w i)
commonHtml touch content = Array.concat
  [ [ rippleElt ]
  , content
  , if touch then [ touchElt ] else []
  ]

-----------------

variantCs :: Variant -> Array ClassName
variantCs variant = case variant of
  Text -> []
  Raised -> [ mdc_button____raised ]
  Unelevated -> [ mdc_button____unelevated ]
  Outlined -> [ mdc_button____outlined ]

rippleElt :: forall w i . HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_button__ripple ] []

touchElt :: forall w i . HH.HTML w i
touchElt = HH.div [ HP.class_ mdc_button__touch ] []

------------------

buttonLabel :: forall w i . String -> HH.HTML w i
buttonLabel label = HH.span [ HP.class_ mdc_button__label ] [ HH.text label ]

buttonIcon :: forall w i . String -> HH.HTML w i
buttonIcon iconName =
  HH.i
    [ HP.classes [mdc_button__icon, material_icons]
    , HP.attr (AttrName "aria-hidden") "true"
    ]
    [ HH.text iconName
    ]