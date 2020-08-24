module HalogenMWC.Button.Common where

import Material.Classes.Button (mdc_button, mdc_button____outlined, mdc_button____raised, mdc_button____touch, mdc_button____unelevated, mdc_button__icon, mdc_button__label, mdc_button__ripple, mdc_button__touch, mdc_touch_target_wrapper)
import Protolude (Maybe(..))

import Data.Array as Array
import Halogen (AttrName(..), ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import MaterialIconsFont.Classes (material_icons)

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

commonHtml :: forall configRest w i . { icon :: Maybe String, touch :: Boolean, trailingIcon :: Boolean | configRest } -> String -> Array (HH.HTML w i)
commonHtml config label = Array.concat
  [ [ rippleElt ]
  , if config.trailingIcon then [] else iconElt config.icon
  , [ labelElt label ]
  , if config.trailingIcon then iconElt config.icon else []
  , if config.touch then [ touchElt ] else []
  ]

-----------------

variantCs :: Variant -> Array ClassName
variantCs variant = case variant of
  Text -> []
  Raised -> [ mdc_button____raised ]
  Unelevated -> [ mdc_button____unelevated ]
  Outlined -> [ mdc_button____outlined ]

iconElt :: forall w i . Maybe String -> Array (HH.HTML w i)
iconElt Nothing = []
iconElt (Just iconName) =
  [ HH.i
    [ HP.classes [mdc_button__icon, material_icons]
    , HP.attr (AttrName "aria-hidden") "true"
    ]
    [ HH.text iconName ]
  ]

rippleElt :: forall w i . HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_button__ripple ] []

touchElt :: forall w i . HH.HTML w i
touchElt = HH.div [ HP.class_ mdc_button__touch ] []

labelElt :: forall w i . String -> HH.HTML w i
labelElt label = HH.span [ HP.class_ mdc_button__label ] [ HH.text label ]
