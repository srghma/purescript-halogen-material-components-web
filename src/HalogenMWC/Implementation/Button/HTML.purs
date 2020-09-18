module HalogenMWC.Implementation.Button.HTML where

import Prelude
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.Button (mdc_button, mdc_button____outlined, mdc_button____raised, mdc_button____touch, mdc_button____unelevated, mdc_button__ripple, mdc_button__touch, mdc_touch_target_wrapper)

data Variant
  = Text
  | Raised
  | Unelevated
  | Outlined

commonClasses :: Variant -> Array ClassName
commonClasses = \variant -> variantCs variant <> other
  where
    variantCs :: Variant -> Array ClassName
    variantCs variant = case variant of
      Text -> []
      Raised -> [ mdc_button____raised ]
      Unelevated -> [ mdc_button____unelevated ]
      Outlined -> [ mdc_button____outlined ]

    other = [ mdc_button, mdc_button____touch ]

commonHtml :: forall w i. Array (HH.HTML w i) -> Array (HH.HTML w i)
commonHtml = \content ->
  Array.concat
    [ rippleElt
    , content
    , touchElt
    ]
  where
    rippleElt = [ HH.div [ HP.class_ mdc_button__ripple ] [] ]
    touchElt = [ HH.div [ HP.class_ mdc_button__touch ] [] ]

wrapTouch :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
wrapTouch = HH.div [ HP.class_ mdc_touch_target_wrapper ]

