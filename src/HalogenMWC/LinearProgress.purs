module HalogenMWC.LinearProgress where

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
import Material.Classes.LinearProgress

type Config i =
  { reverse :: Boolean
  , closed :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  }

data Variant
  = Indeterminate
  | Determinate Number
  | Buffered Number Number

defaultConfig :: forall i . Config i
defaultConfig =
  { reverse: false
  , closed: false
  , additionalAttributes: []
  }

linearProgress :: forall w i . Variant -> Config i -> HH.HTML w i
linearProgress variant config =
  HH.element (ElemName "mdc-linear-progress")
    ( [ HP.classes $ Array.catMaybes
        [ Just mdc_linear_progress
        , variantCs variant
        ]
      , HP.attr (AttrName "style") "display: block;"
      , HP.attr (AttrName "role") "progressbar"
      , HP.prop (PropName "determinate")
        (case variant of
              Indeterminate -> false
              _ -> true
        )
      , progressProp variant
      , bufferProp variant
      , HP.prop (PropName "reverse") config.reverse
      , HP.prop (PropName "closed") config.closed
      ]
      <> config.additionalAttributes
    )
    [ bufferingDotsElt
    , bufferElt
    , primaryBarElt
    , secondaryBarElt
    ]

variantCs :: Variant -> Maybe ClassName
variantCs variant = case variant of
  Indeterminate -> Just mdc_linear_progress____indeterminate
  _ -> Nothing

progressProp :: forall r i . Variant -> IProp r i
progressProp variant = HP.prop (PropName "progress")
  case variant of
       Determinate progress -> progress
       Buffered progress _ -> progress
       _ -> 0.0

bufferProp :: forall r i . Variant -> IProp r i
bufferProp variant =
  HP.prop (PropName "buffer")
  case variant of
    Buffered _ buffer -> buffer
    _ -> 0.0

bufferingDotsElt :: forall w i . HH.HTML w i
bufferingDotsElt = HH.div [ HP.class_ mdc_linear_progress__buffering_dots ] []

bufferElt :: forall w i . HH.HTML w i
bufferElt = HH.div [ HP.class_ mdc_linear_progress__buffer ] []

primaryBarElt :: forall w i . HH.HTML w i
primaryBarElt =
  HH.div [ HP.classes [ mdc_linear_progress__bar, mdc_linear_progress__primary_bar ] ]
    [ barInnerElt ]

secondaryBarElt :: forall w i . HH.HTML w i
secondaryBarElt =
  HH.div [ HP.classes [ mdc_linear_progress__bar, mdc_linear_progress__secondary_bar ] ]
    [ barInnerElt ]

barInnerElt :: forall w i . HH.HTML w i
barInnerElt = HH.div [ HP.class_ mdc_linear_progress__bar_inner ] []
