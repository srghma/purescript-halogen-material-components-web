module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Export
  ) where

import Protolude (Maybe(..), negate, ($), (<>))

import DOM.HTML.Indexed as I
import Halogen (ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.Button.Common as Common
import HalogenMWC.Button.Common (Variant(..))
import HalogenMWC.Button.Common (Variant(..), buttonLabel, buttonIcon, defaultConfig) as Export

type Config i =
  { disabled :: Boolean
  , variant :: Variant
  , touch :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLbutton i) -- put `HE.onClick (\_ -> Increment)` here
  }

button :: forall i w . Config i -> Array (HH.HTML w i) -> HH.HTML w i
button config =
  let
    wrapTouch = Common.wrapTouch config.touch
    commonHtml = Common.commonHtml config.touch
    commonProps =
      [ HP.classes (Common.commonClasses config.variant config.touch)
      , HP.disabled config.disabled
      , HP.prop (PropName "tabIndex") (if config.disabled then -1 else 0)
      ]
  in \content ->
      wrapTouch
        $ HH.element (ElemName "mdc-button")
            [ HP.prop (PropName "disabled") config.disabled
            ]
            [ HH.button (commonProps <> config.additionalAttributes) (commonHtml content)
            ]
