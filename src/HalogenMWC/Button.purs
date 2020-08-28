module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Export
  ) where

import Halogen (ElemName(..), PropName(..))
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP
import HalogenMWC.Button.Common (Variant(..), buttonLabel, buttonIcon, defaultConfig) as Export
import HalogenMWC.Button.Common (Variant)
import HalogenMWC.Button.Common as Common
import Prelude

type Config r i
  = { disabled :: Boolean
    , variant :: Variant
    , touch :: Boolean
    , additionalClasses :: Array ClassName
    , additionalAttributes :: Array (IProp r i) -- put `HE.onClick (\_ -> Increment)` here
    }

button :: forall w i. Config I.HTMLbutton i -> Array (HH.HTML w i) -> HH.HTML w i
button config =
  let
    wrapTouch = Common.wrapTouch config.touch

    commonHtml = Common.commonHtml config.touch

    commonProps =
      [ HP.classes (Common.commonClasses config.variant config.touch <> config.additionalClasses)
      , HP.disabled config.disabled
      , HP.prop (PropName "tabIndex") (if config.disabled then -1 else 0)
      ]
  in
    \content ->
      wrapTouch
        $ HH.element (ElemName "mdc-button")
            [ HP.disabled config.disabled
            ]
            [ HH.button (commonProps <> config.additionalAttributes) (commonHtml content)
            ]

buttonLink :: forall w i. Config I.HTMLa i -> Array (HH.HTML w i) -> HH.HTML w i
buttonLink config =
  let
    wrapTouch = Common.wrapTouch config.touch

    commonHtml = Common.commonHtml config.touch

    commonProps = [ HP.classes (Common.commonClasses config.variant config.touch <> config.additionalClasses) ]
  in
    \content ->
      wrapTouch
        $ HH.element (ElemName "mdc-button")
            [ HP.disabled config.disabled
            ]
            [ HH.a (commonProps <> config.additionalAttributes) (commonHtml content)
            ]
