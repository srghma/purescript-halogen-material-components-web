module HalogenMWC.Button.Link
  ( module HalogenMWC.Button.Link
  , module Implementation
  , module Insides
  ) where

import Halogen (ElemName(..))
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Implementation as Implementation
import HalogenMWC.Button.Insides as Insides
import Prelude

type Config i =
  { additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLa i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { additionalClasses: []
  , additionalAttributes: []
  }

buttonLink :: forall w i. Variant -> Config i -> Array (HH.HTML w i) -> HH.HTML w i
buttonLink variant config =
  let
    commonProps = [ HP.classes (Implementation.commonClasses variant <> config.additionalClasses) ]
  in
    \content ->
      Implementation.wrapTouch
        [ HH.a (commonProps <> config.additionalAttributes) (Implementation.commonHtml content)
        ]
