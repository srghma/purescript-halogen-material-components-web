module HalogenMWC.Select.Item where

import Halogen.HTML (IProp)
import DOM.HTML.Indexed as I
import Halogen.HTML as HH

type Config a i
  = { value :: a
    , disabled :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLa i)
    }

data SelectItem w a i
  = SelectItem (Config a i) (Array (HH.HTML w i))

defaultConfig :: forall i a. a -> Config a i
defaultConfig value =
  { value: value
  , disabled: false
  , additionalAttributes: []
  }
