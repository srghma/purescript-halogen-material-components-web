module HalogenMWC.Select.Item where

import Halogen.HTML (IProp)
import Halogen.HTML as HH

type Config a r i
  = { value :: a
    , disabled :: Boolean
    , additionalAttributes :: Array (IProp r i)
    }

data SelectItem w a r i
  = SelectItem (Config a r i) (Array (HH.HTML w i))

defaultConfig :: forall r i a . { value :: a } -> Config a r i
defaultConfig { value } =
  { value: value
  , disabled: false
  , additionalAttributes: []
  }
