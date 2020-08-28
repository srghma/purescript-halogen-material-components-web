module HalogenMWC.Chip.Choice where

import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)

type Config i
  = { icon :: Maybe String
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    }

data Chip a i
  = Chip (Config i) a

defaultConfig :: forall i. Config i
defaultConfig =
  { icon: Nothing
  , additionalAttributes: []
  }
