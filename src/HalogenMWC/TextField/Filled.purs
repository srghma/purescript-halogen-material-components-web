module HalogenMWC.TextField.Filled
  ( module HalogenMWC.TextField.Filled
  , module Export
  ) where

import HalogenMWC.Implementation.TextField.Component.Shared (Message, Query, additionalAttributes, defaultConfigShared, eval, initialState)
import Protolude

import Halogen as H
import HalogenMWC.Implementation.TextField.Component.Shared (Action(..), Message(..), Query) as Export
import HalogenMWC.Implementation.TextField.View.Input (ConfigFilled) as Export
import HalogenMWC.Implementation.TextField.View.Input as TextField.Input
import HalogenMWC.Implementation.TextField.View.Shared (LabelConfig(..)) as Export
import Record as Record
import HalogenMWC.Implementation.TextField.View.Input (ConfigManagedByComponent, ConfigManagedByUser)

type Input = Record (ConfigManagedByUser + ( fullwidth :: Boolean ))

type State = Record (ConfigManagedByUser + ConfigManagedByComponent + ( fullwidth :: Boolean ))

defaultConfig :: Input
defaultConfig = Record.union defaultConfigShared { fullwidth: false }

filled :: H.Component Query Input Message Aff
filled =
  H.mkComponent
    { initialState
    , render: \state -> TextField.Input.filled $ Record.union state additionalAttributes
    , eval
    }
