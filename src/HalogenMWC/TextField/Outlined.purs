module HalogenMWC.TextField.Outlined
  ( module HalogenMWC.TextField.Outlined
  , module Export
  ) where

import Protolude

import Halogen as H
import HalogenMWC.Implementation.TextField.View.Input as TextField.Input
import HalogenMWC.Implementation.TextField.View.Shared (LabelConfig(..)) as Export
import Record as Record
import HalogenMWC.Implementation.TextField.Component.Shared (Message, Query, additionalAttributes, defaultConfigShared, eval, initialState)
import HalogenMWC.Implementation.TextField.Component.Shared (Action(..), Message(..), Query) as Export
import HalogenMWC.Implementation.TextField.View.Input (ConfigManagedByComponent, ConfigManagedByUser)

type Input = Record (ConfigManagedByUser + ())

type State = Record (ConfigManagedByUser + ConfigManagedByComponent + ())

defaultConfig :: Input
defaultConfig = defaultConfigShared

outlined :: H.Component Query Input Message Aff
outlined =
  H.mkComponent
    { initialState
    , render: \state -> TextField.Input.outlined $ Record.union state additionalAttributes
    , eval
    }
