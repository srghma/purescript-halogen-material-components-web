module HalogenMWC.Implementation.TextField.CharacterCounter where

import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.Shared
import HalogenMWC.Implementation.TextField.FilledShared as FilledShared
import HalogenMWC.Implementation.TextField.OutlinedShared as OutlinedShared

type CharacterCounterConfig =
  { value :: Int
  , max :: Int
  }

characterCounter = \config -> wrap [ HH.text $ show config.value <> " / " <> show config.max ]
  where
    wrap = HH.div [ HP.class_ mdc_text_field_character_counter ]
