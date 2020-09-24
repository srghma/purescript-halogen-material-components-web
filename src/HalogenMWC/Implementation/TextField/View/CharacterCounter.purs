module HalogenMWC.Implementation.TextField.View.CharacterCounter where

import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.View.Shared
import HalogenMWC.Implementation.TextField.View.FilledShared as FilledShared
import HalogenMWC.Implementation.TextField.View.OutlinedShared as OutlinedShared

type CharacterCounterConfig =
  { value :: Int
  , max :: Int
  }

characterCounter = \config -> wrap [ HH.text $ show config.value <> " / " <> show config.max ]
  where
    wrap = HH.div [ HP.class_ mdc_text_field_character_counter ]
