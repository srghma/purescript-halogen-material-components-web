module HalogenMWC.Implementation.TextField.View.CharacterCounter where

import Material.Classes.Textfield (mdc_text_field_character_counter)
import Protolude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type CharacterCounterConfig =
  { value :: Int
  , max :: Int
  }

characterCounter :: forall t2 t3. { max :: Int, value :: Int } -> HH.HTML t3 t2
characterCounter = \config -> wrap [ HH.text $ show config.value <> " / " <> show config.max ]
  where
    wrap = HH.div [ HP.class_ mdc_text_field_character_counter ]
