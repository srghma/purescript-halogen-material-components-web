module HalogenMWC.HelperText where

import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import DOM.HTML.Indexed as I
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.Textfield

type Config i =
  { persistent :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { persistent: false
  , additionalAttributes: []
  }

helperText :: forall w i . Config i -> String -> HH.HTML w i
helperText config string =
  HH.div
    ( [ HP.classes $ [ mdc_text_field_helper_text ] <> (if config.persistent then [ mdc_text_field_helper_text____persistent ] else [])
      , HP.attr (AttrName "aria-hidden") "true"
      ]
      <> config.additionalAttributes
    )
    [ HH.text string ]

helperLine :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
helperLine = HH.div ([ HP.class_ mdc_text_field_helper_line ])

characterCounter :: forall w i . HH.HTML w i
characterCounter = HH.div ([ HP.class_ mdc_text_field_character_counter ]) []
