module HalogenMWC.HelperText where

import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.Textfield

type Config r i
  = { persistent :: Boolean
    , additionalAttributes :: Array (IProp r i)
    }

defaultConfig :: Config r i
defaultConfig =
  { persistent: false
  , additionalAttributes: []
  }

helperText :: Config r i -> String -> HH.HTML w i
helperText config_ string =
  HH.div
    ( Array.catMaybes
        [ helperTextCs
        , persistentCs config_
        , ariaHiddenAttr
        ]
        <> config_.additionalAttributes
    )
    [ HH.text string ]

helperLine :: Array (IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
helperLine additionalAttributes nodes = HH.div ([ helperLineCs ] <> additionalAttributes) nodes

helperTextCs :: Maybe (IProp r i)
helperTextCs = Just (HP.class_ mdc_text_field_helper_text)

helperLineCs :: IProp r i
helperLineCs = HP.class_ mdc_text_field_helper_line

persistentCs :: Config r i -> Maybe (IProp r i)
persistentCs (Config config_) =
  if config_.persistent then
    Just (HP.class_ mdc_text_field_helper_text____persistent)
  else
    Nothing

ariaHiddenAttr :: Maybe (IProp r i)
ariaHiddenAttr = Just (HP.attr "aria-hidden" "true")

characterCounter :: Array (IProp r i) -> HH.HTML w i
characterCounter additionalAttributes = HH.div ([ characterCounterCs ] <> additionalAttributes) []

characterCounterCs :: IProp r i
characterCounterCs = HP.class_ mdc_text_field_character_counter
