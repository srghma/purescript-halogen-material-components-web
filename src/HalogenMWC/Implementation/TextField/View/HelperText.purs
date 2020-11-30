module HalogenMWC.Implementation.TextField.View.HelperText where

import Protolude

import Data.Array as Array
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import Material.Classes.Textfield (mdc_text_field_helper_text, mdc_text_field_helper_text____persistent, mdc_text_field_helper_text____validation_msg)

type HelperTextConfig =
  { id :: String
  , text :: String
  , persistent :: Boolean
  , validation :: Boolean
  }

maybeInputProps :: forall t12 t5 t6.
  Maybe
    { id :: String
    | t12
    }
  -> Array (IProp t6 t5)
maybeInputProps =
  case _ of
    Just { id } -> inputProps id
    _ -> []
  where
    inputProps id =
      [ HP.ARIA.controls id
      , HP.ARIA.describedBy id
      ]

helperText :: forall w i . HelperTextConfig -> HH.HTML w i
helperText config =
  HH.div
  ( [ HP.classes $ Array.catMaybes
      [ Just mdc_text_field_helper_text
      , if config.validation then Just mdc_text_field_helper_text____persistent else Nothing
      , if config.validation then Just mdc_text_field_helper_text____validation_msg else Nothing
      ]
    , HP.id_ config.id
    , HP.ARIA.hidden $
      if config.validation
        then "false" -- should be visible always
        else "true" -- dont show on screen reader
    ]
    <> if config.validation then [ HP.ARIA.role "alert" ] else []
  )
  [HH.text config.text]
