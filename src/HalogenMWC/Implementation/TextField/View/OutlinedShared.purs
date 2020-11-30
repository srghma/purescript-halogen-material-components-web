module HalogenMWC.Implementation.TextField.View.OutlinedShared where

import HalogenMWC.Implementation.TextField.View.Shared (LabelConfig(..), floatingLabelSpanElement)
import Material.Classes.Textfield (mdc_notched_outline, mdc_notched_outline____no_label, mdc_notched_outline____notched, mdc_notched_outline__leading, mdc_notched_outline__notch, mdc_notched_outline__trailing)
import Protolude

import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

notchedOutlineLeadingElement :: forall t12 t13. Maybe (HH.HTML t13 t12)
notchedOutlineLeadingElement = Just $ HH.div [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElement :: forall t4 t5. Maybe (HH.HTML t5 t4)
notchedOutlineTrailingElement = Just $ HH.div [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineElement :: ∀ w i r. { noLabel :: Boolean, floatAbove ∷ Boolean , label ∷ LabelConfig , required ∷ Boolean , shake ∷ Boolean | r } → HH.HTML w i
notchedOutlineElement config =
  HH.div
  [ HP.classes $ Array.catMaybes
    [ Just mdc_notched_outline
    -- | XXX:
    -- | makes label display incorrectly when floated,
    -- | requires `width` style to be calculated
    -- | , Just mdc_notched_outline____upgraded
    , if config.floatAbove then Just mdc_notched_outline____notched else Nothing
    , if config.noLabel then Just mdc_notched_outline____no_label else Nothing
    ]
  ]
  ( Array.catMaybes
    [ notchedOutlineLeadingElement
    , case config.label of
           LabelConfig__With labelConfig ->
             Just $
                HH.div
                [ HP.class_ mdc_notched_outline__notch
                ]
                [ floatingLabelSpanElement
                  { labelConfig: labelConfig
                  , floatAbove: config.floatAbove
                  , required: config.required
                  , shake: config.shake
                  }
                ]
           LabelConfig__Without _ -> Nothing
    , notchedOutlineTrailingElement
    ]
  )
