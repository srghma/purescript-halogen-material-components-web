module HalogenMWC.Implementation.TextField.View.OutlinedShared where

import HalogenMWC.Implementation.TextField.View.Shared
import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Utils (styleVar)

notchedOutlineLeadingElement = Just $ HH.div [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElement = Just $ HH.div [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineElement :: ∀ t52 t53 t83. { noLabel :: Boolean, floatAbove ∷ Boolean , label ∷ LabelConfig , required ∷ Boolean , shake ∷ Boolean | t83 } → HH.HTML t53 t52
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
