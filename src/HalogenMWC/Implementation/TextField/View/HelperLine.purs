module HalogenMWC.Implementation.TextField.View.HelperLine where

import Material.Classes.Textfield (mdc_text_field_helper_line)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

helperLine :: forall t1 t2. Array (HH.HTML t2 t1) -> HH.HTML t2 t1
helperLine = HH.div [ HP.class_ mdc_text_field_helper_line ]
