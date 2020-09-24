module HalogenMWC.Implementation.TextField.View.HelperLine where

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

helperLine = HH.div [ HP.class_ mdc_text_field_helper_line ]
