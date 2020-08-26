module HalogenMWC.FormField where

import Material.Classes.FormField (mdc_form_field, mdc_form_field____align_end)
import Protolude (Maybe(..), ($), (<>))
import DOM.HTML.Indexed (HTMLdiv) as I
import Data.Array as Array
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

type Config i
  = { label :: String
    , for :: Maybe String
    , alignEnd :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onClick :: Maybe (MouseEvent -> i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { label: ""
  , for: Nothing
  , alignEnd: false
  , additionalAttributes: []
  , onClick: Nothing
  }

formField :: forall w i. Config i -> Array (HH.HTML w i) -> HH.HTML w i
formField config nodes =
  HH.element (ElemName "mdc-form-field")
    ( [ HP.classes $ [ mdc_form_field ] <> (if config.alignEnd then [ mdc_form_field____align_end ] else [])
      ]
        <> config.additionalAttributes
    )
    (nodes <> [ labelElt config ])

labelElt :: forall w i. Config i -> HH.HTML w i
labelElt config =
  HH.label
    ( Array.concat
        [ case config.for of
            Nothing -> []
            Just for -> [ HP.for for ]
        , case config.onClick of
            Nothing -> []
            Just onClick -> [ HE.onClick onClick ]
        ]
    )
    [ HH.text config.label ]
