module HalogenMWC.FormField where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { label :: Maybe String
    , for :: Maybe String
    , alignEnd :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    }

defaultConfig :: Config r i
defaultConfig =
  { label: Nothing
  , for: Nothing
  , alignEnd: false
  , additionalAttributes: []
  , onClick: Nothing
  }

formField :: Config r i -> Array (HH.HTML w i) -> HH.HTML w i
formField config_ nodes =
  HH.element "mdc-form-field"
    ( Array.catMaybes
        [ rootCs
        , alignEndCs config_
        ]
        <> config_.additionalAttributes
    )
    (nodes <> [ labelElt config_ ])

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_form_field)

alignEndCs :: Config r i -> Maybe (IProp r i)
alignEndCs { alignEnd } =
  if alignEnd then
    Just (HP.class_ mdc_form_field____align_end)
  else
    Nothing

forAttr :: Config r i -> Maybe (IProp r i)
forAttr { for } = map HH.Attributes.for for

clickHandler :: Config r i -> Maybe (IProp r i)
clickHandler { onClick } = map HH.Events.onClick onClick

labelElt :: Config r i -> HH.HTML w i
labelElt (config_@{ label }) =
  HH.label
    ( Array.catMaybes
        [ forAttr config_
        , clickHandler config_
        ]
    )
    [ text (Maybe.withDefault "" label) ]
