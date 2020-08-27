module HalogenMWC.List.Divider where

import Protolude
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes
import Web.Event.Event

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.List.Item (ListItem)
import HalogenMWC.List.Item as ListItem
import Material.Classes.List

type Config i =
  { inset :: Boolean
  , padded :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLli i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { inset: false
  , padded: false
  , additionalAttributes: []
  }

listItem :: forall w i . Config i -> ListItem w i
listItem config =
  ListItem.ListItemDivider $ HH.li
    ( [ HP.classes $ Array.catMaybes
        [ Just mdc_list_divider
        , if config.inset then Just mdc_list_divider____inset else Nothing
        , if config.padded then Just mdc_list_divider____padded else Nothing
        ]
      , HP.attr (AttrName "role") "separator"
      ]
      <> config.additionalAttributes
    )
    []

group :: forall i w . HH.HTML w i
group = HH.hr [ HP.class_ mdc_list_divider ]
