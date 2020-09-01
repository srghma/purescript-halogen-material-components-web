module HalogenMWC.List.Divider where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.List.Item (ListItem)
import HalogenMWC.List.Item as ListItem
import Material.Classes.List (mdc_list_divider, mdc_list_divider____inset, mdc_list_divider____padded)

type Config i
  = { inset :: Boolean
    , padded :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLli i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { inset: false
  , padded: false
  , additionalAttributes: []
  }

listItem :: forall w i. Config i -> ListItem w i
listItem config =
  ListItem.ListItemDivider
    $ HH.li
        ( [ HP.classes
              $ Array.catMaybes
                  [ Just mdc_list_divider
                  , if config.inset then Just mdc_list_divider____inset else Nothing
                  , if config.padded then Just mdc_list_divider____padded else Nothing
                  ]
          , HP.attr (AttrName "role") "separator"
          ]
            <> config.additionalAttributes
        )
        []

listDivider :: forall i w. HH.HTML w i
listDivider = HH.hr [ HP.class_ mdc_list_divider ]
