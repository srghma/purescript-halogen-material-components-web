module HalogenMWC.List.Item where

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
import Material.Classes.List

type Config w i =
  { disabled             :: Boolean
  , selection            :: Maybe Selection
  , href                 :: Maybe String
  , target               :: Maybe String
  , additionalAttributes :: Array (IProp I.HTMLa i)
  , onClick              :: Maybe (Event -> i)
  , node                 :: HH.HTML w i
  }

data Selection
  = Selected
  | Activated

derive instance eqSelection :: Eq Selection

data ListItem w i
  = ListItem (Config w i)
  | ListItemDivider (HH.HTML w i)
  | ArrayGroupSubheader (HH.HTML w i)

defaultConfig :: forall w i . Config w i
defaultConfig =
  { disabled:             false
  , selection:            Nothing
  , href:                 Nothing
  , target:               Nothing
  , additionalAttributes: []
  , onClick:              Nothing
  , node:                 HH.text ""
  }

listItem :: forall w i . Config w i -> Array (HH.HTML w i) -> ListItem w i
listItem config nodes = ListItem (config { node = listItemView config nodes })

listItemView :: forall w i . Config w i -> Array (HH.HTML w i) -> HH.HTML w i
listItemView config nodes =
  let
    attributes =
      Array.catMaybes
        [ Just $ HP.classes $ Array.catMaybes
          [ Just mdc_list_item
          , if config.disabled then Just mdc_list_item____disabled else Nothing
          , if config.selection == Just Selected then Just mdc_list_item____selected else Nothing
          , if config.selection == Just Activated then Just mdc_list_item____activated else Nothing
          ]
        , map HP.href config.href
        , if config.href /= Nothing then map HP.target config.target else Nothing
        , if config.selection /= Nothing then Just (HP.attr (AttrName "aria-selected") "true") else Nothing
        ]
        <> config.additionalAttributes
  in
      if config.href /= Nothing then
        HH.element (ElemName "mdc-list-item") [] [ HH.a attributes nodes ]
      else
        HH.element (ElemName "mdc-list-item") attributes nodes

text ::
  forall w i .
  Array (IProp I.HTMLdiv i) ->
  { primary :: Array (HH.HTML w i)
  , secondary :: Array (HH.HTML w i)
  } ->
  HH.HTML w i
text additionalAttributes { primary, secondary } =
  HH.div ([ HP.class_ mdc_list_item__text ] <> additionalAttributes)
    [ primaryText primary
    , secondaryText secondary
    ]

primaryText :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
primaryText = HH.div [ HP.class_ mdc_list_item__primary_text ]

secondaryText :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
secondaryText = HH.div [ HP.class_ mdc_list_item__secondary_text ]

graphic :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
graphic = HH.div [ HP.class_ mdc_list_item__graphic ]

meta :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
meta = HH.div [ HP.class_ mdc_list_item__meta ]
