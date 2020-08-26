module HalogenMWC.TopAppBar where

import Material.Classes.TopAppBar
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

type Config i =
  { dense :: Boolean
  , fixed :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  }

data Variant
  = Regular
  | Short
  | ShortCollapsed
  | Prominent

defaultConfig :: forall i . Config i
defaultConfig =
  { dense: false
  , fixed: false
  , additionalAttributes: []
  }

genericTopAppBar :: forall i w . Variant -> Config i -> Array (HH.HTML w i) -> HH.HTML w i
genericTopAppBar variant config =
  HH.element (ElemName "mdc-top-app-bar")
    (
        [ HP.classes $ Array.concat
          [ [ mdc_top_app_bar ]
          , variantCs variant
          , if config.dense then [ mdc_top_app_bar____dense ] else []
          , if config.fixed then [ mdc_top_app_bar____fixed ] else []
          ]
        ]
        <> config.additionalAttributes
    )

------------

regular :: forall w i . Config i -> Array (HH.HTML w i) -> HH.HTML w i
regular config nodes = genericTopAppBar Regular config nodes

short :: forall w i . Config i -> Array (HH.HTML w i) -> HH.HTML w i
short config nodes = genericTopAppBar Short config nodes

shortCollapsed :: forall w i . Config i -> Array (HH.HTML w i) -> HH.HTML w i
shortCollapsed config nodes = genericTopAppBar ShortCollapsed config nodes

prominent :: forall w i . Config i -> Array (HH.HTML w i) -> HH.HTML w i
prominent config nodes = genericTopAppBar Prominent config nodes

------------

row :: forall w i . Array (IProp I.HTMLsection i) -> Array (HH.HTML w i) -> HH.HTML w i
row attributes nodes = HH.section ([ HP.class_ mdc_top_app_bar__row ] <> attributes) nodes

section :: forall w i . Array (IProp I.HTMLsection i) -> Array (HH.HTML w i) -> HH.HTML w i
section attributes nodes = HH.section ([ HP.class_ mdc_top_app_bar__section ] <> attributes) nodes

variantCs :: Variant -> Array ClassName
variantCs variant = case variant of
  Regular -> []
  Short -> [ mdc_top_app_bar____short ]
  ShortCollapsed -> [ mdc_top_app_bar____short, mdc_top_app_bar____short_collapsed ]
  Prominent -> [ mdc_top_app_bar____prominent ]

