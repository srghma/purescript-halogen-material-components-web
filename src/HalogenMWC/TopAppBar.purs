module HalogenMWC.TopAppBar where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { dense :: Boolean
    , fixed :: Boolean
    , additionalAttributes :: Array (IProp r i)
    }

data Variant
  = Regular
  | Short
  | ShortCollapsed
  | Prominent

defaultConfig :: Config r i
defaultConfig =
  { dense: False
  , fixed: False
  , additionalAttributes: []
  }

genericTopAppBar :: Variant -> Config r i -> Array (Html r i) -> Html r i
genericTopAppBar variant (config_@{ additionalAttributes }) nodes =
  HH.element "mdc-top-app-bar"
    ( Array.filterMap identity
        [ HP.class_ mdc_top_app_bar
        , variantCs variant
        , denseCs config_
        , fixedCs config_
        ]
        <> additionalAttributes
    )
    nodes

regular :: Config r i -> Array (Html r i) -> Html r i
regular config_ nodes = genericTopAppBar Regular config_ nodes

short :: Config r i -> Array (Html r i) -> Html r i
short config_ nodes = genericTopAppBar Short config_ nodes

shortCollapsed :: Config r i -> Array (Html r i) -> Html r i
shortCollapsed config_ nodes = genericTopAppBar ShortCollapsed config_ nodes

prominent :: Config r i -> Array (Html r i) -> Html r i
prominent config_ nodes = genericTopAppBar Prominent config_ nodes

row :: Array (IProp r i) -> Array (Html r i) -> Html r i
row attributes nodes = HH.section ([ HP.class_ mdc_top_app_bar__row ] <> attributes) nodes

section :: Array (IProp r i) -> Array (Html r i) -> Html r i
section attributes nodes = HH.section ([ HP.class_ mdc_top_app_bar__section ] <> attributes) nodes

variantCs :: Variant -> Maybe (IProp r i)
variantCs variant = case variant of
  Regular -> Nothing
  Short -> Just (HP.class_ mdc_top_app_bar____short)
  ShortCollapsed -> Just (HP.classes [mdc_top_app_bar____short mdc_top_app_bar____short_collapsed])
  Prominent -> Just (HP.class_ mdc_top_app_bar____prominent)

denseCs :: Config r i -> Maybe (IProp r i)
denseCs { dense } =
  if dense then
    Just (HP.class_ mdc_top_app_bar____dense)
  else
    Nothing

fixedCs :: Config r i -> Maybe (IProp r i)
fixedCs { fixed } =
  if fixed then
    Just (HP.class_ mdc_top_app_bar____fixed)
  else
    Nothing
