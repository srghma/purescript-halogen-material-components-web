module HalogenMWC.TopAppBar where

import Prelude
import DOM.HTML.Indexed as I
import Data.Array as Array
import Halogen (ClassName, ElemName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.TopAppBar (mdc_top_app_bar, mdc_top_app_bar____dense, mdc_top_app_bar____fixed, mdc_top_app_bar____prominent, mdc_top_app_bar____short, mdc_top_app_bar____short_collapsed)

type Config i =
  { dense :: Boolean
  , fixed :: Boolean
  , additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  }

data Variant
  = Regular
  | Short
  | ShortCollapsed
  | Prominent

defaultConfig :: forall i. Config i
defaultConfig =
  { dense: false
  , fixed: false
  , additionalClasses: []
  , additionalAttributes: []
  }

topAppBar :: forall i w. Variant -> Config i -> Array (HH.HTML w i) -> HH.HTML w i
topAppBar variant config =
  HH.element (ElemName "mdc-top-app-bar")
    ( [ HP.classes $ Array.concat
        [ [ mdc_top_app_bar ]
        , variantCs variant
        , if config.dense then [ mdc_top_app_bar____dense ] else []
        , if config.fixed then [ mdc_top_app_bar____fixed ] else []
        , config.additionalClasses
        ]
      ]
      <> config.additionalAttributes
    )

variantCs :: Variant -> Array ClassName
variantCs variant = case variant of
  Regular -> []
  Short -> [ mdc_top_app_bar____short ]
  ShortCollapsed -> [ mdc_top_app_bar____short, mdc_top_app_bar____short_collapsed ]
  Prominent -> [ mdc_top_app_bar____prominent ]
