module HalogenMWC.Ripple where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Halogen (AttrName(..), ClassName, ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.Ripple (mdc_ripple_surface, mdc_ripple_surface____accent, mdc_ripple_surface____primary)

data Color
  = Primary
  | Accent

type Config i
  = { color :: Maybe Color
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { color: Nothing
  , additionalAttributes: []
  }

data RippleType
  = Bounded
  | Unbounded

ripple :: forall w i. RippleType -> Config i -> HH.HTML w i
ripple rippleType config =
  HH.element (ElemName "mdc-ripple")
    ( Array.catMaybes
        [ Just $ HP.classes $ [ mdc_ripple_surface ] <> colorCs config
        , Just $ HP.prop (PropName "unbounded")
          case rippleType of
                Unbounded -> true
                _ -> false
        , case rippleType of
               Unbounded -> Just (HP.attr (AttrName "data-mdc-ripple-is-unbounded") "")
               _ -> Nothing
        , Just (HP.style "position: absolute; top: 0; left: 0; bottom: 0; right: 0; ")
        ]
        <> config.additionalAttributes
    )
    []

colorCs :: forall i. Config i -> Array ClassName
colorCs config = case config.color of
  Just Primary -> [ mdc_ripple_surface____primary ]
  Just Accent -> [ mdc_ripple_surface____accent ]
  Nothing -> []
