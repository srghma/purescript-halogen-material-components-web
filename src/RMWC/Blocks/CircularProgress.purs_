module RMWC.Blocks.CircularProgress where

import Protolude
import RMWC.Classes.CircularProgress

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Halogen.HTML.Core (AttrName(..))
import Halogen.SVG.Attributes as SVG.Attributes
import Halogen.SVG.Elements as SVG
import Halogen.XLINK.Attributes as XLINK
import Halogen.XLINK.Core as SVXLINK
import Math as Math
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML (ClassName(..))
import Data.Percent (Percent)
import Data.Percent as Percent

data Size
  = Xsmall
  | Small
  | Medium
  | Large
  | Xlarge
  | Custom Number

sizeToNumber :: Size -> Number
sizeToNumber Xsmall          = 18.0
sizeToNumber Small           = 20.0
sizeToNumber Medium          = 24.0
sizeToNumber Large           = 36.0
sizeToNumber Xlarge          = 48.0
sizeToNumber (Custom number) = number

sizeToClassName :: Size -> Array ClassName
sizeToClassName Xsmall     = [ rmwc_circular_progress____size_xsmall ]
sizeToClassName Small      = [ rmwc_circular_progress____size_small ]
sizeToClassName Medium     = [ rmwc_circular_progress____size_medium ]
sizeToClassName Large      = [ rmwc_circular_progress____size_large ]
sizeToClassName Xlarge     = [ rmwc_circular_progress____size_xlarge ]
sizeToClassName (Custom _) = []

circleProps size =
  [ SVG.Attributes.class_ rmwc_circular_progress__path
  , SVG.Attributes.cx (size / 2.0)
  , SVG.Attributes.cy (size / 2.0)
  , SVG.Attributes.r (size / 2.4)
  ]

circularProgressIndeterminate
  :: ∀ w i
   . Size
  -> HH.HTML w i
circularProgressIndeterminate size =
  let
    size' = sizeToNumber size
  in
    HH.div
      [ HP.classes $ [ rmwc_circular_progress, rmwc_circular_progress____indeterminate ] <> sizeToClassName size
      ]
      [ SVG.svg
          [ SVG.Attributes.class_ rmwc_circular_progress__circle
          , SVG.Attributes.viewBox 0.0 0.0 size' size'
          ]
          [ SVG.circle (circleProps size') []
          ]
      ]

circularProgressDeterminate
  :: ∀ w i
   .  { progress :: Percent
      , size     :: Size
      }
  -> HH.HTML w i
circularProgressDeterminate { size, progress } =
  let
    size' = sizeToNumber size

    styleAttribute =
      let
        n = 2.0 * Math.pi * (size' / 2.4) * Percent.toNumber progress
       in [ HP.attr (AttrName "style") ("stroke-dasharray: " <> show n <> " 666.66%;") ]
  in
    HH.div
      [ Halogen.HTML.Properties.ARIA.valueMin "0"
      , Halogen.HTML.Properties.ARIA.valueMax "1"
      , Halogen.HTML.Properties.ARIA.valueNow (show progress)
      , HP.classes $ [ rmwc_circular_progress ] <> sizeToClassName size
      ]
      [ SVG.svg
          [ SVG.Attributes.class_ rmwc_circular_progress__circle
          , SVG.Attributes.viewBox 0.0 0.0 size' size'
          ]
          [ SVG.circle (circleProps size' <> styleAttribute) []
          ]
      ]
