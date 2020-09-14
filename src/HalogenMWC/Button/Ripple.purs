module HalogenMWC.Button.Ripple where

import Material.Classes.Ripple
import Protolude

import Data.Int as Int
import Effect.Uncurried as EFn
import Halogen (ClassName(..))
import Math as Math
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as Web.HTML.HTMLElement

cssClasses ::
  { "BG_FOCUSED"      :: ClassName
  , "FG_ACTIVATION"   :: ClassName
  , "FG_DEACTIVATION" :: ClassName
  , "ROOT"            :: ClassName
  , "UNBOUNDED"       :: ClassName
  }
cssClasses =
  { "BG_FOCUSED":      mdc_ripple_upgraded____background_focused
  , "FG_ACTIVATION":   mdc_ripple_upgraded____foreground_activation
  , "FG_DEACTIVATION": mdc_ripple_upgraded____foreground_deactivation
  , "ROOT":            mdc_ripple_upgraded
  , "UNBOUNDED":       mdc_ripple_upgraded____unbounded
  }

foreign import strings ::
  { "VAR_FG_SCALE"           :: String
  , "VAR_FG_SIZE"            :: String
  , "VAR_FG_TRANSLATE_END"   :: String
  , "VAR_FG_TRANSLATE_START" :: String
  , "VAR_LEFT"               :: String
  , "VAR_TOP"                :: String
  }

foreign import numbers ::
  { "DEACTIVATION_TIMEOUT_MS" :: Int -- Corresponds to $mdc-ripple-translate-duration (i.e. activation animation duration)
  , "FG_DEACTIVATION_MS"      :: Int -- Corresponds to $mdc-ripple-fade-out-duration (i.e. deactivation animation duration)
  , "INITIAL_ORIGIN_SCALE"    :: Number
  , "PADDING"                 :: Number
  , "TAP_DELAY_MS"            :: Int -- Delay between touch and simulated mouse events on touch devices
  }

layoutInternal
  :: Web.HTML.HTMLElement.DOMRect -- this.adapter.computeBoundingRect()
  -> Boolean
  -> { maxRadius :: Number
     , initialSize :: Int
     , fgScale :: Number
     }
layoutInternal = \frame isUnbounded ->
  let
    maxDim = Math.max frame.height frame.width

    -- Ripple is sized as a fraction of the largest dimension of the surface, then scales up using a CSS scale transform
    initialSize =
      let
        x = Int.floor (maxDim * numbers."INITIAL_ORIGIN_SCALE")
      in
        -- Unbounded ripple size should always be even number to equally center align.
        if isUnbounded && not (Int.even x)
          then x - 1
          else x

    maxRadius =
      if isUnbounded
        then maxDim
        else getBoundedRadius frame

    fgScale = maxRadius / Int.toNumber initialSize
  in
  { maxRadius
  , initialSize
  , fgScale
  }
  where
    getBoundedRadius frame =
      let hypotenuse = Math.sqrt (Math.pow frame.width 2.0 + Math.pow frame.height 2.0)
       in hypotenuse + numbers."PADDING"

updateCssVarsCommon
  :: { initialSize :: Int
     , fgScale :: Number
     }
  -> { "VAR_FG_SCALE" :: String
     , "VAR_FG_SIZE"  :: String
     }
updateCssVarsCommon { initialSize, fgScale } =
  { "VAR_FG_SCALE": show fgScale
  , "VAR_FG_SIZE": show initialSize <> "px"
  }

updateCssVarsUnbounded
  :: { initialSize :: Int
     , frame :: Web.HTML.HTMLElement.DOMRect
     }
  -> { "VAR_LEFT" :: String
     , "VAR_TOP"  :: String
     }
updateCssVarsUnbounded { initialSize, frame }=
  let
    left = Int.round ((frame.width / 2.0) - Int.toNumber (initialSize / 2))
    top = Int.round ((frame.height / 2.0) - Int.toNumber (initialSize / 2))
  in
    { "VAR_LEFT": show left <> "px"
    , "VAR_TOP":  show top <> "px"
    }

-- | layout(): void {
-- |   if (this.layoutFrame_) {
-- |     cancelAnimationFrame(this.layoutFrame_);
-- |   }
-- |   this.layoutFrame_ = requestAnimationFrame(() => {
-- |     this.layoutInternal_();
-- |     this.layoutFrame_ = 0;
-- |   });
-- | }
