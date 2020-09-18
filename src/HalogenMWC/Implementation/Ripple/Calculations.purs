module HalogenMWC.Ripple.Calculations where

import Protolude

import Data.Int as Int
import Math as Math
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.TouchEvent (TouchEvent)
import Web.TouchEvent.Touch as Web.TouchEvent.Touch
import Web.TouchEvent.TouchList as Web.TouchEvent.TouchList
import Web.TouchEvent.TouchEvent as Web.TouchEvent.TouchEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as Web.UIEvent.MouseEvent
import HalogenMWC.Ripple.Constants (numbers)

---- GET LAYOUT ------------------------------------------------------

layoutInternal
  :: { rootDomRect :: Web.HTML.HTMLElement.DOMRect, isUnbounded :: Boolean }
  -> { maxRadius :: Number
     , initialSize :: Int
     , fgScale :: Number
     }
layoutInternal = \{ rootDomRect, isUnbounded } ->
  let
    maxDim = Math.max rootDomRect.height rootDomRect.width

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
        else getBoundedRadius rootDomRect

    fgScale = maxRadius / Int.toNumber initialSize
  in
  { maxRadius
  , initialSize
  , fgScale
  }
  where
    getBoundedRadius rootDomRect =
      let hypotenuse = Math.sqrt (Math.pow rootDomRect.width 2.0 + Math.pow rootDomRect.height 2.0)
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
     , rootDomRect :: Web.HTML.HTMLElement.DOMRect
     }
  -> { "VAR_LEFT" :: String
     , "VAR_TOP"  :: String
     }
updateCssVarsUnbounded { initialSize, rootDomRect }=
  let
    left = Int.round ((rootDomRect.width / 2.0) - Int.toNumber (initialSize / 2))
    top = Int.round ((rootDomRect.height / 2.0) - Int.toNumber (initialSize / 2))
  in
    { "VAR_LEFT": show left <> "px"
    , "VAR_TOP":  show top <> "px"
    }

---- getFgTranslationCoordinates_ ------------------------------------------------------

type MDCRipplePoint
  = { x :: Number
    , y :: Number
    }

type FgTranslationCoordinates
  = { startPoint :: MDCRipplePoint
    , endPoint :: MDCRipplePoint
    }

fgTranslationCoordinatesToTranslateForUnbounded ::
  FgTranslationCoordinates ->
  { "VAR_FG_TRANSLATE_START" :: String
  , "VAR_FG_TRANSLATE_END" :: String
  }
fgTranslationCoordinatesToTranslateForUnbounded = \coords ->
  { "VAR_FG_TRANSLATE_START": print coords.startPoint
  , "VAR_FG_TRANSLATE_END": print coords.endPoint
  }
  where
    print { x, y } = show x <> "px, " <> show y <> "px"

getFgTranslationCoordinatesPonter
  :: { normalizedEventCoords :: MDCRipplePoint
     , initialSize :: Int
     , rootDomRect :: Web.HTML.HTMLElement.DOMRect
     }
  -> FgTranslationCoordinates
getFgTranslationCoordinatesPonter
  { normalizedEventCoords
  , initialSize
  , rootDomRect
  } =
  let
    -- Center the element around the start point.
    startPoint =
      { x: normalizedEventCoords.x - Int.toNumber (initialSize / 2)
      , y: normalizedEventCoords.y - Int.toNumber (initialSize / 2)
      }

    endPoint =
      { x: (rootDomRect.width / 2.0) - Int.toNumber (initialSize / 2)
      , y: (rootDomRect.height / 2.0) - Int.toNumber (initialSize / 2)
      }
   in { startPoint, endPoint }

-------------------

getNormalizedEventCoordsDefault :: Web.HTML.HTMLElement.DOMRect -> MDCRipplePoint
getNormalizedEventCoordsDefault rootDomRect = { x: rootDomRect.width / 2.0, y: rootDomRect.height / 2.0 }

getNormalizedEventCoordsTouchEvent
  :: { event :: TouchEvent
     , scrollX :: Int
     , scrollY :: Int
     , rootDomRect :: Web.HTML.HTMLElement.DOMRect
     }
  -> MDCRipplePoint
getNormalizedEventCoordsTouchEvent
  { event
  , scrollX
  , scrollY
  , rootDomRect
  } =
    case Web.TouchEvent.TouchList.item 0 $ Web.TouchEvent.TouchEvent.changedTouches event of
         Just touchEventItem ->
           { x: Int.toNumber (Web.TouchEvent.Touch.pageX touchEventItem) - (Int.toNumber scrollX + rootDomRect.left)
           , y: Int.toNumber (Web.TouchEvent.Touch.pageY touchEventItem) - (Int.toNumber scrollY + rootDomRect.top)
           }
         _ -> getNormalizedEventCoordsDefault rootDomRect

getNormalizedEventCoordsMouseEvent
  :: { event :: MouseEvent
     , scrollX :: Int
     , scrollY :: Int
     , rootDomRect :: Web.HTML.HTMLElement.DOMRect
     }
  -> MDCRipplePoint
getNormalizedEventCoordsMouseEvent
  { event
  , scrollX
  , scrollY
  , rootDomRect
  } =
  { x: Int.toNumber (Web.UIEvent.MouseEvent.pageX event) - (Int.toNumber scrollX + rootDomRect.left)
  , y: Int.toNumber (Web.UIEvent.MouseEvent.pageY event) - (Int.toNumber scrollY + rootDomRect.top)
  }

