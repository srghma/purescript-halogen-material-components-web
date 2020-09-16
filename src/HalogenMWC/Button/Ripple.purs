module HalogenMWC.Button.Ripple where

import Material.Classes.Ripple
import Protolude

import DOM.HTML.Indexed as I
import Data.Int as Int
import Effect.Uncurried as EFn
import FRP.Event (Event) as Event
import Halogen (ClassName(..))
import Halogen (ComponentSlot, ElemName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events (onKeyUp)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Implementation as Implementation
import HalogenMWC.Button.Insides as Insides
import HalogenMWC.Utils as Utils
import Math as Math
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web.Event.Event
import Web.HTML as Web.HTML
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.Window as Web.HTML.Window
import Web.TouchEvent (TouchEvent)
import Web.TouchEvent.Touch as Web.TouchEvent.Touch
import Web.TouchEvent.TouchList as Web.TouchEvent.TouchList
import Web.TouchEvent.TouchEvent as Web.TouchEvent.TouchEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as Web.UIEvent.MouseEvent

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

pointer_deactivation_event_types :: Array EventType
pointer_deactivation_event_types =
  [ EventType "touchend"
  , EventType "pointerup"
  , EventType "mouseup"
  , EventType "contextmenu"
  ]

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
  :: { touchEvent :: TouchEvent
     , scrollX :: Int
     , scrollY :: Int
     , rootDomRect :: Web.HTML.HTMLElement.DOMRect
     }
  -> MDCRipplePoint
getNormalizedEventCoordsTouchEvent
  { touchEvent
  , scrollX
  , scrollY
  , rootDomRect
  } =
    case Web.TouchEvent.TouchList.item 0 $ Web.TouchEvent.TouchEvent.changedTouches touchEvent of
         Just touchEventItem ->
           { x: Int.toNumber (Web.TouchEvent.Touch.pageX touchEventItem) - (Int.toNumber scrollX + rootDomRect.left)
           , y: Int.toNumber (Web.TouchEvent.Touch.pageY touchEventItem) - (Int.toNumber scrollY + rootDomRect.top)
           }
         _ -> getNormalizedEventCoordsDefault rootDomRect

getNormalizedEventCoordsMouseEvent
  :: { mouseEvent :: MouseEvent
     , scrollX :: Int
     , scrollY :: Int
     , rootDomRect :: Web.HTML.HTMLElement.DOMRect
     }
  -> MDCRipplePoint
getNormalizedEventCoordsMouseEvent
  { mouseEvent
  , scrollX
  , scrollY
  , rootDomRect
  } =
  { x: Int.toNumber (Web.UIEvent.MouseEvent.pageX mouseEvent) - (Int.toNumber scrollX + rootDomRect.left)
  , y: Int.toNumber (Web.UIEvent.MouseEvent.pageY mouseEvent) - (Int.toNumber scrollY + rootDomRect.top)
  }

