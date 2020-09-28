module HalogenMWC.Implementation.Ripple.Common where

import Protolude

import Data.Lens.Record (prop) as Lens
import Halogen as H
import HalogenMWC.Implementation.Ripple.Calculations (FgTranslationCoordinates, MDCRipplePoint, fgTranslationCoordinatesToTranslateForUnbounded, getFgTranslationCoordinatesPonter, getNormalizedEventCoordsMouseEvent, getNormalizedEventCoordsTouchEvent, layoutInternal, updateCssVarsCommon, updateCssVarsUnbounded)
import HalogenMWC.Implementation.Ripple.Constants (numbers, pointer_deactivation_event_types)
import HalogenMWC.Utils (setEfficiently)
import HalogenMWC.Utils as Utils
import Web.DOM (Element)
import Web.DOM.Document as Web.DOM.Document
import Web.DOM.Element as Web.DOM.Element
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.Window (Window)
import Web.HTML.Window as Web.HTML.Window
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type StyleCommonVars =
  { "VAR_FG_SCALE" :: String
  , "VAR_FG_SIZE"  :: String
  }

-- TODO: split for each like `type StyleVarsBounded/StyleVarsUnbounded = Maybe ...`
data StyleVars
  = StyleVars__Empty
  | StyleVars__Bounded
    { "VAR_FG_TRANSLATE_START" :: String
    , "VAR_FG_TRANSLATE_END" :: String
    }
  | StyleVars__Unbounded
    { "VAR_LEFT" :: String
    , "VAR_TOP"  :: String
    }

data ActivationState
  = ActivationState__Idle
  | ActivationState__Activated
  | ActivationState__Deactivated

derive instance eqActivationState :: Eq ActivationState

data RippleAction__Common
  = Focus
  | Blur
  | TouchActivate TouchEvent
  | MouseActivate MouseEvent
  | Deactivate H.SubscriptionId
  | DeactivationEnded

type RippleState =
  { focused                 :: Boolean
  , styleCommonVars         :: StyleCommonVars
  , styleVars               :: StyleVars
  , activationState         :: ActivationState
  }

initialRippleState :: RippleState
initialRippleState =
  { styleCommonVars:
      { "VAR_FG_SCALE": "1" -- from css, the 0 will result in error (element won't be able to focus)
      , "VAR_FG_SIZE": "0"
      }
  , styleVars: StyleVars__Empty
  , focused: false
  , activationState: ActivationState__Idle
  }


setStateActivationStateEfficiently :: ActivationState -> RippleState -> RippleState
setStateActivationStateEfficiently = setEfficiently (Lens.prop (SProxy :: SProxy "activationState"))

setStateFocusedEfficiently :: Boolean -> RippleState -> RippleState
setStateFocusedEfficiently = setEfficiently (Lens.prop (SProxy :: SProxy "focused"))

type GetNormalizedEventCoordsFn event =
  { event :: event
  , scrollX :: Int
  , scrollY :: Int
  , rootDomRect :: Web.HTML.HTMLElement.DOMRect
  }
  -> MDCRipplePoint

----------------------------------------------------------

layoutAndModifyState
  :: forall output slots action
   . Boolean
  -> Web.HTML.HTMLElement
  -> H.HalogenM RippleState action slots output Aff Unit
layoutAndModifyState
  isUnbounded
  rootElement
  = do
    (rootDomRect :: Web.HTML.HTMLElement.DOMRect) <- H.liftEffect $ Web.HTML.HTMLElement.getBoundingClientRect rootElement

    let { maxRadius, initialSize, fgScale } = layoutInternal { rootDomRect, isUnbounded }

    H.modify_ \state -> state
      { styleCommonVars = updateCssVarsCommon { initialSize, fgScale }
      , styleVars =
          if isUnbounded
            then StyleVars__Unbounded $ updateCssVarsUnbounded { initialSize, rootDomRect }
            else StyleVars__Empty
      }


----------------------------------------------------------

activationLogicGo
  :: forall event slots output
   . Boolean
  -> Web.HTML.HTMLElement
  -> GetNormalizedEventCoordsFn event
  -> event
  -> H.HalogenM RippleState RippleAction__Common slots output Aff Unit
activationLogicGo
  isUnbounded
  rootElement
  getNormalizedEventCoords
  event
  = do
    ( { rootDomRect
      , scrollX
      , scrollY
      , documentElement
      }
    ) <- H.liftEffect do
        (rootDomRect :: Web.HTML.HTMLElement.DOMRect) <- Web.HTML.HTMLElement.getBoundingClientRect rootElement
        (window :: Window) <- Web.HTML.window
        (scrollX :: Int) <- Web.HTML.Window.scrollX window
        (scrollY :: Int) <- Web.HTML.Window.scrollY window

        (documentElement :: Element) <- (Web.DOM.Document.documentElement =<< Web.HTML.HTMLDocument.toDocument <$> Web.HTML.Window.document window)
            >>= maybe (throwError $ error "no document element (html)") pure

        pure
          { rootDomRect
          , scrollX
          , scrollY
          , documentElement
          }

    let { maxRadius, initialSize, fgScale } = layoutInternal { rootDomRect, isUnbounded }

    let (styleCommonVars :: StyleCommonVars) = updateCssVarsCommon { initialSize, fgScale }
    let (styleVars :: StyleVars) =
          if isUnbounded
            then StyleVars__Unbounded $ updateCssVarsUnbounded { initialSize, rootDomRect }
            else let
              (normalizedEventCoords :: MDCRipplePoint) = getNormalizedEventCoords
                { event
                , scrollX
                , scrollY
                , rootDomRect
                }

              (fgTranslationCoordinates :: FgTranslationCoordinates) = getFgTranslationCoordinatesPonter
                { normalizedEventCoords
                , initialSize
                , rootDomRect
                }

              in StyleVars__Bounded $ fgTranslationCoordinatesToTranslateForUnbounded fgTranslationCoordinates

    H.modify_ \state' -> state'
      { activationState = ActivationState__Activated
      -- for faster render, will render once instead of 2 times, because Focus action comes after mouseEvent
      , focused = true
      , styleCommonVars = styleCommonVars
      , styleVars = styleVars
      }

    void $ H.subscribe' \pointerReleasedSubscriptionId ->
      Utils.eventListenerEventSourceWithOptionsMany
          pointer_deactivation_event_types
          Utils.unsafePassiveIfSupportsAddEventListenerOptions
          (Web.DOM.Element.toEventTarget documentElement)
      <#> const (Deactivate pointerReleasedSubscriptionId)

activationLogic
  :: forall event slots output
   . Boolean
  -> Boolean
  -> Web.HTML.HTMLElement
  -> GetNormalizedEventCoordsFn event
  -> event
  -> H.HalogenM RippleState RippleAction__Common slots output Aff Unit
activationLogic
  isDisabled
  isUnbounded
  rootElement
  getNormalizedEventCoords
  event
  =
  if isDisabled then pure unit else do
     state <- H.get
     case state.activationState of
         ActivationState__Idle -> activationLogicGo isUnbounded rootElement getNormalizedEventCoords event
         ActivationState__Deactivated -> activationLogicGo isUnbounded rootElement getNormalizedEventCoords event
         _ -> pure unit

----------------------------------------------------------

handleAction__Common
  :: forall slots output
   . Boolean
  -> Boolean
  -> Web.HTML.HTMLElement
  -> RippleAction__Common
  -> H.HalogenM RippleState RippleAction__Common slots output Aff Unit
handleAction__Common
  isDisabled
  isUnbounded
  rootElement
  =
  case _ of
       Focus -> H.modify_ $ setStateFocusedEfficiently true
       Blur -> H.modify_ $ setStateFocusedEfficiently false

       TouchActivate touchEvent -> activationLogic isDisabled isUnbounded rootElement getNormalizedEventCoordsTouchEvent touchEvent
       MouseActivate mouseEvent -> activationLogic isDisabled isUnbounded rootElement getNormalizedEventCoordsMouseEvent mouseEvent

       Deactivate pointerReleasedSubscriptionId -> do
          H.unsubscribe pointerReleasedSubscriptionId

          state <- H.get

          case state.activationState of
               ActivationState__Activated -> do
                  H.modify_ $ setStateActivationStateEfficiently ActivationState__Deactivated

                  -- TODO: how not to register if already registered?
                  void $ H.subscribe (Utils.mkTimeoutEvent DeactivationEnded numbers."FG_DEACTIVATION_MS")
               _ -> pure unit

       DeactivationEnded -> H.modify_ \state ->
         case state.activationState of
              ActivationState__Deactivated -> state { activationState = ActivationState__Idle }
              ActivationState__Activated -> state
              ActivationState__Idle -> state
