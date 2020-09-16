module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Implementation
  , module Insides
  ) where

import HalogenMWC.Button.Ripple
import Protolude

import DOM.HTML.Indexed as I
import Data.Array (concat) as Array
import Data.String as String
import FRP.Event (Event) as Event
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
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web.Event.Event
import Web.HTML as Web.HTML
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.Window (Window)
import Web.HTML.Window as Web.HTML.Window
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type Config i =
  { disabled :: Boolean
  , additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLbutton i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { disabled: false
  , additionalClasses: []
  , additionalAttributes: []
  }

buttonView :: forall w i. Variant -> Config i -> Array (HH.HTML w i) -> HH.HTML w i
buttonView variant config =
  let
    commonProps =
      [ HP.classes (Implementation.commonClasses variant <> config.additionalClasses)
      , HP.disabled config.disabled
      , HP.tabIndex (if config.disabled then -1 else 0)
      , HP.ref buttonRefLabel
      ]
  in
    \content ->
      Implementation.wrapTouch
        [ HH.button (commonProps <> config.additionalAttributes) (Implementation.commonHtml content)
        ]

type Query = Const Void

type Input w i =
  { variant :: Variant
  , config :: Config i
  , content :: Array (HH.HTML w i)
  }

data Action
  = Initialize
  | Focus
  | Blur
  | WindowResized
  | TouchActivate TouchEvent
  | MouseActivate MouseEvent
  | KeyActivate
  | Deactivate

type Message = Void

type ChildSlots = ()

type StyleCommonVars =
  { "VAR_FG_SCALE" :: String
  , "VAR_FG_SIZE"  :: String
  }

data StyleVars
  = StyleVars__Empty
  | StyleVars__NonUnbounded
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

type State w i =
  { input                   :: Input w i
  , focused            :: Boolean
  , styleCommonVars         :: StyleCommonVars
  , styleVars               :: StyleVars
  , previousActivationEvent :: Maybe Web.Event.Event.Event
  , activationState :: ActivationState
  }

isUnbounded :: Boolean
isUnbounded = false

styleVar x y = x <> ": " <> y

buttonRefLabel = H.RefLabel "button"

button :: H.Component Query (Input (H.ComponentSlot ChildSlots Aff Action) Action) Message Aff
button =
  H.mkComponent
    { initialState: \input ->
      { input
      , styleCommonVars:
          { "VAR_FG_SCALE": "0" -- https://github.com/material-components/material-components-web/blob/83d83f131118073943a6a45923b37b3a961bd894/packages/mdc-ripple/foundation.ts#L103
          , "VAR_FG_SIZE": "0px"
          }
      , styleVars: StyleVars__Empty
      , previousActivationEvent: Nothing
      , focused: false
      , activationState: ActivationState__Idle
      }
    , render: \state ->
        let
          rippleClasses =
            Array.concat
            [ [ cssClasses."ROOT" ]
            , if isUnbounded then [ cssClasses."UNBOUNDED" ] else []
            , if state.focused then [ cssClasses."BG_FOCUSED" ] else []
            , case state.activationState of
                   ActivationState__Idle -> []
                   ActivationState__Activated -> [ cssClasses."FG_ACTIVATION" ]
                   ActivationState__Deactivated -> [ cssClasses."FG_DEACTIVATION" ]
            ]
        in
          Implementation.wrapTouch
            [ HH.button
              ( [ HP.classes $
                  spy "render classes" $
                  ( Implementation.commonClasses state.input.variant
                  <> rippleClasses
                  <> state.input.config.additionalClasses
                  )
                , HP.style $ String.joinWith "; " $ spy "render style" $
                    [ styleVar strings."VAR_FG_SCALE" state.styleCommonVars."VAR_FG_SCALE"
                    , styleVar strings."VAR_FG_SIZE" state.styleCommonVars."VAR_FG_SIZE"
                    ] <>
                      case state.styleVars of
                           StyleVars__Empty -> []
                           StyleVars__Unbounded styleVars__Unbounded ->
                             [ styleVar strings."VAR_LEFT" styleVars__Unbounded."VAR_LEFT"
                             , styleVar strings."VAR_TOP" styleVars__Unbounded."VAR_TOP"
                             ]
                           StyleVars__NonUnbounded styleVars__NonUnbounded ->
                             [ styleVar strings."VAR_FG_TRANSLATE_END" styleVars__NonUnbounded."VAR_FG_TRANSLATE_END"
                             , styleVar strings."VAR_FG_TRANSLATE_START" styleVars__NonUnbounded."VAR_FG_TRANSLATE_START"
                             ]
                , HP.disabled state.input.config.disabled
                , HP.tabIndex (if state.input.config.disabled then -1 else 0)
                , HP.ref buttonRefLabel

                -- | , HE.handler (EventType "pointerdown") (const PointerActivation) -- TODO
                , HE.onTouchStart TouchActivate
                , HE.onMouseDown MouseActivate
                -- | , HE.onKeyDown (const KeyActivate)
                -- | , HE.onKeyUp (const Deactivate) -- only when onKeyDown worked?

                , HE.onFocus (const Focus)
                , HE.onBlur (const Blur)
                ]
              <> state.input.config.additionalAttributes
              )
              (Implementation.commonHtml state.input.content)
            ]
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        -- | , finalize = Just Finalize
        , handleAction = handleAction
        }
    }

layoutAndModifyState :: H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
layoutAndModifyState = do
  traceM "layoutAndModifyState"

  H.getHTMLElementRef buttonRefLabel >>= traverse_ \(htmlElement :: Web.HTML.HTMLElement) -> do
    traceM { message: "layoutAndModifyState updating", htmlElement }

    (rootDomRect :: Web.HTML.HTMLElement.DOMRect) <- H.liftEffect $ Web.HTML.HTMLElement.getBoundingClientRect htmlElement

    let { maxRadius, initialSize, fgScale } = layoutInternal { rootDomRect, isUnbounded }

    H.modify_ \state -> spy "layoutAndModifyState state after modify" $ state
      { styleCommonVars = updateCssVarsCommon { initialSize, fgScale }
      , styleVars =
          if isUnbounded
            then StyleVars__Unbounded $ updateCssVarsUnbounded { initialSize, rootDomRect }
            else StyleVars__Empty
      }

    newState <- H.get

    traceM { message: "layoutAndModifyState newState", newState }

handleAction :: Action -> H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
handleAction action =
  case spy "action" action of
       Initialize ->
          when isUnbounded do
            window <- H.liftEffect Web.HTML.window

            let event :: Event.Event Web.Event.Event.Event
                event = Utils.eventListenerEventSourceWithOptions (EventType "resize") Utils.unsafePassiveIfSupportsAddEventListenerOptions (Web.HTML.Window.toEventTarget window)

                event' :: Event.Event Action
                event' = event <#> const WindowResized

            void $ H.subscribe event'

            layoutAndModifyState
       Focus -> H.modify_ \state -> spy "Focus state after" $ if state.focused then state else state { focused = true }
       Blur -> H.modify_ \state -> spy "Blur state after" $ if state.focused then state { focused = false } else state
       WindowResized -> layoutAndModifyState
       TouchActivate touchEvent -> pure unit
       MouseActivate mouseEvent -> do
          state <- H.get

          case state.input.config.disabled, state.activationState of
               false, ActivationState__Idle -> do
                 H.getHTMLElementRef buttonRefLabel >>= traverse_ \(htmlElement :: Web.HTML.HTMLElement) -> do
                   ({ rootDomRect
                     , scrollX
                     , scrollY
                     }
                   ) <- H.liftEffect do
                       (rootDomRect :: Web.HTML.HTMLElement.DOMRect) <- Web.HTML.HTMLElement.getBoundingClientRect htmlElement
                       (window :: Window) <- Web.HTML.window
                       (scrollX :: Int) <- Web.HTML.Window.scrollX window
                       (scrollY :: Int) <- Web.HTML.Window.scrollY window
                       pure
                         { rootDomRect
                         , scrollX
                         , scrollY
                         }

                   let { maxRadius, initialSize, fgScale } = layoutInternal { rootDomRect, isUnbounded }

                   let (styleCommonVars :: StyleCommonVars) = updateCssVarsCommon { initialSize, fgScale }
                   let (styleVars :: StyleVars) =
                         if isUnbounded
                           then StyleVars__Unbounded $ updateCssVarsUnbounded { initialSize, rootDomRect }
                           else let
                             (normalizedEventCoords :: MDCRipplePoint) = getNormalizedEventCoordsMouseEvent
                               { mouseEvent
                               , scrollX
                               , scrollY
                               , rootDomRect
                               }

                             (fgTranslationCoordinates :: FgTranslationCoordinates) = getFgTranslationCoordinatesPonter
                               { normalizedEventCoords
                               , initialSize
                               , rootDomRect
                               }

                             in StyleVars__NonUnbounded $ fgTranslationCoordinatesToTranslateForUnbounded fgTranslationCoordinates

                   H.modify_ \state' -> spy "MouseActivate state after" $ state'
                     { activationState = ActivationState__Activated
                     , focused = true
                     , styleCommonVars = styleCommonVars
                     , styleVars = styleVars
                     }
               _, _ -> pure unit
       KeyActivate -> pure unit
       Deactivate -> do
          state <- H.get

          case state.activationState of
               ActivationState__Activated -> do
                  H.modify_ \state' -> spy "Deactivate state after" $ state'
                    { activationState = ActivationState__Deactivated
                    }
                  -- | this.adapter.deregisterInteractionHandler('keyup', this.deactivateHandler_);
                  -- | POINTER_DEACTIVATION_EVENT_TYPES.forEach((evtType) => {
                  -- |   this.adapter.deregisterDocumentInteractionHandler(
                  -- |       evtType, this.deactivateHandler_);
                  -- | });

                  -- | requestAnimationFrame(() => {
                  -- |   this.activationState_.hasDeactivationUXRun = true;

                  -- |   if (wasActivatedByPointer || wasElementMadeActive) {
                  -- |     // This method is called both when a pointing device is released, and when the activation animation ends.
                  -- |     // The deactivation animation should only run after both of those occur.
                  -- |     const {FG_DEACTIVATION} = MDCRippleFoundation.cssClasses;
                  -- |     const {hasDeactivationUXRun, isActivated} = this.activationState_;
                  -- |     const activationHasEnded = hasDeactivationUXRun || !isActivated;

                  -- |     if (activationHasEnded && this.activationAnimationHasEnded_) {
                  -- |       const {FG_ACTIVATION} = MDCRippleFoundation.cssClasses;
                  -- |       this.adapter.removeClass(FG_ACTIVATION);
                  -- |       this.activationAnimationHasEnded_ = false;
                  -- |       this.adapter.computeBoundingRect();

                  -- |       this.adapter.addClass(FG_DEACTIVATION);
                  -- |       this.fgDeactivationRemovalTimer_ = setTimeout(() => {
                  -- |         this.adapter.removeClass(FG_DEACTIVATION);
                  -- |       }, numbers.FG_DEACTIVATION_MS);
                  -- |     }
                  -- |   }

                  -- |   this.previousActivationEvent_ = this.activationState_.activationEvent;
                  -- |   this.activationState_ = this.defaultActivationState_();
                  -- |   // Touch devices may fire additional events for the same interaction within a short time.
                  -- |   // Store the previous event until it's safe to assume that subsequent events are for new interactions.
                  -- |   setTimeout(() => this.previousActivationEvent_ = undefined, MDCRippleFoundation.numbers.TAP_DELAY_MS);
                  -- | });
               _ -> pure unit
       -- | Finalize -> do
       -- |   case state.activationTimer_ of
       -- |        Just activationTimer_ -> clearTimeout(this.activationTimer_)
       -- |        _ -> pure unit

       -- |   case state.fgDeactivationRemovalTimer_ of
       -- |        Just fgDeactivationRemovalTimer_ -> clearTimeout(this.fgDeactivationRemovalTimer_);
       -- |        _ -> pure unit

       -- |   if isUnbounded then window.removeEventListener('resize', handler)
