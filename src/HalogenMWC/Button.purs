module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Implementation
  , module Insides
  ) where

import HalogenMWC.Button.Ripple
import Protolude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.String as String
import FRP.Event as Event
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
import Web.DOM (Element)
import Web.DOM.Document as Web.DOM.Document
import Web.DOM.Element as Web.DOM.Element
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web.Event.Event
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
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
  | Deactivate H.SubscriptionId
  | DeactivationEnded

type Message = Void

type ChildSlots = ()

type StyleCommonVars =
  { "VAR_FG_SCALE" :: String
  , "VAR_FG_SIZE"  :: String
  }

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

type State w i =
  { input                   :: Input w i
  , focused                 :: Boolean
  , styleCommonVars         :: StyleCommonVars
  , styleVars               :: StyleVars
  , previousActivationEvent :: Maybe Web.Event.Event.Event
  , activationState         :: ActivationState
  }

setStateActivationStateEfficiently :: forall w i . State w i -> ActivationState -> State w i
setStateActivationStateEfficiently state new = if state.activationState == new then state else state { activationState = new }

setStateFocusedEfficiently :: forall w i . State w i -> Boolean -> State w i
setStateFocusedEfficiently state new = if state.focused == new then state else state { focused = new }

isUnbounded :: Boolean
isUnbounded = false

styleVar :: String -> String -> String
styleVar x y = x <> ": " <> y

buttonRefLabel :: H.RefLabel
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
                  ( Implementation.commonClasses state.input.variant
                  <> rippleClasses
                  <> state.input.config.additionalClasses
                  )
                , HP.style $ String.joinWith "; " $
                    [ styleVar strings."VAR_FG_SCALE" state.styleCommonVars."VAR_FG_SCALE"
                    , styleVar strings."VAR_FG_SIZE" state.styleCommonVars."VAR_FG_SIZE"
                    ] <>
                      case state.styleVars of
                           StyleVars__Empty -> []
                           StyleVars__Unbounded styleVars__Unbounded ->
                             [ styleVar strings."VAR_LEFT" styleVars__Unbounded."VAR_LEFT"
                             , styleVar strings."VAR_TOP" styleVars__Unbounded."VAR_TOP"
                             ]
                           StyleVars__Bounded styleVars__Bounded ->
                             [ styleVar strings."VAR_FG_TRANSLATE_END" styleVars__Bounded."VAR_FG_TRANSLATE_END"
                             , styleVar strings."VAR_FG_TRANSLATE_START" styleVars__Bounded."VAR_FG_TRANSLATE_START"
                             ]
                , HP.disabled state.input.config.disabled
                , HP.tabIndex (if state.input.config.disabled then -1 else 0)
                , HP.ref buttonRefLabel

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
  H.getHTMLElementRef buttonRefLabel >>= traverse_ \(htmlElement :: Web.HTML.HTMLElement) -> do
    (rootDomRect :: Web.HTML.HTMLElement.DOMRect) <- H.liftEffect $ Web.HTML.HTMLElement.getBoundingClientRect htmlElement

    let { maxRadius, initialSize, fgScale } = layoutInternal { rootDomRect, isUnbounded }

    H.modify_ \state -> state
      { styleCommonVars = updateCssVarsCommon { initialSize, fgScale }
      , styleVars =
          if isUnbounded
            then StyleVars__Unbounded $ updateCssVarsUnbounded { initialSize, rootDomRect }
            else StyleVars__Empty
      }

activationLogicGo
  :: forall event
   .  ( { event :: event
        , scrollX :: Int
        , scrollY :: Int
        , rootDomRect :: Web.HTML.HTMLElement.DOMRect
        }
        -> MDCRipplePoint
      )
  -> event
  -> H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
activationLogicGo getNormalizedEventCoords event =
  H.getHTMLElementRef buttonRefLabel >>= traverse_ \(htmlElement :: Web.HTML.HTMLElement) -> do
    ( { rootDomRect
      , scrollX
      , scrollY
      , documentElement
      }
    ) <- H.liftEffect do
        (rootDomRect :: Web.HTML.HTMLElement.DOMRect) <- Web.HTML.HTMLElement.getBoundingClientRect htmlElement
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
  :: forall event
   .  ( { event :: event
        , scrollX :: Int
        , scrollY :: Int
        , rootDomRect :: Web.HTML.HTMLElement.DOMRect
        }
        -> MDCRipplePoint
      )
  -> event
  -> H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
activationLogic getNormalizedEventCoords event = do
  state <- H.get

  if state.input.config.disabled then pure unit else
    case state.activationState of
        ActivationState__Idle -> activationLogicGo getNormalizedEventCoords event
        ActivationState__Deactivated -> activationLogicGo getNormalizedEventCoords event
        _ -> pure unit

handleAction :: Action -> H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
handleAction =
  case _ of
       Initialize ->
          when isUnbounded do
            window <- H.liftEffect Web.HTML.window

            let event :: Event.Event Action
                event =
                  (Utils.eventListenerEventSourceWithOptions (EventType "resize") Utils.unsafePassiveIfSupportsAddEventListenerOptions (Web.HTML.Window.toEventTarget window))
                  <#> const WindowResized

            void $ H.subscribe event

            layoutAndModifyState

       Focus -> H.modify_ \state -> setStateFocusedEfficiently state true
       Blur -> H.modify_ \state -> setStateFocusedEfficiently state false

       WindowResized -> layoutAndModifyState

       TouchActivate touchEvent -> activationLogic getNormalizedEventCoordsTouchEvent touchEvent
       MouseActivate mouseEvent -> activationLogic getNormalizedEventCoordsMouseEvent mouseEvent

       Deactivate pointerReleasedSubscriptionId -> do
          H.unsubscribe pointerReleasedSubscriptionId

          state <- H.get

          case state.activationState of
               ActivationState__Activated -> do
                  H.modify_ \state' -> setStateActivationStateEfficiently state' ActivationState__Deactivated

                  -- TODO: how not to register if already registered?
                  void $ H.subscribe (Utils.mkTimeoutEvent DeactivationEnded numbers."FG_DEACTIVATION_MS")
               _ -> pure unit

       DeactivationEnded -> H.modify_ \state' -> setStateActivationStateEfficiently state' ActivationState__Idle
