module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Implementation
  , module Insides
  ) where

import HalogenMWC.Button.Ripple
import Protolude

import DOM.HTML.Indexed as I
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
      , HP.ref (H.RefLabel "button")
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
  | TouchActivation TouchEvent
  | MouseActivation MouseEvent
  | KeyActivation
  | Deactivation

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

type State w i =
  { input                   :: Input w i
  , focusedClass            :: Boolean
  , styleCommonVars         :: StyleCommonVars
  , styleVars               :: StyleVars
  , previousActivationEvent :: Maybe Web.Event.Event.Event
  , activationState ::
    { activationEvent       :: Maybe Web.Event.Event.Event
    , hasDeactivationUXRun  :: Boolean
    , isActivated           :: Boolean
    , isProgrammatic        :: Boolean
    , wasActivatedByPointer :: Boolean
    , wasElementMadeActive  :: Boolean
    }
  }

isUnbounded :: Boolean
isUnbounded = false

button :: H.Component Query (Input (H.ComponentSlot ChildSlots Aff Action) Action) Message Aff
button =
  H.mkComponent
    { initialState: \input ->
      { input
      , focusedClass: false
      , styleCommonVars:
          { "VAR_FG_SCALE": "0" -- https://github.com/material-components/material-components-web/blob/83d83f131118073943a6a45923b37b3a961bd894/packages/mdc-ripple/foundation.ts#L103
          , "VAR_FG_SIZE": "0px"
          }
      , styleVars: StyleVars__Empty
      , previousActivationEvent: Nothing
      , activationState: defaultActivationState
      }
    , render: \state ->
        let
          rippleClasses =
            [ cssClasses."ROOT" ]
            <> if isUnbounded then [ cssClasses."UNBOUNDED" ] else []
            <> if (spy "render state" state).focusedClass then [ cssClasses."BG_FOCUSED" ] else []
            <> if state.activationState.isActivated then [ cssClasses."FG_ACTIVATION" ] else []
        in
          Implementation.wrapTouch
            [ HH.button
              ( [ HP.classes $
                  spy "classes" $
                  ( Implementation.commonClasses state.input.variant
                  <> rippleClasses
                  <> state.input.config.additionalClasses
                  )
                , HP.style $ String.joinWith "; "
                  let styleVar x y = x <> ": " <> y
                   in
                    spy "style" $
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
                , HP.ref (H.RefLabel "button")

                -- | , HE.handler (EventType "pointerdown") (const PointerActivation) -- TODO
                , HE.onTouchStart TouchActivation
                , HE.onMouseDown MouseActivation
                -- | , HE.onKeyDown (const KeyActivation)
                -- | , HE.onKeyUp (const Deactivation) -- only when onKeyDown worked?

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

defaultActivationState =
  { activationEvent: Nothing
  , hasDeactivationUXRun: false
  , isActivated: false
  , isProgrammatic: false
  , wasActivatedByPointer: false
  , wasElementMadeActive: false
  }

getButtonHtmlElementRef = H.getHTMLElementRef (H.RefLabel "button")

layoutAndModifyState :: H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
layoutAndModifyState = getButtonHtmlElementRef >>= traverse_ \(htmlElement :: Web.HTML.HTMLElement) -> do
  (rootDomRect :: Web.HTML.HTMLElement.DOMRect) <- H.liftEffect $ Web.HTML.HTMLElement.getBoundingClientRect htmlElement

  let { maxRadius, initialSize, fgScale } = layoutInternal { rootDomRect, isUnbounded }

  H.modify_ \state -> spy "state after" $ state
    { styleCommonVars = updateCssVarsCommon { initialSize, fgScale }
    , styleVars =
        if isUnbounded
          then StyleVars__Unbounded $ updateCssVarsUnbounded { initialSize, rootDomRect }
          else StyleVars__Empty
    }

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
       Focus -> H.modify_ \state -> spy "state after" $ state { focusedClass = true }
       Blur -> H.modify_ \state -> spy "state after" $ state { focusedClass = false }
       WindowResized -> layoutAndModifyState
       TouchActivation touchEvent -> pure unit
       MouseActivation mouseEvent -> do
          state <- H.get

          if state.input.config.disabled then pure unit else
            if state.activationState.isActivated then pure unit else
              getButtonHtmlElementRef >>= traverse_ \(htmlElement :: Web.HTML.HTMLElement) -> do
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

                H.modify_ \state' -> spy "state after" $ state'
                  { activationState =
                    { activationEvent:       state'.activationState.activationEvent
                    , hasDeactivationUXRun:  state'.activationState.hasDeactivationUXRun
                    , isActivated:           true
                    , isProgrammatic:        true
                    , wasActivatedByPointer: true
                    , wasElementMadeActive:  state'.activationState.wasElementMadeActive
                    }
                  , styleCommonVars = styleCommonVars
                  , styleVars = styleVars
                  }
       KeyActivation -> pure unit
       Deactivation -> pure unit

       -- | Finalize -> do
       -- |   case state.activationTimer_ of
       -- |        Just activationTimer_ -> clearTimeout(this.activationTimer_)
       -- |        _ -> pure unit

       -- |   case state.fgDeactivationRemovalTimer_ of
       -- |        Just fgDeactivationRemovalTimer_ -> clearTimeout(this.fgDeactivationRemovalTimer_);
       -- |        _ -> pure unit

       -- |   if isUnbounded then window.removeEventListener('resize', handler)
