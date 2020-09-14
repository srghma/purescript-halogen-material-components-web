module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Implementation
  , module Insides
  ) where

import HalogenMWC.Button.Ripple
import Protolude

import DOM.HTML.Indexed as I
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

type Input w i = { variant :: Variant, config :: Config i, content :: Array (HH.HTML w i) }

data Action
  = Initialize

type Message = Void

type ChildSlots = ()

type State w i = Input w i

button :: H.Component Query (Input (H.ComponentSlot ChildSlots Aff Action) Action) Message Aff
button =
  H.mkComponent
    { initialState: identity
    , render: \{ variant, config, content } ->
        let
          unbounded = false

          rippleClasses =
            [ cssClasses."ROOT" ]
            <> if unbounded then [ cssClasses."UNBOUNDED" ] else []
            <> if state.focusedClass then [ cssClasses."BG_FOCUSED" ] else []

          commonProps =
            [ HP.classes (Implementation.commonClasses variant <> rippleClasses <> config.additionalClasses)
            , HP.disabled config.disabled
            , HP.tabIndex (if config.disabled then -1 else 0)
            , HP.ref (H.RefLabel "button")
            , HE.onFocus (const Focus)
            , HE.onBlur (const Blur)

            -- | HE.pointerdown
            , HE.onTouchStart (const Activation)
            , HE.onMouseDown (const Activation)
            , HE.onKeyDown (const Activation)
            , HE.onKeyUp (const Deactivation) -- only when onKeyDown worked?
            ]
        in
          Implementation.wrapTouch
            [ HH.button
              (commonProps <> config.additionalAttributes)
              (Implementation.commonHtml content)
            ]
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        -- | , finalize = Just Finalize
        , handleAction = handleAction
        }
    }

handleAction :: Action -> H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
handleAction =
  case _ of
       Focus -> HH.modify_ \state ->
         if not state.focusedClass
           then state { focusedClass = true }
           else state
       Blur -> HH.modify_ \state ->
         if state.focusedClass
           then state { focusedClass = false }
           else state
       Initialize -> if unbounded then window.addEventListener('resize', handler)
       Finalize -> do
         case state.activationTimer_ of
              Just activationTimer_ -> clearTimeout(this.activationTimer_);
              _ -> pure unit

         case state.fgDeactivationRemovalTimer_ of
              Just fgDeactivationRemovalTimer_ -> clearTimeout(this.fgDeactivationRemovalTimer_);
              _ -> pure unit

         if unbounded then window.removeEventListener('resize', handler)
