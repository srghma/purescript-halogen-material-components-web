module HalogenMWC.Implementation.Button
  ( module HalogenMWC.Button
  , module Implementation
  , module Insides
  , module HalogenMWC.Button.WithRippleCommon
  ) where

import HalogenMWC.Button.WithRippleCommon
import Protolude

import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Button.Implementation (Variant(..), commonClasses, commonHtml, wrapTouch) as Implementation
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Insides (buttonIconMaterialIcons, buttonLabel) as Insides
import HalogenMWC.Ripple.Bounded (handleAction) as Ripple
import HalogenMWC.Ripple.Common (RippleAction__Common, RippleState, initialRippleState) as Ripple
import HalogenMWC.Ripple.HTML (rippleClasses, rippleProps, rippleStyles) as Ripple

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
        [ HH.button
          (commonProps <> config.additionalAttributes)
          (Implementation.commonHtml content)
        ]

------------------------------------------------

button :: H.Component Query (Input Config (H.ComponentSlot ChildSlots Aff Action) Action) Message Aff
button =
  H.mkComponent
    { initialState: initialState
    , render: \state ->
        buttonView
          state.input.variant
          { disabled: state.input.config.disabled
          , additionalClasses: Ripple.rippleClasses isUnbounded state.rippleState <> state.input.config.additionalClasses
          , additionalAttributes:
            [ HP.style (Ripple.rippleStyles state.rippleState)
            , HE.onClick (const Click)
            ]
            <> (Ripple.rippleProps <#> map RippleAction)
            <> state.input.config.additionalAttributes
          }
          state.input.content
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
      }

handleAction :: Action -> H.HalogenM (State Config (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
handleAction =
  case _ of
       RippleAction rippleAction -> do
         state <- H.get

         liftRippleHandleAction state $ Ripple.handleAction state.input.config.disabled (H.getHTMLElementRef buttonRefLabel) rippleAction
       Click -> H.raise Clicked
