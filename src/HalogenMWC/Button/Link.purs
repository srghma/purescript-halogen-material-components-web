module HalogenMWC.Button.Link
  ( module HalogenMWC.Button.Link
  , module Implementation
  , module Insides
  ) where

import Protolude
import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Implementation (Variant(..), commonClasses, commonHtml, wrapTouch) as Implementation
import HalogenMWC.Button.Insides (buttonIconMaterialIcons, buttonLabel) as Insides
import HalogenMWC.Ripple.Common (RippleAction__Common, RippleState, initialRippleState) as Ripple
import HalogenMWC.Ripple.Bounded (handleAction) as Ripple
import HalogenMWC.Ripple.HTML (rippleClasses, rippleProps, rippleStyles) as Ripple
import HalogenMWC.Button.WithRippleCommon


type Config i =
  { additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLa i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { additionalClasses: []
  , additionalAttributes: []
  }

buttonLinkView :: forall w i. Variant -> Config i -> Array (HH.HTML w i) -> HH.HTML w i
buttonLinkView variant config =
  let
    commonProps =
      [ HP.classes (Implementation.commonClasses variant <> config.additionalClasses)
      , HP.ref buttonRefLabel
      ]
  in
    \content ->
      Implementation.wrapTouch
        [ HH.a
          (commonProps <> config.additionalAttributes)
          (Implementation.commonHtml content)
        ]

------------------------------------------------

buttonLink :: H.Component Query (Input Config (H.ComponentSlot ChildSlots Aff Action) Action) Message Aff
buttonLink =
  H.mkComponent
    { initialState: initialState
    , render: \state ->
        buttonLinkView
          state.input.variant
          { additionalClasses: Ripple.rippleClasses isUnbounded state.rippleState <> state.input.config.additionalClasses
          , additionalAttributes:
            [ HP.style (Ripple.rippleStyles state.rippleState) ]
            <> (Ripple.rippleProps <#> map RippleAction)
            <> state.input.config.additionalAttributes
          }
          state.input.content
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
      }

disabled :: Boolean
disabled = false

handleAction :: Action -> H.HalogenM (State Config (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
handleAction =
  case _ of
       RippleAction rippleAction -> do
         state <- H.get

         liftRippleHandleAction state $ Ripple.handleAction disabled (H.getHTMLElementRef buttonRefLabel) rippleAction
