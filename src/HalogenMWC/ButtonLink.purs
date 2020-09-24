module HalogenMWC.ButtonLink
  ( module HalogenMWC.ButtonLink
  , module Implementation
  , module Insides
  , module HalogenMWC.Implementation.Button.WithRippleCommon
  ) where

import Protolude
import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Implementation.Button.HTML (Variant)
import HalogenMWC.Implementation.Button.HTML (Variant(..), commonClasses, commonHtml, wrapTouch) as Implementation
import HalogenMWC.Implementation.Button.Insides (buttonIconMaterialIcons, buttonLabel) as Insides
import HalogenMWC.Implementation.Button.WithRippleCommon
import HalogenMWC.Ripple.Bounded as Ripple


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
          { additionalClasses: Ripple.rippleClasses state.rippleState <> state.input.config.additionalClasses
          , additionalAttributes:
            [ HP.style (Ripple.rippleStyles state.rippleState)
            , HE.onClick (const Click)
            , HP.ref buttonRefLabel
            ]
            <> (Ripple.rippleProps <#> map RippleAction)
            <> state.input.config.additionalAttributes
          }
          state.input.content
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
      }
  where
    disabled :: Boolean
    disabled = false

    handleAction :: Action -> H.HalogenM (State Config (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
    handleAction =
      case _ of
          RippleAction rippleAction -> H.getHTMLElementRef buttonRefLabel >>= traverse_ \rootElement -> do
            state <- H.get

            liftRippleHandleAction state $ Ripple.handleAction disabled rootElement rippleAction
          Click -> H.raise Message__Clicked
