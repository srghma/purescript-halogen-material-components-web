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
import HalogenMWC.Implementation.Button.HTML (ButtonVariant)
import HalogenMWC.Implementation.Button.HTML (ButtonVariant(..), commonClasses, commonHtml, wrapTouch) as Implementation
import HalogenMWC.Implementation.Button.Insides (buttonIconMaterialIcons, buttonLabel) as Insides
import HalogenMWC.Implementation.Button.WithRippleCommon (Action(..), Input, Message(..), Query, State, buttonRefLabel, initialState, isUnbounded, liftRippleHandleAction)
import HalogenMWC.Ripple.Bounded as Ripple
import HalogenMWC.Utils (fromPlainHTMLArray, fromPlainIPropArray)


type Config =
  { additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLa Void)
  }

defaultConfig :: Config
defaultConfig =
  { additionalClasses: []
  , additionalAttributes: []
  }

buttonLinkView
  :: forall w i
   . ButtonVariant
  -> { additionalClasses :: Array ClassName
     , additionalAttributes :: Array (IProp I.HTMLa i)
     }
  -> Array (HH.HTML w i)
  -> HH.HTML w i
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

buttonLink :: H.Component Query (Input Config) Message Aff
buttonLink =
  H.mkComponent
    { initialState: initialState
    , render: \state ->
        buttonLinkView
          state.input.variant
          { additionalClasses: Ripple.rippleClasses state.rippleState <> state.input.config.additionalClasses
          , additionalAttributes:
            [ HP.style (Ripple.rippleStyles state.rippleState)
            , HE.onClick (const Action__Click)
            , HP.ref buttonRefLabel
            ]
            <> (Ripple.rippleProps <#> map Action__RippleAction)
            <> fromPlainIPropArray state.input.config.additionalAttributes
          }
          (fromPlainHTMLArray state.input.content)
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
      }
  where
    disabled :: Boolean
    disabled = false

    -- | handleAction :: Action Config -> H.HalogenM (State Config) (Action Config) slots Message Aff Unit
    handleAction =
      case _ of
          Action__RippleAction rippleAction -> H.getHTMLElementRef buttonRefLabel >>= traverse_ \rootElement -> do
            state <- H.get

            liftRippleHandleAction state $ Ripple.handleAction disabled rootElement rippleAction
          Action__Click -> H.raise Message__Clicked
          Action__Receive input -> H.modify_ (\state -> state { input = input })
