module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Export
  ) where

import HalogenMWC.Implementation.Button.HTML (ButtonVariant, commonClasses, commonHtml, wrapTouch)
import HalogenMWC.Implementation.Button.WithRippleCommon (Action(..), Input, Message(..), Query, buttonRefLabel, initialState, liftRippleHandleAction)
import HalogenMWC.Utils (fromPlainHTMLArray, fromPlainIPropArray)
import Protolude

import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Implementation.Button.HTML (ButtonVariant(..)) as Export
import HalogenMWC.Implementation.Button.Insides (buttonIconMaterialIcons, buttonLabel) as Export
import HalogenMWC.Implementation.Button.WithRippleCommon (Action(..), Input, Message(..), Query, State, buttonRefLabel, initialState, isUnbounded, liftRippleHandleAction) as Export
import HalogenMWC.Ripple.Bounded as Ripple

type Config =
  { disabled :: Boolean
  , additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLbutton Void)
  }

defaultConfig :: Config
defaultConfig =
  { disabled: false
  , additionalClasses: []
  , additionalAttributes: []
  }

buttonView
  :: forall w i
   . ButtonVariant
  -> { disabled :: Boolean
     , additionalClasses :: Array ClassName
     , additionalAttributes :: Array (IProp I.HTMLbutton i)
     }
  -> Array (HH.HTML w i)
  -> HH.HTML w i
buttonView variant config =
  let
    commonProps =
      [ HP.classes (commonClasses variant <> config.additionalClasses)
      , HP.disabled config.disabled
      , HP.tabIndex (if config.disabled then -1 else 0)
      ]
  in
    \content ->
      wrapTouch
        [ HH.button
          (commonProps <> config.additionalAttributes)
          (commonHtml content)
        ]

------------------------------------------------

button :: forall slots . H.Component Query (Input Config) Message Aff
button =
  H.mkComponent
    { initialState: initialState
    , render: \state ->
        buttonView
          state.input.variant
          { disabled: state.input.config.disabled
          , additionalClasses: Ripple.rippleClasses state.rippleState <> state.input.config.additionalClasses
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
        , receive = \input -> Just $ Action__Receive input
        }
      }
  where
    -- | handleAction :: Action -> H.HalogenM (State Config (H.ComponentSlot slots Aff Action) Action) Action slots Message Aff Unit
    -- | handleAction :: Action Config -> H.HalogenM (State Config) (Action Config) slots Message Aff Unit
    handleAction =
      case _ of
          Action__RippleAction rippleAction -> H.getHTMLElementRef buttonRefLabel >>= traverse_ \rootElement -> do
            state <- H.get

            liftRippleHandleAction state $ Ripple.handleAction state.input.config.disabled rootElement rippleAction
          Action__Click -> H.raise Message__Clicked

          -- TODO: (setEfficiently (Lens.prop (SProxy :: SProxy "input")))
          Action__Receive input -> H.modify_ (\state -> state { input = input })
