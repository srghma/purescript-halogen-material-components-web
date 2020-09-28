module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Export
  ) where

import Protolude hiding (Variant)

import HalogenMWC.Implementation.Button.WithRippleCommon as Export
import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Implementation.Button.HTML (Variant(..)) as Export
import HalogenMWC.Implementation.Button.HTML
import HalogenMWC.Implementation.Button.Insides as Export
import HalogenMWC.Ripple.Bounded as Ripple
import HalogenMWC.Implementation.Button.WithRippleCommon

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

button :: forall slots . H.Component Query (Input Config (H.ComponentSlot slots Aff Action) Action) Message Aff
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
    handleAction :: Action -> H.HalogenM (State Config (H.ComponentSlot slots Aff Action) Action) Action slots Message Aff Unit
    handleAction =
      case _ of
          RippleAction rippleAction -> H.getHTMLElementRef buttonRefLabel >>= traverse_ \rootElement -> do
            state <- H.get

            liftRippleHandleAction state $ Ripple.handleAction state.input.config.disabled rootElement rippleAction
          Click -> H.raise Message__Clicked
