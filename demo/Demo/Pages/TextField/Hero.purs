module Demo.Pages.TextField.Hero where

import Protolude

import Data.Lens.Record (prop) as Lens
import Demo.HOC.CatalogPage (CatalogPage)
import Demo.MkComponent.WithFocus as WithFocus
import Demo.Pages.TextField.Css as Css
import Demo.Utils (mkComponentStatic)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.HelperText as HelperText
import HalogenMWC.TextArea as TextArea
import HalogenMWC.TextField.Filled as TextField.Filled
import HalogenMWC.Utils (setEfficiently)
import Material.Classes.Typography (mdc_typography____subtitle1)

type Query = Const Void

type Input = Unit

type Message = Void

data Action
  = MessageFromFilled TextField.Filled.Message

type ChildSlots =
  ( filled :: H.Slot TextField.Filled.Query TextField.Filled.Message Unit
  )

type State =
  { filledValue :: String
  }

component :: H.Component Query Input Message Aff
component =
  H.mkComponent
    { initialState: \_ ->
      { filledValue: "prefilled"
      }
    , render: \state ->
        HH.div
        [ HP.class_ Css.styles.textFieldContainerHeroRoot
        ]
        [ HH.div
          [ HP.class_ Css.styles.textFieldContainerHero ]
          [ HH.slot
            (SProxy :: SProxy "filled")
            unit
            TextField.Filled.filled
              ( TextField.Filled.defaultConfig
                { label: TextField.Filled.LabelConfig__With { id: "hero-filled", labelText: "Standard" }
                , value: state.filledValue
                }
              )
            MessageFromFilled
          ]
        -- | , HH.div [ HP.class_ Css.styles.textFieldContainerHero ]
        -- |     [ TextField.outlined
        -- |         (TextField.defaultConfig
        -- |             { label = Just "Standard"
        -- |             }
        -- |         )
        -- |     ]
        ]
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
    }
    where
      handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
      handleAction =
        case _ of
             MessageFromFilled (TextField.Filled.Message__Input newValue) -> H.modify_ $ setEfficiently (Lens.prop (SProxy :: SProxy "filledValue")) newValue
