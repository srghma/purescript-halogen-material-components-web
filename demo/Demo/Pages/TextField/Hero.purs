module Demo.Pages.TextField.Hero where

import Material.Classes.Typography (mdc_typography____subtitle1)
import Protolude

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils (mkComponentStatic)
import Halogen.HTML (IProp)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.HelperText as HelperText
import HalogenMWC.TextArea as TextArea
import HalogenMWC.TextField.Filled as TextField.Filled
import Demo.MkComponent.WithFocus as WithFocus
import Demo.Pages.TextField.Css as Css

type Query = Const Void

type Input = Unit

type Message = Void

component :: forall query input output action slots m. H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: const unit
    , render: \state ->
        HH.div
        [ HP.class_ Css.styles.textFieldContainerHeroRoot
        ]
        [ HH.div
          [ HP.class_ Css.styles.textFieldContainerHero ]
          [ HH.slot_
            (SProxy :: SProxy "filled")
            unit
            TextField.Filled.filled
              ( TextField.Filled.defaultConfig
                { label: TextField.Filled.LabelConfig__With { id: "hero-filled-label-global-id", labelText: "Standard" }
                , value: "asdf"
                }
              )
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
    }
