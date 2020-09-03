module Demo.Pages.IconButton where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.IconButton as IconButton
import HalogenMWC.IconToggle as IconToggle
import Material.Classes.Typography
import Data.Set (Set)
import Data.Set as Set
import Demo.Utils

type State = { ons :: Set String }

type ChildSlots = ()

type Message = Void

initialState :: forall r w i . State
initialState = { ons: Set.empty }

data Action
  = Toggle String
  | Focus String

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
      Toggle id -> H.modify_ \state -> state
        { ons = if Set.member id state.ons then Set.delete id state.ons else Set.insert id state.ons
        }
      Focus id -> H.liftEffect $ focusById id

config :: CatalogPage
config =
    { title: "Icon Button"
    , prelude: "Icons are appropriate for buttons that allow a user to take actions or make a selected, such as adding or removing a star to an item."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/design/components/buttons.html#toggle-button"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-IconButton"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-icon-button"
        }
    , hero:
          H.mkComponent
            { initialState: const initialState
            , render: \state ->
                HH.div_
                [ IconToggle.iconToggleMaterialIcons
                    (IconToggle.defaultConfig
                        { on = Set.member "icon-button-hero" state.ons
                        , onChange = Just $ const $ Toggle "icon-button-hero"
                        }
                    )
                    { offIcon: "favorite_border"
                    , onIcon: "favorite"
                    }
                ]
            , eval: H.mkEval H.defaultEval { handleAction = handleAction }
            }
    , content:
        H.mkComponent
          { initialState: const initialState
          , render: \state ->
              HH.div_
                [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Icon Button" ]
                , IconButton.iconButtonMaterialIcons IconButton.defaultConfig "wifi"
                , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Icon Toggle" ]
                , IconToggle.iconToggleMaterialIcons
                    (IconToggle.defaultConfig
                        { on = Set.member "icon-button-toggle" state.ons
                        , onChange = Just $ const $ Toggle "icon-button-toggle"
                        }
                    )
                    { offIcon: "favorite_border"
                    , onIcon: "favorite"
                    }
                , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Icon Button" ]
                , HH.div_
                    [ IconButton.iconButtonMaterialIcons
                        (IconButton.defaultConfig
                            { additionalAttributes = [ HP.id_ "my-icon-button" ]
                            }
                        )
                        "wifi"
                    , HH.text "\x00A0"
                    , Button.button Button.Raised
                        (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-icon-button") ] })
                        [ HH.text "Focus" ]
                    ]
                , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Icon Toggle" ]
                , HH.div_
                    [ IconToggle.iconToggleMaterialIcons
                        (IconToggle.defaultConfig
                            { on = Set.member "icon-button-toggle" state.ons
                            , onChange = Just $ const $ Toggle "icon-button-toggle"
                            , additionalAttributes = [ HP.id_ "my-icon-toggle" ]
                            }
                        )
                        { offIcon: "favorite_border"
                        , onIcon: "favorite"
                        }
                    , HH.text "\x00A0"
                    , Button.button Button.Raised
                        (Button.defaultConfig { additionalAttributes = [ HE.onClick $ const $ Focus "my-icon-toggle" ] })
                        [ HH.text "Focus" ]
                    ]
                ]
          , eval: H.mkEval H.defaultEval { handleAction = handleAction }
          }
    }
