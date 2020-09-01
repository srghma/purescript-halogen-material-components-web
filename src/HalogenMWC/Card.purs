module HalogenMWC.Card where

import Prelude
import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen (ClassName, ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.IconButton as IconButton
import Material.Classes.Card (mdc_card, mdc_card____outlined, mdc_card__action, mdc_card__action____button, mdc_card__action____icon, mdc_card__action_buttons, mdc_card__action_icons, mdc_card__actions, mdc_card__actions____full_bleed, mdc_card__media, mdc_card__media____16_9, mdc_card__media____square, mdc_card__primary_action)

type Content w i
  = { blocks :: Array (HH.HTML w i)
    , actions :: Maybe (Actions w i)
    }

data Aspect
  = Square
  | SixteenToNine

type Config i
  = { outlined :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    }

type Actions w i
  = { buttons :: Array (HH.HTML w i)
    , icons :: Array (HH.HTML w i)
    , fullBleed :: Boolean
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { outlined: false
  , additionalAttributes: []
  }

card :: forall w i. Config i -> Content w i -> HH.HTML w i
card config content =
  HH.element (ElemName "mdc-card")
    ( [ HP.classes $ [ mdc_card ] <> if config.outlined then [ mdc_card____outlined ] else []
      ]
        <> config.additionalAttributes
    )
    ( Array.concat
        [ content.blocks
        , actionsElt content
        ]
    )

actionsElt :: forall w i. Content w i -> Array (HH.HTML w i)
actionsElt content = case content.actions of
  Just actions ->
    [ HH.div
        [ HP.classes
            $ Array.catMaybes
                [ Just mdc_card__actions
                , if actions.fullBleed then
                    Just mdc_card__actions____full_bleed
                  else
                    Nothing
                ]
        ]
        ( Array.concat
            [ if not (Array.null actions.buttons) then
                [ HH.div [ HP.class_ mdc_card__action_buttons ] actions.buttons
                ]
              else
                []
            , if not (Array.null actions.icons) then
                [ HH.div [ HP.class_ mdc_card__action_icons ] actions.icons
                ]
              else
                []
            ]
        )
    ]
  Nothing -> []

mediaView :: forall w i. Maybe Aspect -> Array (IProp I.HTMLdiv i) -> String -> HH.HTML w i
mediaView aspect additionalAttributes backgroundImage =
  HH.div
    ( [ HP.classes $ [ mdc_card__media ] <> aspectCs aspect
      , backgroundImageAttr backgroundImage
      ]
        <> additionalAttributes
    )
    []

backgroundImageAttr :: forall i. String -> IProp I.HTMLdiv i
backgroundImageAttr url = HP.style ("background-image: url(\"" <> url <> "\")")

aspectCs :: Maybe Aspect -> Array ClassName
aspectCs = case _ of
  Just Square -> [ mdc_card__media____square ]
  Just SixteenToNine -> [ mdc_card__media____16_9 ]
  Nothing -> []

primaryAction :: forall w i. Array (IProp I.HTMLdiv i) -> Array (HH.HTML w i) -> HH.HTML w i
primaryAction additionalAttributes =
  HH.div
    ( [ HP.class_ mdc_card__primary_action
      , HP.prop (PropName "tabIndex") 0
      ]
        <> additionalAttributes
    )

button :: forall w i. Button.Config I.HTMLbutton i -> Array (HH.HTML w i) -> HH.HTML w i
button config =
  Button.button Button.Text
    ( config
        { additionalClasses = [ mdc_card__action, mdc_card__action____button ] <> config.additionalClasses
        }
    )

iconMaterialIcons :: forall w i. IconButton.Config i -> String -> HH.HTML w i
iconMaterialIcons config =
  IconButton.iconButtonMaterialIcons
    (config { additionalClasses = [ mdc_card__action, mdc_card__action____icon ] <> config.additionalClasses })
