module HalogenMWC.Card where

import HalogenMWC.Button
import HalogenMWC.IconButton
import Protolude
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes
import Web.Event.Event

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.IconButton as IconButton
import Material.Classes.Card

type Content w i =
  { blocks :: Array (HH.HTML w i)
  , actions :: Maybe (Actions w i)
  }

data Aspect
  = Square
  | SixteenToNine

type Config i =
  { outlined :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  }

type Actions w i =
  { buttons   :: Array (HH.HTML w i)
  , icons     :: Array (HH.HTML w i)
  , fullBleed :: Boolean
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { outlined: false
  , additionalAttributes: []
  }

card :: forall w i . Config i -> Content w i -> HH.HTML w i
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

actionsElt :: forall w i . Content w i -> Array (HH.HTML w i)
actionsElt content = case content.actions of
  Just actions ->
    [ HH.div
        [ HP.classes $ Array.catMaybes
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

mediaView :: forall w i . Maybe Aspect -> Array (IProp I.HTMLdiv i) -> String -> HH.HTML w i
mediaView aspect additionalAttributes backgroundImage =
  HH.div
    ( [ HP.classes $ [ mdc_card__media ] <> aspectCs aspect
      , backgroundImageAttr backgroundImage
      ]
      <> additionalAttributes
    )
    []

backgroundImageAttr :: forall r i . String -> IProp r i
backgroundImageAttr url = HP.attr (AttrName "style") ("background-image: url(\"" <> url <> "\")")

aspectCs :: Maybe Aspect -> Array ClassName
aspectCs = case _ of
  Just Square -> [ mdc_card__media____square ]
  Just SixteenToNine -> [ mdc_card__media____16_9 ]
  Nothing -> []

primaryAction :: forall w i . Array (IProp I.HTMLdiv i) -> Array (HH.HTML w i) -> HH.HTML w i
primaryAction additionalAttributes =
  HH.div
    ( [ HP.class_ mdc_card__primary_action
      , HP.prop (PropName "tabIndex") 0
      ]
      <> additionalAttributes
    )

button :: forall w i . Button.Config I.HTMLbutton i -> Array (HH.HTML w i) -> HH.HTML w i
button config =
  Button.button
    (config
      { variant = Button.Text
      , additionalClasses = [ mdc_card__action, mdc_card__action____button ] <> config.additionalClasses
      }
    )

icon :: forall w i . IconButton.Config i -> String -> HH.HTML w i
icon config =
  IconButton.iconButton
    (config
      { additionalClasses = [ mdc_card__action, mdc_card__action____icon ] <> config.additionalClasses
      }
    )
