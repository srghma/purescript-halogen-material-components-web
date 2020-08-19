module HalogenMWC.Card where

import HalogenMWC.Button
import HalogenMWC.IconButton
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.IconButton as IconButton
import Material.Classes.Card

type Config r i
  = { outlined :: Boolean
    , additionalAttributes :: Array (IProp r i)
    }

defaultConfig :: Config r i
defaultConfig =
  { outlined: false
  , additionalAttributes: []
  }

card :: Config r i -> Content r i -> HH.HTML w i
card (config_@{ additionalAttributes }) content =
  HH.element "mdc-card"
    ( Array.catMaybes
        [ HP.class_ mdc_card
        , outlinedCs config_
        ]
        <> additionalAttributes
    )
    ( Array.concat
        [ blocksElt content
        , actionsElt content
        ]
    )

blocksElt :: Content r i -> Array (HH.HTML w i)
blocksElt { blocks } = map (\(Block html) -> html) blocks

actionsElt :: Content r i -> Array (HH.HTML w i)
actionsElt content = case content.actions of
  Just (Actions { buttons, icons, fullBleed }) ->
    [ HH.div
        ( Array.catMaybes
            [ Just (HP.class_ mdc_card__actions)
            , if fullBleed then
                Just (HP.class_ mdc_card__actions____full_bleed)
              else
                Nothing
            ]
        )
        ( Array.concat
            [ if not (Array.isEmpty buttons) then
                [ HH.div [ HP.class_ mdc_card__action_buttons ]
                    (map (\(Button button_) -> button_) buttons)
                ]
              else
                []
            , if not (Array.isEmpty icons) then
                [ HH.div [ HP.class_ mdc_card__action_icons ]
                    (map (\(Icon icon_) -> icon_) icons)
                ]
              else
                []
            ]
        )
    ]
  Nothing -> []

outlinedCs :: Config r i -> Maybe (IProp r i)
outlinedCs config =
  if config.outlined then
    Just (HP.class_ mdc_card____outlined)
  else
    Nothing

type Content r i
  = { blocks :: Array (Block r i)
    , actions :: Maybe (Actions r i)
    }

data Aspect
  = Square
  | SixteenToNine

mediaView :: Maybe Aspect -> Array (IProp r i) -> String -> HH.HTML w i
mediaView aspect additionalAttributes backgroundImage =
  HH.div
    ( Array.catMaybes
        [ mediaCs
        , backgroundImageAttr backgroundImage
        , aspectCs aspect
        ]
        <> additionalAttributes
    )
    []

squareMedia :: Array (IProp r i) -> String -> HH.HTML w i
squareMedia additionalAttributes backgroundImage = mediaView (Just Square) additionalAttributes backgroundImage

sixteenToNineMedia :: Array (IProp r i) -> String -> HH.HTML w i
sixteenToNineMedia additionalAttributes backgroundImage = mediaView (Just SixteenToNine) additionalAttributes backgroundImage

media :: Array (IProp r i) -> String -> HH.HTML w i
media additionalAttributes backgroundImage = mediaView Nothing additionalAttributes backgroundImage

mediaCs :: Maybe (IProp r i)
mediaCs = Just (HP.class_ mdc_card__media)

backgroundImageAttr :: String -> Maybe (IProp r i)
backgroundImageAttr url = Just (style "background-image" ("url(\"" <> url <> "\")"))

aspectCs :: Maybe Aspect -> Maybe (IProp r i)
aspectCs aspect = case aspect of
  Just Square -> Just (HP.class_ mdc_card__media____square)
  Just SixteenToNine -> Just (HP.class_ mdc_card__media____16_9)
  Nothing -> Nothing

primaryAction :: Array (IProp r i) -> Array (HH.HTML w i) -> Array (HH.HTML w i)
primaryAction additionalAttributes blocks =
  [ HH.div
      ( [ HP.class_ mdc_card__primary_action
        , tabIndexProp 0
        ]
          <> additionalAttributes
      )
      blocks
  ]

tabIndexProp :: Int -> IProp r i
tabIndexProp tabIndex = HP.prop "tabIndex" tabIndex

newtype Actions r i
  = Actions
  { buttons :: Array (Button r i)
  , icons :: Array (Icon r i)
  , fullBleed :: Boolean
  }

actions :: { buttons :: Array (Button r i), icons :: Array (Icon r i) } -> Actions r i
actions { buttons, icons } = Actions { buttons = buttons, icons = icons, fullBleed = false }

fullBleedActions :: Button r i -> Actions r i
fullBleedActions button_ = Actions { buttons = [ button_ ], icons = [], fullBleed = true }

button :: Button.Config r i -> String -> HH.HTML w i
button config label =
  Button.text
    (config { additionalAttributes = HP.classes [ mdc_card__action, mdc_card__action____button ] <> config.additionalAttributes })
    label

icon :: IconButton.Config r i -> String -> HH.HTML w i
icon config iconName =
  IconButton.iconButton
    (config { additionalAttributes = HP.classes [ mdc_card__action, mdc_card__action____icon ] <> config.additionalAttributes })
    iconName
