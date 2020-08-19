module HalogenMWC.Button where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { icon :: Maybe String
    , trailingIcon :: Boolean
    , disabled :: Boolean
    , dense :: Boolean
    , href :: Maybe String
    , target :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    , touch :: Boolean
    }

defaultConfig :: Config r i
defaultConfig =
  { icon: Nothing
  , trailingIcon: false
  , disabled: false
  , dense: false
  , href: Nothing
  , target: Nothing
  , additionalAttributes: []
  , onClick: Nothing
  , touch: true
  }

data Variant
  = Text
  | Raised
  | Unelevated
  | Outlined

button :: Variant -> Config r i -> String -> HH.HTML w i
button variant (config_@{ additionalAttributes, touch, href }) label =
  let
    wrapTouch node =
      if touch then
        HH.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]
      else
        node
  in
    wrapTouch
      $ HH.element "mdc-button"
          (Array.catMaybes [ disabledProp config_ ])
          [ ( if href /= Nothing then
                HH.a
              else
                HH.button
            )
              ( Array.catMaybes
                  [ rootCs
                  , variantCs variant
                  , denseCs config_
                  , touchCs config_
                  , disabledAttr config_
                  , tabIndexProp config_
                  , hrefAttr config_
                  , targetAttr config_
                  , clickHandler config_
                  ]
                  <> additionalAttributes
              )
              ( Array.catMaybes
                  [ rippleElt
                  , leadingIconElt config_
                  , labelElt label
                  , trailingIconElt config_
                  , touchElt config_
                  ]
              )
          ]

{-| Text button variant (flush without outline)
-}
text :: Config r i -> String -> HH.HTML w i
text config_ label = button Text config_ label

{-| Outlined button variant (flush with outline)
-}
outlined :: Config r i -> String -> HH.HTML w i
outlined config_ label = button Outlined config_ label

{-| Raised button variant (contained with elevation)
-}
raised :: Config r i -> String -> HH.HTML w i
raised config_ label = button Raised config_ label

{-| Unelevated button variant (contained without elevation)
-}
unelevated :: Config r i -> String -> HH.HTML w i
unelevated config_ label = button Unelevated config_ label

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_button)

disabledProp :: Config r i -> Maybe (IProp r i)
disabledProp { disabled } = Just (HP.prop "disabled" disabled)

disabledAttr :: Config r i -> Maybe (IProp r i)
disabledAttr { disabled } = Just (HH.Attributes.disabled disabled)

tabIndexProp :: Config r i -> Maybe (IProp r i)
tabIndexProp { disabled } =
  if disabled then
    Just (HP.prop "tabIndex" -1)
  else
    Just (HP.prop "tabIndex" 0)

hrefAttr :: Config r i -> Maybe (IProp r i)
hrefAttr { href } = map HH.Attributes.href href

targetAttr :: Config r i -> Maybe (IProp r i)
targetAttr { href, target } =
  if href /= Nothing then
    map HH.Attributes.target target
  else
    Nothing

clickHandler :: Config r i -> Maybe (IProp r i)
clickHandler { onClick } = map HH.Events.onClick onClick

variantCs :: Variant -> Maybe (IProp r i)
variantCs variant = case variant of
  Text -> Nothing
  Raised -> Just (HP.class_ mdc_button____raised)
  Unelevated -> Just (HP.class_ mdc_button____unelevated)
  Outlined -> Just (HP.class_ mdc_button____outlined)

denseCs :: Config r i -> Maybe (IProp r i)
denseCs { dense } =
  if dense then
    Just (HP.class_ mdc_button____dense)
  else
    Nothing

touchCs :: Config r i -> Maybe (IProp r i)
touchCs { touch } =
  if touch then
    Just (HP.class_ mdc_button____touch)
  else
    Nothing

iconElt :: Config r i -> Maybe (HH.HTML w i)
iconElt { icon } =
  map
    ( \iconName ->
        HH.i
          [ HP.class_ "mdc-button__icon material-icons"
          , HH.Attributes.attribute "aria-hidden" "true"
          ]
          [ HH.text iconName ]
    )
    icon

rippleElt :: Maybe (HH.HTML w i)
rippleElt = Just (HH.div [ HP.class_ mdc_button__ripple ] [])

leadingIconElt :: Config r i -> Maybe (HH.HTML w i)
leadingIconElt (config_@{ trailingIcon }) =
  if not trailingIcon then
    iconElt config_
  else
    Nothing

trailingIconElt :: Config r i -> Maybe (HH.HTML w i)
trailingIconElt (config_@{ trailingIcon }) =
  if trailingIcon then
    iconElt config_
  else
    Nothing

touchElt :: Config r i -> Maybe (HH.HTML w i)
touchElt { touch } =
  if touch then
    Just (HH.div [ HP.class_ mdc_button__touch ] [])
  else
    Nothing

labelElt :: String -> Maybe (HH.HTML w i)
labelElt label = Just (HH.span [ HP.class_ mdc_button__label ] [ HH.text label ])
