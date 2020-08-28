module HalogenMWC.Select where

import Data.FoldableWithIndex (findMapWithIndex)
import Halogen (AttrName(..), ElemName(..), PropName(..))
import Material.Classes.Select (mdc_floating_label, mdc_line_ripple, mdc_notched_outline, mdc_notched_outline__leading, mdc_notched_outline__notch, mdc_notched_outline__trailing, mdc_select, mdc_select____outlined, mdc_select____with_leading_icon, mdc_select__anchor, mdc_select__dropdown_icon, mdc_select__icon, mdc_select__menu, mdc_select__selected_text)
import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.Icon as Icon
import HalogenMWC.List as List
import HalogenMWC.List.Item as List.Item
import HalogenMWC.Menu as Menu
import HalogenMWC.Select.Item as Select.Item

type Config a w i
  = { label :: Maybe String
    , disabled :: Boolean
    , required :: Boolean
    , valid :: Boolean
    , selected :: Maybe a
    , leadingIcon :: forall a w i. Maybe (HH.HTML w i)
    , additionalAttributes :: Array (IProp I.HTMLselect i)
    , onChange :: Maybe (a -> i)
    }

defaultConfig :: forall a w i. Config a w i
defaultConfig =
  { label: Nothing
  , disabled: false
  , required: false
  , valid: true
  , selected: Nothing
  , leadingIcon: Nothing
  , additionalAttributes: []
  , onChange: Nothing
  }

data Variant
  = Filled
  | Outlined

derive instance eqVariant :: Eq Variant

select ::
  forall a w i.
  Eq a =>
  Variant ->
  Config a w i ->
  Select.Item.SelectItem w a i ->
  Array (Select.Item.SelectItem w a i) ->
  HH.HTML w i
select variant config firstSelectItem remainingSelectItems =
  let
    selectedIndex :: Maybe Int
    selectedIndex =
      findMapWithIndex
        ( \index (Select.Item.SelectItem selectItemConfig _) ->
            if Just selectItemConfig.value == config.selected then
              Just index
            else
              Nothing
        )
        ([ firstSelectItem ] <> remainingSelectItems)
  in
    HH.element (ElemName "mdc-select")
      ( [ HP.classes
            $ Array.catMaybes
                [ Just mdc_select
                , if variant == Outlined then Just mdc_select____outlined else Nothing
                , case config.leadingIcon of
                    Just _ -> Just mdc_select____with_leading_icon
                    _ -> Nothing
                ]
        , HP.disabled config.disabled
        , HP.prop (PropName "selectedIndex") (Maybe.fromMaybe (-1) selectedIndex)
        , HP.prop (PropName "valid") config.valid
        , HP.prop (PropName "required") config.required
        ]
          <> config.additionalAttributes
      )
      [ anchorElt
          ( Array.concat
              [ [ leadingIconElt config
                , dropdownIconElt
                , selectedTextElt
                ]
              , if variant == Outlined then
                  [ notchedOutlineElt config ]
                else
                  [ floatingLabelElt config
                  , lineRippleElt
                  ]
              ]
          )
      , menuElt config.leadingIcon config.selected config.onChange firstSelectItem remainingSelectItems
      ]

icon :: forall i w. Array (IProp I.HTMLi i) -> String -> HH.HTML w i
icon additionalAttributes iconName = Icon.icon ([ HP.class_ mdc_select__icon ] <> additionalAttributes) iconName

anchorElt :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
anchorElt = HH.div [ HP.class_ mdc_select__anchor ]

leadingIconElt :: forall a w i. Config a w i -> HH.HTML w i
leadingIconElt config = case config.leadingIcon of
  Just icon_ -> icon_
  Nothing -> HH.text ""

dropdownIconElt :: forall w i. HH.HTML w i
dropdownIconElt = HH.i [ HP.class_ mdc_select__dropdown_icon ] []

floatingLabelElt :: forall a w i. Config a w i -> HH.HTML w i
floatingLabelElt config = HH.div [ HP.class_ mdc_floating_label ] [ HH.text (Maybe.fromMaybe "" config.label) ]

lineRippleElt :: forall w i. HH.HTML w i
lineRippleElt = HH.label [ HP.class_ mdc_line_ripple ] []

notchedOutlineElt :: forall a w i. Config a w i -> HH.HTML w i
notchedOutlineElt config =
  HH.div [ HP.class_ mdc_notched_outline ]
    [ HH.div [ HP.class_ mdc_notched_outline__leading ] []
    , HH.div [ HP.class_ mdc_notched_outline__notch ]
        [ HH.label
            [ HP.class_ mdc_floating_label ]
            [ HH.text (Maybe.fromMaybe "" config.label) ]
        ]
    , HH.div [ HP.class_ mdc_notched_outline__trailing ] []
    ]

menuElt :: forall a w i. Maybe (HH.HTML w i) -> Maybe a -> Maybe (a -> i) -> Select.Item.SelectItem w a i -> Array (Select.Item.SelectItem w a i) -> HH.HTML w i
menuElt leadingIcon selected onChange firstSelectItem remainingSelectItems =
  Menu.menu
    ( Menu.defaultConfig
        { additionalClasses = [ mdc_select__menu ]
        , additionalAttributes = [ HP.attr (AttrName "style") "width: 100%;" ]
        }
    )
    [ List.list (List.defaultConfig { wrapFocus = true })
        (listItem (Maybe.isJust leadingIcon) selected onChange firstSelectItem)
        (map (listItem (Maybe.isJust leadingIcon) selected onChange) remainingSelectItems)
    ]

listItem :: forall a w i. Boolean -> Maybe a -> Maybe (a -> i) -> Select.Item.SelectItem w a i -> List.Item.ListItem w i
listItem showLeadingIcon selected onChange (Select.Item.SelectItem config nodes) =
  List.Item.listItem
    (listItemConfig selected onChange config)
    (if showLeadingIcon then [ List.Item.graphic nodes ] else nodes)

listItemConfig :: forall a i w. Maybe a -> Maybe (a -> i) -> Select.Item.Config a i -> List.Item.Config w i
listItemConfig selectedValue onChange config =
  List.Item.defaultConfig
    { disabled = config.disabled
    , additionalAttributes = config.additionalAttributes
    , onClick =
      case onChange of
        Nothing -> Nothing
        Just onChange' -> Just (\_ -> onChange' config.value)
    }

selectedTextElt :: forall w i. HH.HTML w i
selectedTextElt = HH.div [ HP.class_ mdc_select__selected_text ] []
