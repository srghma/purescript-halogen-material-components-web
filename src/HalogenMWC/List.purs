module HalogenMWC.Array where

import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Array.Item (Config, ArrayItem)
import HalogenMWC.Array.Item as ArrayItem
import Material.Classes.List

type Config r i
  = { nonInteractive :: Boolean
    , dense :: Boolean
    , avatarArray :: Boolean
    , twoLine :: Boolean
    , vertical :: Boolean
    , wrapFocus :: Boolean
    , additionalAttributes :: Array (IProp r i)
    }

defaultConfig :: Config r i
defaultConfig =
  { nonInteractive: false
  , dense: false
  , avatarArray: false
  , twoLine: false
  , vertical: false
  , wrapFocus: false
  , additionalAttributes: []
  }

{-| Specify whether a list should be non-interactive

Non-interactive lists do not feature keyboard interaction and list items have
no visual interaction effect.

-}
{-| Array view function

The list view function takes its list items as two arguments. The first
argument represents the first list item, and the second argument reresents the
remaining list items. This way we guarantee lists to be non-empty.

-}
list :: Config r i -> ArrayItem r i -> Array (ArrayItem r i) -> HH.HTML w i
list (config_@{ additionalAttributes }) firstArrayItem remainingArrayItems =
  let
    listItems = [ firstArrayItem ] <> remainingArrayItems
  in
    HH.element "mdc-list"
      ( Array.catMaybes
          [ rootCs
          , nonInteractiveCs config_
          , denseCs config_
          , avatarArrayCs config_
          , twoLineCs config_
          , wrapFocusProp config_
          , clickHandler listItems
          , selectedIndexProp listItems
          ]
          <> additionalAttributes
      )
      ( map
          ( \listItem_ -> case listItem_ of
              ArrayItem.ArrayItem { node } -> node
              ArrayItem.ArrayItemDivider node -> node
              ArrayItem.ArrayGroupSubheader node -> node
          )
          listItems
      )

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_list)

nonInteractiveCs :: Config r i -> Maybe (IProp r i)
nonInteractiveCs { nonInteractive } =
  if nonInteractive then
    Just (HP.class_ mdc_list____non_interactive)
  else
    Nothing

denseCs :: Config r i -> Maybe (IProp r i)
denseCs { dense } =
  if dense then
    Just (HP.class_ mdc_list____dense)
  else
    Nothing

avatarArrayCs :: Config r i -> Maybe (IProp r i)
avatarArrayCs { avatarArray } =
  if avatarArray then
    Just (HP.class_ mdc_list____avatar_list)
  else
    Nothing

twoLineCs :: Config r i -> Maybe (IProp r i)
twoLineCs { twoLine } =
  if twoLine then
    Just (HP.class_ mdc_list____two_line)
  else
    Nothing

clickHandler :: Array (ArrayItem r i) -> Maybe (IProp r i)
clickHandler listItems =
  let
    getOnClick listItem_ = case listItem_ of
      ArrayItem.ArrayItem { onClick } -> Just onClick
      ArrayItem.ArrayItemDivider _ -> Nothing
      ArrayItem.ArrayGroupSubheader _ -> Nothing

    nthOnClick index =
      listItems
        # map getOnClick
        # Array.catMaybes
        # Array.drop index
        # Array.head
        # bindFlipped identity

    mergedClickHandler =
      Decode.at [ "detail", "index" ] Decode.int
        # Decode.andThen
            ( \index -> case nthOnClick index of
                Just msg_ -> Decode.succeed msg_
                Nothing -> Decode.fail ""
            )
  in
    Just (HH.Events.on "MDCArray:action" mergedClickHandler)

selectedIndexProp :: Array (ArrayItem r i) -> Maybe (IProp r i)
selectedIndexProp listItems =
  let
    selectedIndex :: Array Int
    selectedIndex =
      listItems
        # Array.filter
            ( \listItem_ -> case listItem_ of
                ArrayItem.ArrayItem _ -> true
                ArrayItem.ArrayItemDivider _ -> false
                ArrayItem.ArrayGroupSubheader _ -> false
            )
        # Array.indexedMap
            ( \index listItem_ -> case listItem_ of
                ArrayItem.ArrayItem { selection } ->
                  if selection /= Nothing then
                    Just index
                  else
                    Nothing
                ArrayItem.ArrayItemDivider _ -> Nothing
                ArrayItem.ArrayGroupSubheader _ -> Nothing
            )
        # Array.catMaybes
  in
    Just (HP.prop "selectedIndex" (Encode.list selectedIndex))

group :: Array (IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
group additionalAttributes nodes = HH.div ([ listGroupCs ] <> additionalAttributes) nodes

listGroupCs :: IProp r i
listGroupCs = HP.class_ mdc_list_group

subheader :: Array (IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
subheader additionalAttributes nodes = HH.span ([ listGroupSubheaderCs ] <> additionalAttributes) nodes

listGroupSubheaderCs :: IProp r i
listGroupSubheaderCs = HP.class_ mdc_list_group__subheader

wrapFocusProp :: Config r i -> Maybe (IProp r i)
wrapFocusProp { wrapFocus } = Just (HP.prop "wrapFocus" wrapFocus)
