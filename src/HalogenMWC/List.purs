module HalogenMWC.List where

import Halogen (ElemName(..), PropName(..))
import Material.Classes.List (mdc_list, mdc_list____avatar_list, mdc_list____dense, mdc_list____two_line, mdc_list_group, mdc_list_group__subheader)
import Prelude
import Data.Maybe (Maybe(..))
import Web.Event.Event (Event, EventType(..))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Either (hush) as Either
import Foreign (readInt, unsafeToForeign) as Foreign
import Foreign.Index (readProp) as Foreign
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.List.Item as ListItem
import HalogenMWC.Utils as Utils
import Control.Monad.Except (runExcept)

type Config i
  = { dense :: Boolean
    , avatarList :: Boolean
    , twoLine :: Boolean
    , vertical :: Boolean
    , wrapFocus :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { dense: false
  , avatarList: false
  , twoLine: false
  , vertical: false
  , wrapFocus: false
  , additionalAttributes: []
  }

list :: forall w i. Config i -> ListItem.ListItem w i -> Array (ListItem.ListItem w i) -> HH.HTML w i
list config firstListItem remainingListItems =
  let
    listItems = [ firstListItem ] <> remainingListItems
  in
    HH.element (ElemName "mdc-list")
      ( [ HP.classes
            $ Array.catMaybes
                [ Just mdc_list
                , if config.dense then Just mdc_list____dense else Nothing
                , if config.avatarList then Just mdc_list____avatar_list else Nothing
                , if config.twoLine then Just mdc_list____two_line else Nothing
                ]
        , HP.prop (PropName "wrapFocus") config.wrapFocus
        , clickHandler listItems
        , selectedIndexProp listItems
        ]
          <> config.additionalAttributes
      )
      ( map
          ( \listItem_ -> case listItem_ of
              ListItem.ListItem { node } -> node
              ListItem.ListItemDivider node -> node
              ListItem.ArrayGroupSubheader node -> node
          )
          listItems
      )

readDetailIndex :: Event -> Maybe Int
readDetailIndex event = Foreign.unsafeToForeign event # (\f -> Foreign.readProp "detail" f >>= Foreign.readProp "index" >>= Foreign.readInt) # runExcept # Either.hush

clickHandler :: forall w i r. Array (ListItem.ListItem w i) -> IProp r i
clickHandler listItems =
  let
    getListItemConfigs :: ListItem.ListItem w i -> Maybe (ListItem.Config w i)
    getListItemConfigs = case _ of
      ListItem.ListItem config -> Just config
      _ -> Nothing

    nthOnClick :: Int -> Maybe (Maybe (Event -> i))
    nthOnClick index =
      listItems
        # map getListItemConfigs
        # Array.catMaybes
        # (\listItems' -> Array.index listItems' index)
        # map (_.onClick)

    mergedClickHandler :: Event -> Maybe i
    mergedClickHandler event = case readDetailIndex event >>= nthOnClick >>> join of
      Nothing -> Nothing
      Just onClick -> Just (onClick event)
  in
    HE.handler' (EventType "MDCArray:action") mergedClickHandler

selectedIndexProp :: forall w i r. Array (ListItem.ListItem w i) -> IProp r i
selectedIndexProp listItems =
  let
    selectedIndex :: Array Int
    selectedIndex =
      listItems
        # Array.filter
            ( \listItem_ -> case listItem_ of
                ListItem.ListItem _ -> true
                ListItem.ListItemDivider _ -> false
                ListItem.ArrayGroupSubheader _ -> false
            )
        # Array.mapWithIndex
            ( \index listItem_ -> case listItem_ of
                ListItem.ListItem { selected } ->
                  if selected /= Nothing then
                    Just index
                  else
                    Nothing
                _ -> Nothing
            )
        # Array.catMaybes
  in
    Utils.prop (PropName "selectedIndex") (Utils.propFromArrayInt selectedIndex)

group :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
group = HH.div [ HP.class_ mdc_list_group ]

subheader :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
subheader = HH.span [ HP.class_ mdc_list_group__subheader ]
