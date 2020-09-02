module Demo.Pages.Selects.Shared where

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
import HalogenMWC.Select as Select
import HalogenMWC.Select.Item as Select.Item
import Material.Classes.Typography
import Demo.Utils

data Fruit
  = Apple
  | Orange
  | Banana

derive instance eqFruit :: Eq Fruit

type ChildSlots = ()

type Message = Void

type Query = Const Void

type Input = Unit

firstItem :: forall a w i . Select.Item.SelectItem (Maybe a) w i
firstItem =
    Select.Item.SelectItem
        (Select.Item.defaultConfig Nothing)
        []

remainingItems :: forall w i . Array (Select.Item.SelectItem (Maybe Fruit) w i)
remainingItems =
    [ Select.Item.SelectItem
        (Select.Item.defaultConfig (Just Apple))
        [ HH.text "Apple" ]
    , Select.Item.SelectItem
        (Select.Item.defaultConfig (Just Orange))
        [ HH.text "Orange" ]
    , Select.Item.SelectItem
        (Select.Item.defaultConfig (Just Banana))
        [ HH.text "Banana" ]
    ]
