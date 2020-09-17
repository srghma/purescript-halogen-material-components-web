module Demo.Pages.Select.Shared where

import Protolude
import Halogen.HTML as HH
import HalogenMWC.Select.Item as Select.Item

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
