module Demo.Pages.DrawerPages.Permanent where

import Demo.HOC.DrawerPage as DrawerPage
import Protolude
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Drawer.Permanent as Drawer.Permanent

type State = { selectedIndex :: Int }

type Query = Const Void

type Input = Unit

type Message = Void

type ChildSlots = ()

data Action = SetSelectedIndex Int

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  SetSelectedIndex index -> H.modify_ _ { selectedIndex = index }

config :: DrawerPage.DrawerPage Query
config =
  { title: "Permanent Drawer"
  , drawer:
    H.mkComponent
      { initialState: const { selectedIndex: 0 }
      , eval: H.mkEval H.defaultEval { handleAction = handleAction }
      , render: \state -> Drawer.Permanent.drawer (DrawerPage.drawerBody SetSelectedIndex state.selectedIndex)
      }
  , scrim: Nothing
  -- | , onMenuClick: Nothing
  }
