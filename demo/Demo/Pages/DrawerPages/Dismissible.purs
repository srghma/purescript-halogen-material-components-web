module Demo.Pages.DrawerPages.Dismissible where

import Demo.HOC.DrawerPage as DrawerPage
import HalogenMWC.Drawer.Dismissible as Drawer.Dismissible
import Web.UIEvent.MouseEvent (MouseEvent)

import Protolude
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { open :: Boolean
  , selectedIndex :: Int
  }

data Action
  = CloseDrawer
  | SetSelectedIndex Int

data Query a =
  ToggleDrawer MouseEvent a

type Input = Unit

type Message = Void

type ChildSlots = ()

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
    CloseDrawer -> H.modify_ _ { open = false }
    SetSelectedIndex index -> H.modify_ _ { selectedIndex = index }

handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleQuery = case _ of
  ToggleDrawer _mouseEvent next -> do
    H.modify_ \state -> state { open = not state.open }
    pure (Just next)

config :: DrawerPage.DrawerPage Query
config =
  { title: "Dismissible Drawer"
  , drawer:
      H.mkComponent
        { initialState: const
          { open: false
          , selectedIndex: 0
          }
        , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
        , render: \state ->
            Drawer.Dismissible.drawer
              (Drawer.Dismissible.defaultConfig
                { open = state.open
                , onClose = Just $ const $ CloseDrawer
                }
              )
              (DrawerPage.drawerBody SetSelectedIndex state.selectedIndex)
        }
  , onMenuClick: Nothing
  -- | , onMenuClick: Just ToggleDrawer
  , scrim: Nothing
  }
