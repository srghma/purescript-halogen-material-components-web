module Demo.Pages.DrawerPages.Modal where

import Demo.HOC.DrawerPage as DrawerPage
import HalogenMWC.Drawer.Modal as Drawer.Modal
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
  OpenDrawer a

type Input = Unit

type Message = Void

type ChildSlots = ()

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
    CloseDrawer -> H.modify_ (_ { open = false })
    SetSelectedIndex index -> H.modify_ (_ { selectedIndex = index })

handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleQuery = case _ of
  OpenDrawer next -> do
    H.modify_ (_ { open = true })
    pure (Just next)

config :: DrawerPage.DrawerPage Query
config =
  { title: "Modal Drawer"
  , drawer:
      H.mkComponent
        { initialState: const
          { open: false
          , selectedIndex: 0
          }
        , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
        , render: \state ->
            Drawer.Modal.drawer
              (Drawer.Modal.defaultConfig
                { open = state.open
                , onClose = Just $ const $ CloseDrawer
                }
              )
              (DrawerPage.drawerBody SetSelectedIndex state.selectedIndex)
        }
  , scrim: Just (Drawer.Modal.scrim [])
  , onMenuClick: Just OpenDrawer
  }
