module Demo.Pages.Checkbox.Shared where

import Data.Map (Map)
import Data.Map as Map
import Demo.Utils
import Halogen
import Material.Classes.Typography
import Protolude
import DOM.HTML.Indexed (HTMLinput) as I
import Data.Array as Array
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.Checkbox as Checkbox

data Action state
  = Changed (Lens' state (Maybe Checkbox.State))
  | Focus String

type Query
  = Const Void

type Message
  = Void

type ChildSlots
  = ()

handleAction :: forall state. Action state -> H.HalogenM state (Action state) ChildSlots Message Aff Unit
handleAction = case _ of
  Changed lens -> H.modify_ (Lens.over lens changed)
  Focus id -> H.liftEffect $ focusById id
  where
  changed = case _ of
    Just Checkbox.Checked -> Just Checkbox.Unchecked
    _ -> Just Checkbox.Checked

checkbox ::
  forall state m i.
  (Lens' state (Maybe Checkbox.State)) ->
  state ->
  Array (IProp I.HTMLinput (Action state)) ->
  HH.ComponentHTML (Action state) ChildSlots m
checkbox lens state additionalAttributes =
  Checkbox.checkbox
    ( Checkbox.defaultConfig
        { state = Lens.view lens state
        , onChange = Just $ const $ Changed lens
        , additionalAttributes = additionalAttributes
        }
    )
