module Demo.Pages.Checkbox.Shared where

import Demo.Utils (focusById)
import Protolude (Aff, Const, Maybe(..), Unit, Void, const, ($))
import DOM.HTML.Indexed (HTMLinput) as I
import Data.Lens (Lens')
import Data.Lens as Lens
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
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
