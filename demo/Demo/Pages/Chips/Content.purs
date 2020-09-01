module Demo.Pages.Chips.Content where

import Data.Set (Set)
import Data.Set as Set
import Demo.HOC.CatalogPage (ChildSlots, Input, Message, Query)
import Material.Classes.Typography (mdc_typography____body2, mdc_typography____subtitle1)
import Protolude (class Eq, Aff, Maybe(..), Tuple(..), Unit, bind, const, identity, map, pure, unit, unless, when, ($), (-), (/=), (/\))
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.Chip.Action as Chip.Action
import HalogenMWC.Chip.Choice as Chip.Choice
import HalogenMWC.Chip.Filter as Chip.Filter
import HalogenMWC.Chip.Input as Chip.Input
import HalogenMWC.ChipSet.Action as ChipSet.Action
import HalogenMWC.ChipSet.Choice as ChipSet.Choice
import HalogenMWC.ChipSet.Filter as ChipSet.Filter
import HalogenMWC.ChipSet.Input as ChipSet.Input
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as Web.UIEvent.KeyboardEvent
import Data.String as String
import Demo.Utils (focusById)

type State
  = { size :: Size
    , inputChips :: Array String
    , input :: String
    , accessories :: Set String
    , contacts :: Set String
    , focus :: String
    }

initialState :: forall r w i. State
initialState =
  { size: Small
  , inputChips: [ "Portland", "Biking" ]
  , input: ""
  , accessories: Set.singleton "Tops"
  , contacts: Set.singleton "Alice"
  , focus: "One"
  }

data Size
  = ExtraSmall
  | Small
  | Medium
  | Large
  | ExtraLarge

derive instance eqSize :: Eq Size

data Action
  = SizeChanged Size
  | AccessoriesChanged String
  | ContactChanged String
  | InputChanged String
  | ChipInputDeleted String
  | KeyPressed KeyboardEvent
  | FocusChanged String
  | Focus String

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  SizeChanged size -> H.modify_ (_ { size = size })
  InputChanged input -> H.modify_ (_ { input = input })
  AccessoriesChanged accessory ->
    H.modify_
      ( \state ->
          state
            { accessories =
              ( if Set.member accessory state.accessories then
                  Set.delete accessory
                else
                  Set.insert accessory
              )
                state.accessories
            }
      )
  ContactChanged contact ->
    H.modify_
      ( \state ->
          state
            { contacts =
              ( if Set.member contact state.contacts then
                  Set.delete contact
                else
                  Set.insert contact
              )
                state.contacts
            }
      )
  ChipInputDeleted inputChip ->
    H.modify_
      ( \state ->
          state
            { inputChips = Array.filter ((/=) inputChip) state.inputChips
            }
      )
  KeyPressed keyboardEvent -> case Web.UIEvent.KeyboardEvent.key keyboardEvent of
    "Backspace" -> do
      state <- H.get
      when (String.null state.input) do
        H.modify_ \state ->
          state
            { inputChips = Array.take (Array.length state.inputChips - 1) state.inputChips
            }
    "Enter" -> do
      state <- H.get
      let
        trimmedInput = String.trim state.input
      unless (String.null trimmedInput) do
        H.put
          ( state
              { input = ""
              , inputChips =
                if Array.elem trimmedInput state.inputChips then
                  state.inputChips
                else
                  Array.snoc state.inputChips trimmedInput
              }
          )
    _ -> pure unit
  FocusChanged focus -> H.modify_ (_ { focus = focus })
  Focus id -> H.liftEffect $ focusById id

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.h2 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Choice Chips" ]
    , choiceChips state
    , HH.h2 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Filter Chips" ]
    , HH.h3 [ HP.class_ mdc_typography____body2 ] [ HH.text "No leading icon" ]
    , filterChips1 state
    , HH.h3 [ HP.class_ mdc_typography____body2 ] [ HH.text "With leading icon" ]
    , filterChips2 state
    , HH.h2 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Action Chips" ]
    , actionChips state
    , HH.h2 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Shaped Chips" ]
    , shapedChips
    , HH.h2 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Input Chips" ]
    , inputChips state
    , HH.h2 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Chips" ]
    , focusChips state
    ]

choiceChips :: forall w. State -> HH.HTML w Action
choiceChips state =
  let
    toLabel = case _ of
      ExtraSmall -> "Extra Small"
      Small -> "Small"
      Medium -> "Medium"
      Large -> "Large"
      ExtraLarge -> "Extra Large"
  in
    ChipSet.Choice.chipSet
      ( (ChipSet.Choice.defaultConfig { toLabel })
          { selected = Just state.size
          , onChange = Just SizeChanged
          }
      )
      [ Chip.Choice.Chip Chip.Choice.defaultConfig ExtraSmall
      , Chip.Choice.Chip Chip.Choice.defaultConfig Small
      , Chip.Choice.Chip Chip.Choice.defaultConfig Medium
      , Chip.Choice.Chip Chip.Choice.defaultConfig Large
      , Chip.Choice.Chip Chip.Choice.defaultConfig ExtraLarge
      ]

inputChips :: forall w. State -> HH.HTML w Action
inputChips state =
  HH.div
    [ HP.style "position: relative; display: flex;"
    ]
    [ ChipSet.Input.chipSet []
        ( map
            ( \label ->
                Tuple
                  label
                  ( Chip.Input.Chip
                      ( Chip.Input.defaultConfig
                          { onDelete = Just (const $ ChipInputDeleted label)
                          }
                      )
                      label
                  )
            )
            state.inputChips
        )
    , HH.input
        [ HP.value state.input
        , HE.onValueInput InputChanged
        , HE.onKeyDown KeyPressed
        ]
    ]

filterChips1 :: forall w. State -> HH.HTML w Action
filterChips1 state =
  let
    chip accessory =
      Chip.Filter.Chip
        ( Chip.Filter.defaultConfig
            { selected = Set.member accessory state.accessories
            , onChange = Just $ const $ AccessoriesChanged accessory
            }
        )
        accessory
  in
    ChipSet.Filter.chipSet []
      ( map chip
          [ "Tops"
          , "Bottoms"
          , "Shoes"
          , "Accessories"
          ]
      )

filterChips2 :: forall w. State -> HH.HTML w Action
filterChips2 state =
  let
    chip label =
      Chip.Filter.Chip
        ( Chip.Filter.defaultConfig
            { selected = Set.member label state.contacts
            , icon = Just "face"
            , onChange = Just $ const $ ContactChanged label
            }
        )
        label
  in
    ChipSet.Filter.chipSet []
      ( map chip
          [ "Alice"
          , "Bob"
          , "Charlie"
          , "Danielle"
          ]
      )

actionChips :: forall w. State -> HH.HTML w Action
actionChips state =
  let
    chip (icon /\ label) =
      Chip.Action.Chip
        ( Chip.Action.defaultConfig
            { icon = Just icon
            }
        )
        label
  in
    ChipSet.Action.chipSet []
      ( map chip
          [ ("event" /\ "Add to calendar")
          , ("bookmark" /\ "Bookmark")
          , ("alarm" /\ "Set alarm")
          , ("directions" /\ "Get directions")
          ]
      )

shapedChips :: forall w i. HH.HTML w i
shapedChips =
  let
    chip =
      Chip.Action.Chip
        ( Chip.Action.defaultConfig
            { additionalAttributes = [ HP.style "border-radius: 4px;" ]
            }
        )
  in
    ChipSet.Action.chipSet []
      ( map chip
          [ "Bookcase"
          , "TV Stand"
          , "Sofas"
          , "Office chairs"
          ]
      )

focusChips :: forall w. State -> HH.HTML w Action
focusChips state =
  HH.div []
    [ ChipSet.Choice.chipSet
        ( (ChipSet.Choice.defaultConfig { toLabel: identity })
            { selected = Just state.focus
            , onChange = Just FocusChanged
            , additionalAttributes = [ HP.id_ "my-chips" ]
            }
        )
        [ Chip.Choice.Chip Chip.Choice.defaultConfig "One"
        , Chip.Choice.Chip Chip.Choice.defaultConfig "Two"
        ]
    , HH.text "\x00A0"
    , Button.button Button.Raised
        (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-chips") ] })
        [ HH.text "Focus" ]
    ]

component :: H.Component Query Input Message Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
