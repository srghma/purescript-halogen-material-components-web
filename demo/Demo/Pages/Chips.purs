module Demo.Pages.Chips where

import Data.Set
import Demo.HOC.CatalogPage
import Halogen
import Halogen
import Material.Classes.Typography
import Protolude
import Protolude

import Data.Array as Array
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Maybe as Maybe
import Halogen as H
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
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

type State =
  { chip :: Maybe String
  , size :: Size
  , inputChips :: Array String
  , input :: String
  , accessories :: Set String
  , contacts :: Set String
  , focus :: String
  }

initialState :: forall r w i . State
initialState =
  { chip: Just "Chip One"
  , size: Small
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

data Action
  = ChipChanged String
  | SizeChanged Size
  | AccessoriesChanged String
  | ContactChanged String
  | InputChanged String
  | ChipInputDeleted String
  | KeyPressed KeyboardEvent
  | FocusChanged String
  | Focus String
  | Focused (Result Browser.Dom.Error ())

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
    ChipChanged chip -> H.modify_ (_ { chip = Just chip })
    SizeChanged size -> H.modify_ (_ { size = size })
    InputChanged input -> H.modify_ (_ { input = input })
    AccessoriesChanged accessory ->
      H.modify_
        (\state ->
          state
          { accessories =
            (if Set.member accessory state.accessories then
                Set.remove accessory
              else
                Set.insert accessory
            )
            state.accessories
          }
        )

    ContactChanged contact ->
      H.modify_
        (\state ->
          state
          { contacts =
            (if Set.member accessory state.contacts then
                Set.remove accessory
              else
                Set.insert accessory
            )
            state.contacts
          }
        )

    ChipInputDeleted inputChip ->
      H.modify_
        (\state ->
          state
          { inputChips = Array.filter ((/=) inputChip) state.inputChips
          }
        )

    KeyPressed keyboardEvent ->
      case Web.UIEvent.KeyboardEvent.key keyboardEvent of
           "Backspace" -> do
              state <- H.get
              when (String.null state.input) do
                H.modify_ \state ->
                  state
                  { inputChips = Array.take (Array.length state.inputChips - 1) state.inputChips
                  }
           "Enter" -> do
              state <- H.get
              let trimmedInput = String.trim state.input
              unless (String.null trimmedInput) do
                H.put
                  ( state
                    { input = ""
                    , inputChips =
                      if Array.elem trimmedInput state.inputChips
                        then Array.snoc state.inputChips trimmedInput
                        else state.inputChips
                    }
                  )
           _ -> pure unit
    Focus id -> H.liftEffect $ focusById id

catalogPage :: CatalogPage
catalogPage =
    { title: "Chips"
    , prelude: "Chips are compact elements that allow users to enter information, select a choice, filter content, or trigger an action."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-chips"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Chips"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips"
        }
    , hero: heroChips state
    , content:
        [ HH.h2 [ mdc_typography____subtitle1 ] [ HH.text "Choice Chips" ]
        , choiceChips state
        , HH.h2 [ mdc_typography____subtitle1 ] [ HH.text "Filter Chips" ]
        , HH.h3 [ mdc_typography____body2 ] [ HH.text "No leading icon" ]
        , filterChips1 state
        , HH.h3 [ mdc_typography____body2 ] [ HH.text "With leading icon" ]
        , filterChips2 state
        , HH.h2 [ mdc_typography____subtitle1 ] [ HH.text "Action Chips" ]
        , actionChips state
        , HH.h2 [ mdc_typography____subtitle1 ] [ HH.text "Shaped Chips" ]
        , shapedChips state
        , HH.h2 [ mdc_typography____subtitle1 ] [ HH.text "Input Chips" ]
        , inputChips state
        , HH.h2 [ mdc_typography____subtitle1 ] [ HH.text "Focus Chips" ]
        , focusChips state
        ]
    }

heroChips :: forall r w i . State -> Array (HH.HTML Action)
heroChips state =
  [ ChipSet.Choice.chipSet
      ((ChipSet.Choice.defaultConfig { toLabel: identity })
        { selected = state.chip
        , onChange = ChipChanged
        }
      )
      [ Chip.Choice.chip Chip.Choice.defaultConfig "Chip One"
      , Chip.Choice.chip Chip.Choice.defaultConfig "Chip Two"
      , Chip.Choice.chip Chip.Choice.defaultConfig "Chip Three"
      , Chip.Choice.chip Chip.Choice.defaultConfig "Chip Four"
      ]
  ]

choiceChips :: forall r w i . State -> HH.HTML Action
choiceChips state =
  let
    toLabel =
      case _ of
            ExtraSmall -> "Extra Small"
            Small -> "Small"
            Medium -> "Medium"
            Large -> "Large"
            ExtraLarge -> "Extra Large"
  in
  ChipSet.Choice.chipSet
      ((ChipSet.Choice.defaultConfig { toLabel: toLabel })
          { selected = Just state.size
          , onChange = SizeChanged
          }
      )
      [ Chip.Choice.chip Chip.Choice.defaultConfig ExtraSmall
      , Chip.Choice.chip Chip.Choice.defaultConfig Small
      , Chip.Choice.chip Chip.Choice.defaultConfig Medium
      , Chip.Choice.chip Chip.Choice.defaultConfig Large
      , Chip.Choice.chip Chip.Choice.defaultConfig ExtraLarge
      ]

inputChips :: forall r w i . State -> HH.HTML Action
inputChips state =
    HH.div
    [ HP.style "position: relative; display: flex;"
    ]
    [ ChipSet.Input.chipSet []
        (map
          (\label ->
            Tuple
            label
            ( Chip.Input.chip
              (Chip.Input.defaultConfig
                { onDelete = ChipInputDeleted label
                }
              )
              label
            )
          )
          state.inputChips
        )
    , HH.input
        [ HP.value state.input
        , HE.onInput InputChanged
        , HE.onKeyDown KeyPressed
        ]
        []
    ]

filterChips1 :: forall r w i . State -> HH.HTML Action
filterChips1 state =
  let
    chip accessory =
      Chip.Filter.chip
        (Chip.Filter.defaultConfig
          { selected = (Set.member accessory state.accessories)
          , onChange = (AccessoriesChanged accessory)
          }
        )
        accessory
  in
  ChipSet.Filter.chipSet []
      (map chip
          [ "Tops"
          , "Bottoms"
          , "Shoes"
          , "Accessories"
          ]
      )

filterChips2 :: forall r w i . State -> HH.HTML Action
filterChips2 state =
  let
    chip label =
      Chip.Filter.chip
        (Chip.Filter.defaultConfig
          { selected = (Set.member label state.contacts)
          , icon = (Just "face")
          , onChange = (ContactChanged label)
          }
        )
        label
  in
  ChipSet.Filter.chipSet []
    (map chip
      [ "Alice"
      , "Bob"
      , "Charlie"
      , "Danielle"
      ]
    )

actionChips :: forall r w i . State -> HH.HTML Action
actionChips state =
    let
        chip ( icon, label ) =
            Chip.Action.chip
                (Chip.Action.defaultConfig
                    { icon = (Just icon)
                )
                label
    in
    ChipSet.Action.chipSet []
        (map chip
            [ ( "event", "Add to calendar" )
            , ( "bookmark", "Bookmark" )
            , ( "alarm", "Set alarm" )
            , ( "directions", "Get directions" )
            ]
        )

shapedChips :: forall r w i . State -> HH.HTML w i
shapedChips state =
    let
        chip label =
            Chip.Action.chip
                (Chip.Action.defaultConfig
                    { additionalAttributes = [ HP.style "border-radius: 4px;" ]
                )
                label
    in
    ChipSet.Action.chipSet []
        (map chip
            [ "Bookcase"
            , "TV Stand"
            , "Sofas"
            , "Office chairs"
            ]
        )

focusChips :: forall r w i . State -> HH.HTML Action
focusChips state =
    HH.div []
        [ ChipSet.Choice.chipSet
            (ChipSet.Choice.defaultConfig { toLabel: identity }
                , selected = (Just state.focus)
                , onChange = FocusChanged
                , additionalAttributes = [ HP.id_ "my-chips" ]
            )
            [ Chip.Choice.chip Chip.Choice.defaultConfig "One"
            , Chip.Choice.chip Chip.Choice.defaultConfig "Two"
            ]
        , HH.text "\x00A0"
        , Button.button Button.Raised
            (Button.defaultConfig
                { onClick = (Focus "my-chips")
            )
            "Focus"
        ]
