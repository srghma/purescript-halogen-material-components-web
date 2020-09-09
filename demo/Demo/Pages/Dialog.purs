module Demo.Pages.Dialog where

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils (mkComponentStatic)
import Material.Classes.List (mdc_list_item__graphic)
import Protolude (class Eq, Aff, Maybe(..), Unit, Void, const, map, ($), (/\), (==))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.Dialog as Dialog
import HalogenMWC.Icon as Icon
import HalogenMWC.List as List
import HalogenMWC.List.Item as List.Item
import HalogenMWC.Radio as Radio
import Material.Classes.Dialog (mdc_dialog, mdc_dialog____open, mdc_dialog__actions, mdc_dialog__content, mdc_dialog__surface, mdc_dialog__title)

data Dialog
  = AlertDialog
  | ConfirmationDialog
  | ScrollableDialog
  | SimpleDialog

type ChildSlots = ()

type Message = Void

derive instance eqDialog :: Eq Dialog

type State = { open :: Maybe Dialog }

initialState :: State
initialState = { open: Nothing }

data Action
    = Close
    | Show Dialog

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
    Close -> H.modify_ (_ { open = Nothing })
    Show id -> H.modify_ (_ { open = Just id })

config :: CatalogPage
config =
  { title: "Dialog"
  , prelude: "Dialogs inform users about a specific task and may contain critical information, require decisions, or involve multiple tasks."
  , resources:
      { materialDesignGuidelines: Just "https://material.io/go/design-dialogs"
      , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Dialog"
      , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-dialog"
      }
  , hero: mkComponentStatic $ HH.div_ heroDialog
  , content:
      let
        render :: forall w. State -> HH.HTML w Action
        render state =
          HH.div_
            [ Button.buttonView Button.Text
                (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Show AlertDialog) ] })
                [ HH.text "Alert" ]
            , HH.text " "
            , Button.buttonView Button.Text
                (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Show SimpleDialog) ] })
                [ HH.text "Simple" ]
            , HH.text " "
            , Button.buttonView Button.Text
                (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Show ConfirmationDialog) ] })
                [ HH.text "Confirmation" ]
            , HH.text " "
            , Button.buttonView Button.Text
                (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Show ScrollableDialog) ] })
                [ HH.text "Scrollable" ]
            , HH.text " "
            , alertDialog state
            , simpleDialog state
            , confirmationDialog state
            , scrollableDialog state
            ]
      in
        H.mkComponent
          { initialState: const initialState
          , render
          , eval: H.mkEval H.defaultEval { handleAction = handleAction }
          }
  }

alertDialog :: forall w . State -> HH.HTML w Action
alertDialog state =
  Dialog.dialog
    (Dialog.defaultConfig
        { open = state.open == Just AlertDialog
        , onClose = Just $ const $ Close
        }
    )
    { title: Nothing
    , content: [ HH.text "Discard draft?" ]
    , actions:
        let
          conf = Button.defaultConfig { additionalAttributes = [ HE.onClick (const Close) ] }
        in
        [ Button.buttonView Button.Text conf [ HH.text "Cancel" ]
        , Button.buttonView Button.Text conf [ HH.text "Discard" ]
        ]
    }

simpleDialog :: forall r w i . State -> HH.HTML w Action
simpleDialog state =
    let
      listItem ( icon /\ label ) =
        List.Item.listItem
          (List.Item.defaultConfig { onClick = Just $ const $ Close })
          [ HH.div
              [ HP.style "background-color: rgba(0,0,0,.3); color: #fff;"
              , HP.class_ mdc_list_item__graphic
              ]
              [ Icon.materialIcon [] icon ]
          , HH.text label
          ]
    in
    Dialog.dialog
      (Dialog.defaultConfig
          { open = (state.open == Just SimpleDialog)
          , onClose = Just $ const $ Close
          }
      )
      { title: Just "Select an account"
      , content:
          [ List.list (List.defaultConfig { avatarList = true })
              (listItem ( "person" /\ "user1@example.com" ))
              (map listItem
                  [ ( "person" /\ "user2@example.com" )
                  , ( "add" /\ "Add account" )
                  ]
              )
          ]
      , actions: []
      }

confirmationDialog :: forall r w i . State -> HH.HTML w Action
confirmationDialog state =
    let
        listItem ( checked /\ label ) =
          List.Item.listItem
            List.Item.defaultConfig
            [ HH.div [ HP.class_ mdc_list_item__graphic ]
            [ Radio.radio (Radio.defaultConfig { checked = checked }) ]
            , HH.text label
            ]
    in
    Dialog.dialog
        (Dialog.defaultConfig
            { open = (state.open == Just ConfirmationDialog)
            , onClose = Just $ const $ Close
            }
        )
        { title: Just "Phone ringtone"
        , content:
            [ List.list (List.defaultConfig { avatarList = true })
                (listItem ( true /\ "Never Gonna Give You Up" ))
                (map listItem
                    [ ( false /\ "Hot Cross Buns" )
                    , ( false /\ "None" )
                    ]
                )
            ]
        , actions:
            let
              conf = Button.defaultConfig { additionalAttributes = [ HE.onClick (const Close) ] }
            in
            [ Button.buttonView Button.Text conf [ HH.text "Cancel" ]
            , Button.buttonView Button.Text conf [ HH.text "Ok" ]
            ]
        }

scrollableDialog :: forall r w i . State -> HH.HTML w Action
scrollableDialog state =
    Dialog.dialog
        (Dialog.defaultConfig
            { open = state.open == Just ScrollableDialog
            , onClose = Just $ const $ Close
            }
        )
        { title: Just "The Wonderful Wizard of Oz"
        , content:
            [ HH.p_
                [ HH.text """
                    Dorothy lived in the midst of the great Kansas prairies,
                    with Uncle Henry, who was a farmer, and Aunt Em, who was
                    the farmer's wife. Their house was small, for the lumber to
                    build it had to be carried by wagon many miles. There were
                    four walls, a floor and a roof, which made one room; and
                    this room contained a rusty looking cookstove, a cupboard
                    for the dishes, a table, three or four chairs, and the
                    beds. Uncle Henry and Aunt Em had a big bed in one corner,
                    and Dorothy a little bed in another corner. There was no
                    garret at all, and no cellar--except a small hole dug in
                    the ground, called a cyclone cellar, where the family could
                    go in case one of those great whirlwinds arose, mighty
                    enough to crush any building in its path. It was reached by
                    a trap door in the middle of the floor, from which a ladder
                    led down into the small, dark hole.
                  """
                ]
            , HH.p_
                [ HH.text """
                    When Dorothy stood in the doorway and looked around, she
                    could see nothing but the great gray prairie on every side.
                    Not a tree nor a house broke the broad sweep of flat
                    country that reached to the edge of the sky in all
                    directions.  The sun had baked the plowed land into a gray
                    mass, with little cracks running through it. Even the grass
                    was not green, for the sun had burned the tops of the long
                    blades until they were the same gray color to be seen
                    everywhere.  Once the house had been painted, but the sun
                    blistered the paint and the rains washed it away, and now
                    the house was as dull and gray as everything else.
                  """
                ]
            , HH.p_
                [ HH.text """
                    When Aunt Em came there to live she was a young, pretty
                    wife. The sun and wind had changed her, too. They had taken
                    the sparkle from her eyes and left them a sober gray; they
                    had taken the red from her cheeks and lips, and they were
                    gray also. She was thin and gaunt, and never smiled now.
                    When Dorothy, who was an orphan, first came to her, Aunt Em
                    had been so startled by the child's laughter that she would
                    scream and press her hand upon her heart whenever Dorothy's
                    merry voice reached her ears; and she still looked at the
                    little girl with wonder that she could find anything to
                    laugh at.
                  """
                ]
            , HH.p_
                [ HH.text """
                    Uncle Henry never laughed. He worked hard from morning till
                    night and did not know what joy was. He was gray also, from
                    his long beard to his rough boots, and he looked stern and
                    solemn, and rarely spoke.
                  """
                ]
            , HH.p_
                [ HH.text """
                    It was Toto that made Dorothy laugh, and saved her from
                    growing as gray as her other surroundings. Toto was not
                    gray; he was a little black dog, with long silky hair and
                    small black eyes that twinkled merrily on either side of
                    his funny, wee nose. Toto played all day long, and Dorothy
                    played with him, and loved him dearly.
                  """
                ]
            , HH.p_
                [ HH.text """
                    Today, however, they were not playing. Uncle Henry sat upon
                    the doorstep and looked anxiously at the sky, which was
                    even grayer than usual. Dorothy stood in the door with Toto
                    in her arms, and looked at the sky too. Aunt Em was washing
                    the dishes.
                  """
                ]
            , HH.p_
                [ HH.text """
                    From the far north they heard a low wail of the wind, and
                    Uncle Henry and Dorothy could see where the long grass
                    bowed in waves before the coming storm.  There now came a
                    sharp whistling in the air from the south, and as they
                    turned their eyes that way they saw ripples in the grass
                    coming from that direction also.
                  """
                ]
            ]
        , actions:
            let
              conf = Button.defaultConfig { additionalAttributes = [ HE.onClick (const Close) ] }
            in
            [ Button.buttonView Button.Text conf [ HH.text "Decline" ]
            , Button.buttonView Button.Text conf [ HH.text "Continue" ]
            ]
        }

heroDialog :: forall r w i . Array (HH.HTML w Action)
heroDialog =
  [ HH.div
    [ HP.classes [ mdc_dialog, mdc_dialog____open ]
    , HP.style "position: relative;"
    ]
    [ HH.div
        [ HP.class_ mdc_dialog__surface ]
        [ HH.div [ HP.class_ mdc_dialog__title ] [ HH.text "Get this party started?" ]
        , HH.div [ HP.class_ mdc_dialog__content ] [ HH.text "Turn up the jams and have a good time." ]
        , HH.div
          [ HP.class_ mdc_dialog__actions ]
          [ Button.buttonView Button.Text Button.defaultConfig [ HH.text "Decline" ]
          , Button.buttonView Button.Text Button.defaultConfig [ HH.text "Accept" ]
          ]
        ]
    ]
  ]
