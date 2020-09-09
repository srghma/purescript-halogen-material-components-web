module Demo.Pages.List where

import Demo.HOC.CatalogPage (CatalogPage)
import Halogen (AttrName(..))
import Material.Classes.Typography (mdc_typography____subtitle1)
import Protolude (Aff, Maybe(..), Unit, Void, const, map, ($), (/\), (<>), (==))

import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set
import Demo.Utils (mkComponentStatic)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.Checkbox as Checkbox
import HalogenMWC.Icon as Icon
import HalogenMWC.List as List
import HalogenMWC.List.Item as List.Item
import HalogenMWC.Radio as Radio
import Material.Classes.List (mdc_list_item__graphic, mdc_list_item__meta)
import Demo.Utils (focusById, mkComponentStatic)

type State =
  { checkboxes :: Set String
  , radio :: Maybe String
  , activated :: String
  , shapedActivated :: String
  }

type ChildSlots = ()

type Message = Void

initialState :: forall r w i . State
initialState =
    { checkboxes: Set.empty
    , radio: Nothing
    , activated: "Star"
    , shapedActivated: "Star"
    }

data Action
    = ToggleCheckbox String
    | SetRadio String
    | SetActivated String
    | SetShapedActivated String
    | Focus String

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
      ToggleCheckbox id     -> H.modify_ \state -> state { checkboxes = if Set.member id state.checkboxes then Set.delete id state.checkboxes else Set.insert id state.checkboxes }
      SetRadio id           -> H.modify_ (_ { radio = Just id })
      SetActivated id       -> H.modify_ (_ { activated = id })
      SetShapedActivated id -> H.modify_ (_ { shapedActivated = id })
      Focus id              -> H.liftEffect $ focusById id

config :: CatalogPage
config =
    { title: "List"
    , prelude: "Lists present multiple line items vertically as a single continuous element."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-lists"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-List"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-list"
        }
    , hero:
      let
        listItem = List.Item.listItem List.Item.defaultConfig [ HH.text "Line item" ]
      in
        mkComponentStatic $ HH.div_
          [ List.list
            (List.defaultConfig { additionalAttributes = [ HP.style $ "background: #fff;" <> demoList ] })
            listItem
            (Array.replicate 2 listItem)
          ]
    , content:
      H.mkComponent
        { initialState: const initialState
        , render: \state ->
          HH.div_
            [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Single-Line" ]
            , singleLineList
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Two-Line" ]
            , twoLineList
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Leading Icon" ]
            , leadingIconList
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "List with activated item" ]
            , activatedItemList state
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "List with shaped activated item" ]
            , shapedActivatedItemList state
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Trailing Icon" ]
            , trailingIconList
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ]
                [ HH.text "Two-Line with Leading and Trailing Icon and Divider" ]
            , folderList
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "List with Trailing Checkbox" ]
            , listWithTrailingCheckbox state
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "List with Trailing Radio Buttons" ]
            , listWithTrailingRadioButton state
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus List" ]
            , focusList
            ]
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }
    }

demoList :: String
demoList = "max-width: 600px; border: 1px solid rgba(0,0,0,.1);"

singleLineList :: forall w i . HH.HTML w i
singleLineList =
    let
        listItem = List.Item.listItem List.Item.defaultConfig [ HH.text "Line item" ]
    in
    List.list (List.defaultConfig { additionalAttributes = [ HP.style demoList ] })
    listItem
    (Array.replicate 2 listItem)

twoLineList :: forall w i . HH.HTML w i
twoLineList =
    let
        listItem =
            List.Item.listItem List.Item.defaultConfig
                [ List.Item.text []
                    { primary: [ HH.text "Line item" ]
                    , secondary: [ HH.text "Secondary text" ]
                    }
                ]
    in
    List.list
        (List.defaultConfig
            { twoLine = true
            , additionalAttributes = [ HP.style demoList ]
            }
        )
        listItem
        (Array.replicate 2 listItem)

leadingIconList :: forall w i . HH.HTML w i
leadingIconList =
    let
        listItem icon =
            List.Item.listItem List.Item.defaultConfig
                [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] icon ]
                , HH.text "Line item"
                ]
    in
    List.list (List.defaultConfig { additionalAttributes = [ HP.style demoList ] })
        (listItem "wifi")
        (map listItem [ "bluetooth", "data_usage" ])

trailingIconList :: forall w i . HH.HTML w i
trailingIconList =
    let
        listItem =
            List.Item.listItem List.Item.defaultConfig
                [ HH.text "Line item"
                , HH.div [ HP.class_ mdc_list_item__meta ] [ Icon.materialIcon [] "info" ]
                ]
    in
    List.list (List.defaultConfig { additionalAttributes = [ HP.style demoList ] })
        listItem
        (Array.replicate 2 listItem)

activatedItemList :: forall r w i . State -> HH.HTML w Action
activatedItemList state =
  let
    listItem ( icon /\ label ) =
      List.Item.listItem
        (List.Item.defaultConfig
          { selected = if state.activated == label then Just List.Item.Activated else Nothing
          , onClick = Just $ const $ SetActivated label
          }
        )
        [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] icon ], HH.text label ]
  in
  List.list (List.defaultConfig { additionalAttributes = [ HP.style demoList ] })
    (listItem ( "inbox" /\ "Inbox" ))
    (map listItem
        [ ( "star" /\ "Star" )
        , ( "send" /\ "Sent" )
        , ( "drafts" /\ "Drafts" )
        ]
    )

shapedActivatedItemList :: forall r w i . State -> HH.HTML w Action
shapedActivatedItemList state =
  let
    listItem ( icon /\ label ) =
      List.Item.listItem
        (List.Item.defaultConfig
          { selected = if state.shapedActivated == label then Just List.Item.Activated else Nothing
          , onClick = Just $ const $ SetShapedActivated label
          , additionalAttributes = [ HP.style "border-radius: 0 32px 32px 0;" ]
          }
        )
        [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] icon ], HH.text label ]
  in
  List.list (List.defaultConfig { additionalAttributes = [ HP.style demoList ] })
      (listItem ( "inbox" /\ "Inbox" ))
      (map listItem
          [ ( "star" /\ "Star" )
          , ( "send" /\ "Sent" )
          , ( "drafts" /\ "Drafts" )
          ]
      )

demoIcon :: forall r w i . IProp ( style :: String | r ) i
demoIcon = HP.style "background: rgba(0,0,0,.3); border-radius: 50%; color: #fff;"

folderList :: forall w i . HH.HTML w i
folderList =
    let
        listItem { primary, secondary } =
            List.Item.listItem List.Item.defaultConfig
                [ HH.div [ HP.class_ mdc_list_item__graphic, demoIcon ] [ Icon.materialIcon [] "folder" ]
                , List.Item.text []
                    { primary: [ HH.text primary ]
                    , secondary: [ HH.text secondary ]
                    }
                , HH.div [ HP.class_ mdc_list_item__meta ] [ Icon.materialIcon [] "info" ]
                ]
    in
    List.list
        (List.defaultConfig
            { avatarList = true
            , twoLine = true
            , additionalAttributes = [ HP.style demoList ]
            }
        )
        (listItem
            { primary: "Dog Photos", secondary: "9 Jan 2018" }
        )
        (map listItem
            [ { primary: "Cat Photos", secondary: "22 Dec 2017" }
            , { primary: "Potatoes", secondary: "30 Noc 2017" }
            , { primary: "Carrots", secondary: "17 Oct 2017" }
            ]
        )

listWithTrailingCheckbox :: forall r w i . State -> HH.HTML w Action
listWithTrailingCheckbox state =
    let
        listItem label =
            List.Item.listItem
                (List.Item.defaultConfig
                    { selected = if Set.member label state.checkboxes then Just List.Item.Selected else Nothing
                    }
                )
                [ HH.text "Dog Photos"
                , HH.div [ HP.class_ mdc_list_item__meta ]
                    [ Checkbox.checkbox
                        (Checkbox.defaultConfig
                            { onChange = Just $ const $ ToggleCheckbox label
                            , state =
                                (Just
                                    (if Set.member label state.checkboxes then
                                        Checkbox.Checked

                                     else
                                        Checkbox.Unchecked
                                    )
                                )
                            }
                        )
                    ]
                ]
    in
    List.list
        (List.defaultConfig { additionalAttributes = [ HP.attr (AttrName "role") "group", HP.style demoList ] })
        (listItem "Dog Photos")
        (map listItem
            [ "Cat Photos"
            , "Potatoes"
            , "Carrots"
            ]
        )

listWithTrailingRadioButton :: forall r w i . State -> HH.HTML w Action
listWithTrailingRadioButton state =
    let
        listItem label =
            List.Item.listItem
                (List.Item.defaultConfig
                    { selected = if state.radio == Just label then Just List.Item.Selected else Nothing
                    }
                )
                [ HH.text label
                , HH.div [ HP.class_ mdc_list_item__meta ]
                    [ Radio.radio
                        (Radio.defaultConfig
                            { checked = state.radio == Just label
                            , onChange = Just $ const $ SetRadio label
                            }
                        )
                    ]
                ]
    in
    List.list (List.defaultConfig { additionalAttributes = [ HP.style demoList ] })
        (listItem "Dog Photos")
        (map listItem
            [ "Cat Photos"
            , "Potatoes"
            , "Carrots"
            ]
        )

focusList :: forall r w i . HH.HTML w Action
focusList =
  HH.div_
    [ List.list
        (List.defaultConfig { additionalAttributes = [ HP.id_ "my-list", HP.style demoList ] })
        (List.Item.listItem List.Item.defaultConfig [ HH.text "Line item" ])
        (Array.replicate 2 $ List.Item.listItem List.Item.defaultConfig [ HH.text "Line item" ])
    , HH.text "\x00A0"
    , Button.buttonView Button.Raised
        (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-list") ] })
        [ HH.text "Focus" ]
    ]
