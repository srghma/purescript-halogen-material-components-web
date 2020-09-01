module Demo.Pages.DataTable where

import Demo.HOC.CatalogPage
import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.DataTable as DataTable
import HalogenMWC.Checkbox as Checkbox
import Material.Classes.Typography
import Data.Set (Set)
import Data.Set as Set
import Demo.Utils

type State = { selected :: Set String }

data Action
    = ItemSelected String
    | AllSelected
    | AllUnselected

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction =
  case _ of
    ItemSelected key ->
      H.modify_ \state -> state
        { selected =
            if Set.member key state.selected then
                Set.delete key state.selected
            else
                Set.insert key state.selected
        }
    AllSelected -> H.modify_ \state -> state { selected = Set.fromFoldable [ "Frozen yogurt", "Ice cream sandwich", "Eclair" ] }
    AllUnselected -> H.modify_ \state -> state { selected = (Set.empty :: Set String) }

catalogPage :: CatalogPage
catalogPage =
    { title: "Data Table"
    , prelude: "Data tables display information in a way that’s easy to scan, so that users can look for patterns and insights."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-data-tables"
        , documentation: Just "https://material.io/components/web/catalog/data-tables/"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-data-table"
        }
    , hero: mkComponentStatic standardDataTable
    , content:
        let
            render :: forall w. State -> HH.HTML w Action
            render state =
              HH.div_
                [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Data Table Standard" ]
                , standardDataTable
                , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Data Table with Row Selection" ]
                , dataTableWithRowSelection state
                ]
        in
          H.mkComponent
            { initialState: const { selected: Set.empty }
            , render
            , eval: H.mkEval H.defaultEval { handleAction = handleAction }
            }
    }

label :: { desert :: String, carbs :: String, protein :: String, comments :: String }
label =
    { desert: "Desert"
    , carbs: "Carbs (g)"
    , protein: "Protein (g)"
    , comments: "Comments"
    }

datum :: Array { desert :: String, carbs :: String, protein :: String, comments :: String }
datum =
    [ { desert: "Frozen yogurt"
      , carbs: "24"
      , protein: "4.0"
      , comments: "Super tasty"
      }
    , { desert: "Ice cream sandwich"
      , carbs: "37"
      , protein: "4.33333333333"
      , comments: "I like ice cream more"
      }
    , { desert: "Eclair"
      , carbs: "24"
      , protein: "6.0"
      , comments: "New filing flavor"
      }
    ]

standardDataTable :: forall w i . HH.HTML w i
standardDataTable =
  let
    row :: forall r . _ -> DataTable.Row r w i
    row { desert, carbs, protein, comments } =
      { attributes: []
      , nodes:
        [ DataTable.Cell { numeric: false, attributes: [], nodes: [ HH.text desert ] }
        , DataTable.Cell { numeric: true, attributes: [], nodes: [ HH.text carbs ] }
        , DataTable.Cell { numeric: true, attributes: [], nodes: [ HH.text protein ] }
        , DataTable.Cell { numeric: false, attributes: [], nodes: [ HH.text comments ] }
        ]
      }
  in
  DataTable.dataTable DataTable.defaultConfig
      { thead: [ row label ]
      , tbody: map row datum
      }

dataTableWithRowSelection :: forall r w i . State -> HH.HTML w Action
dataTableWithRowSelection state =
  let
    allSelected = Set.size state.selected == 3

    allUnselected = Set.size state.selected == 0

    headerRow { onChange, state } { desert, carbs, protein, comments } =
      [ { attributes: []
        , nodes:
          [ DataTable.CheckboxCell
            { config:
              Checkbox.defaultConfig
              { state = Just state
              , onChange = onChange
              }
            , attributes: []
            }
          , DataTable.Cell { numeric: false, attributes: [], nodes: [ HH.text desert ] }
          , DataTable.Cell { numeric: true,  attributes: [], nodes: [ HH.text carbs ] }
          , DataTable.Cell { numeric: true,  attributes: [], nodes: [ HH.text protein ] }
          , DataTable.Cell { numeric: false, attributes: [], nodes: [ HH.text comments ] }
          ]
        }
      ]

    row { onChange, selected } { desert, carbs, protein, comments } =
        { attributes: if selected then DataTable.selected else []
        , nodes:
          [ DataTable.CheckboxCell
            { attributes: []
            , config:
              Checkbox.defaultConfig
              { state = Just $ if selected then Checkbox.Checked else Checkbox.Unchecked
              , onChange = onChange
              }
            }
          , DataTable.Cell { numeric: false, attributes: [], nodes: [ HH.text desert ] }
          , DataTable.Cell { numeric: true, attributes: [], nodes: [ HH.text carbs ] }
          , DataTable.Cell { numeric: true, attributes: [], nodes: [ HH.text protein ] }
          , DataTable.Cell { numeric: false, attributes: [], nodes: [ HH.text comments ] }
          ]
        }
  in
  DataTable.dataTable DataTable.defaultConfig
    { thead:
      headerRow
      { onChange: Just $ if allSelected then const $ AllUnselected else const $ AllSelected
      , state:
          if allSelected then Checkbox.Checked
          else if allUnselected then Checkbox.Unchecked
          else Checkbox.Indeterminate
      }
      label
    , tbody:
      map
      (\data_ -> row
        { onChange: Just $ const $ ItemSelected data_.desert
        , selected: Set.member data_.desert state.selected
        }
        data_
      )
      datum
    }
