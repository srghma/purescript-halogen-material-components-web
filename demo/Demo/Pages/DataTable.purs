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
import Material.Classes.Typography
import Data.Set (Set)
import Data.Set as Set
import Demo.Utils

type State = { selected :: Set String }

initialState :: forall r w i . State
initialState = { selected: Set.empty }

data Action
    = ItemSelected String
    | AllSelected
    | AllUnselected

handleAction :: forall r w i . Action -> State -> State
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
    AllSelected -> H.modify_ \state -> state { selected = Set.fromList [ "Frozen yogurt", "Ice cream sandwich", "Eclair" ] }
    AllUnselected -> H.modify_ \state -> state { selected = Set.empty }

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
        [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Data Table Standard" ]
        , standardDataTable
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Data Table with Row Selection" ]
        , dataTableWithRowSelection state
        ]
    }

label :: forall r w i . { desert :: String, carbs :: String, protein :: String, comments :: String }
label =
    { desert: "Desert"
    , carbs: "Carbs (g)"
    , protein: "Protein (g)"
    , comments: "Comments"
    }

data :: forall r w i . Array { desert :: String, carbs :: String, protein :: String, comments :: String }
data =
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
        row { desert, carbs, protein, comments } =
            DataTable.row []
                [ DataTable.cell [] [ HH.text desert ]
                , DataTable.numericCell [] [ HH.text carbs ]
                , DataTable.numericCell [] [ HH.text protein ]
                , DataTable.cell [] [ HH.text comments ]
                ]
    in
    DataTable.dataTable DataTable.defaultConfig
        { thead: [ row label ]
        , tbody: map row data
        }

dataTableWithRowSelection :: forall r w i . State -> HH.HTML w Action
dataTableWithRowSelection state =
    let
        allSelected =
            Set.size state.selected == 3

        allUnselected =
            Set.size state.selected == 0

        headerRow { onChange, state } { desert, carbs, protein, comments } =
            [ DataTable.row []
                [ DataTable.checkboxCell []
                    (Checkbox.defaultConfig
                        { state = (Just state)
                        , onChange = onChange
                    )
                , DataTable.cell [] [ HH.text desert ]
                , DataTable.numericCell [] [ HH.text carbs ]
                , DataTable.numericCell [] [ HH.text protein ]
                , DataTable.cell [] [ HH.text comments ]
                ]
            ]

        row { onChange, selected } { desert, carbs, protein, comments } =
            DataTable.row
                (if selected then
                    DataTable.selected

                 else
                    []
                )
                [ DataTable.checkboxCell []
                    (Checkbox.defaultConfig
                        { state =
                            (Just
                                (if selected then
                                    Checkbox.Checked

                                 else
                                    Checkbox.Unchecked
                                )
                            )
                        , onChange = onChange
                    )
                , DataTable.cell [] [ HH.text desert ]
                , DataTable.numericCell [] [ HH.text carbs ]
                , DataTable.numericCell [] [ HH.text protein ]
                , DataTable.cell [] [ HH.text comments ]
                ]
    in
    DataTable.dataTable DataTable.defaultConfig
        { thead =
            headerRow
                { onChange =
                    if allSelected then
                        AllUnselected

                    else
                        AllSelected
                , state =
                    if allSelected then
                        Checkbox.Checked

                    else if allUnselected then
                        Checkbox.Unchecked

                    else
                        Checkbox.Indeterminate
                }
                label
        , tbody =
            map
                (\({ desert } as data_) ->
                    row
                        { onChange: ItemSelected desert
                        , selected: Set.member desert state.selected
                        }
                        data_
                )
                data
        }
