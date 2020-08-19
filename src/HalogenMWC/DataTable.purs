module HalogenMWC.DataTable where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Checkbox as Checkbox
import HalogenMWC.Checkbox

type Config r i
  = { label :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    }

defaultConfig :: Config r i
defaultConfig =
  { label: Nothing
  , additionalAttributes: []
  }

{-| Specify the data table's HTML5 aria-label attribute
-}
dataTable ::
  Config r i ->
  { thead :: Array (Row r i)
  , tbody :: Array (Row r i)
  } ->
  HH.HTML w i
dataTable (config_@{ additionalAttributes }) { thead, tbody } =
  HH.element "mdc-data-table"
    ([ dataTableCs ] <> additionalAttributes)
    [ HH.table
        ( Array.filterMap identity
            [ dataTableTableCs
            , ariaLabelAttr config_
            ]
        )
        [ HH.thead [] (map headerRow thead)
        , HH.tbody [ dataTableContentCs ] (map bodyRow tbody)
        ]
    ]

dataTableCs :: IProp r i
dataTableCs = HP.class_ mdc_data_table

dataTableTableCs :: Maybe (IProp r i)
dataTableTableCs = Just (HP.class_ mdc_data_table__table)

dataTableContentCs :: IProp r i
dataTableContentCs = HP.class_ mdc_data_table__content

ariaLabelAttr :: Config r i -> Maybe (IProp r i)
ariaLabelAttr { label } = map (HH.Attributes.attribute "aria-label") label

data Row r i
  = Row { attributes :: Array (IProp r i), nodes :: Array (Cell r i) }

row :: Array (IProp r i) -> Array (Cell r i) -> Row r i
row attributes nodes = Row { attributes = attributes, nodes = nodes }

selected :: Array (IProp r i)
selected =
  [ dataTableRowSelectedCs
  , HH.Attributes.attribute "aria-selected" "true"
  ]

dataTableRowSelectedCs :: IProp r i
dataTableRowSelectedCs = HP.class_ mdc_data_table__row____selected

headerRow :: Row r i -> HH.HTML w i
headerRow (Row { attributes, nodes }) = HH.tr ([ dataTableHeaderRowCs, attributes ] <> (map headerCell nodes))

dataTableHeaderRowCs :: IProp r i
dataTableHeaderRowCs = HP.class_ mdc_data_table__header_row

bodyRow :: Row r i -> HH.HTML w i
bodyRow (Row { attributes, nodes }) = HH.tr ([ dataTableRowCs, attributes ] <> (map bodyCell nodes))

dataTableRowCs :: IProp r i
dataTableRowCs = HP.class_ mdc_data_table__row

headerCell :: Cell r i -> HH.HTML w i
headerCell cell_ = case cell_ of
  Cell { numeric, attributes, nodes } ->
    HH.th
      ( Array.filterMap identity
          [ dataTableHeaderCellCs
          , columnHeaderRoleAttr
          , colScopeAttr
          , dataTableHeaderCellNumericCs numeric
          ]
          <> attributes
      )
      nodes
  CheckboxCell { attributes, config_ } ->
    HH.th
      ( Array.filterMap identity
          [ dataTableHeaderCellCs
          , columnHeaderRoleAttr
          , colScopeAttr
          , dataTableHeaderCellCheckboxCs
          ]
          <> attributes
      )
      [ Checkbox.checkbox (config_ { additionalAttributes = [ HP.class_ mdc_data_table__row_checkbox ] <> config__.additionalAttributes })
      ]

dataTableHeaderCellCs :: Maybe (IProp r i)
dataTableHeaderCellCs = Just (HP.class_ mdc_data_table__header_cell)

columnHeaderRoleAttr :: Maybe (IProp r i)
columnHeaderRoleAttr = Just (HH.Attributes.attribute "role" "columnheader")

colScopeAttr :: Maybe (IProp r i)
colScopeAttr = Just (HH.Attributes.attribute "scope" "col")

dataTableHeaderCellNumericCs :: Boolean -> Maybe (IProp r i)
dataTableHeaderCellNumericCs numeric =
  if numeric then
    Just (HP.class_ mdc_data_table__header_cell____numeric)
  else
    Nothing

dataTableHeaderCellCheckboxCs :: Maybe (IProp r i)
dataTableHeaderCellCheckboxCs = Just (HP.class_ mdc_data_table__header_cell____checkbox)

bodyCell :: Cell r i -> HH.HTML w i
bodyCell cell_ = case cell_ of
  Cell { numeric, attributes, nodes } ->
    HH.td
      ( Array.filterMap identity
          [ dataTableCellCs
          , dataTableCellNumericCs numeric
          ]
          <> attributes
      )
      nodes
  CheckboxCell { attributes, config_ } ->
    HH.td
      ( Array.filterMap identity
          [ dataTableCellCs
          , dataTableCellCheckboxCs
          ]
          <> attributes
      )
      [ Checkbox.checkbox (config_ { additionalAttributes = [ HP.class_ mdc_data_table__row_checkbox ] <> config_.additionalAttributes })
      ]

data Cell r i
  = Cell
    { numeric :: Boolean
    , attributes :: Array (IProp r i)
    , nodes :: Array (HH.HTML w i)
    }
  | CheckboxCell
    { config_ :: Checkbox.Config r i
    , attributes :: Array (IProp r i)
    }

cell :: Array (IProp r i) -> Array (HH.HTML w i) -> Cell r i
cell attributes nodes = Cell { numeric = false, attributes = attributes, nodes = nodes }

numericCell :: Array (IProp r i) -> Array (HH.HTML w i) -> Cell r i
numericCell attributes nodes = Cell { numeric: true, attributes: attributes, nodes: nodes }

checkboxCell :: Array (IProp r i) -> Checkbox.Config r i -> Cell r i
checkboxCell attributes config_ = CheckboxCell { attributes: attributes, config_: config_ }

dataTableCellCs :: Maybe (IProp r i)
dataTableCellCs = Just (HP.class_ mdc_data_table__cell)

dataTableCellNumericCs :: Boolean -> Maybe (IProp r i)
dataTableCellNumericCs numeric =
  if numeric then
    Just (HP.class_ mdc_data_table__cell____numeric)
  else
    Nothing

dataTableCellCheckboxCs :: Maybe (IProp r i)
dataTableCellCheckboxCs = Just (HP.class_ mdc_data_table__cell____checkbox)
