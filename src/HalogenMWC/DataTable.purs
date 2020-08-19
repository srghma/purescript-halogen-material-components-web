module HalogenMWC.DataTable
  ( Config
  , config
  , dataTable
  , Row
  , row
  , selected
  , Cell
  , cell
  , numericCell
  , checkboxCell
  ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Checkbox as Checkbox
import HalogenMWC.Checkbox.Internal

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
  Html r i
dataTable (config_@{ additionalAttributes }) { thead, tbody } =
  HH.node "mdc-data-table"
    ([ dataTableCs ] <> additionalAttributes)
    [ HH.table
        ( Array.filterMap identity
            [ dataTableTableCs
            , ariaLabelAttr config_
            ]
        )
        [ HH.thead [] (Array.map headerRow thead)
        , HH.tbody [ dataTableContentCs ] (Array.map bodyRow tbody)
        ]
    ]

dataTableCs :: HH.Attribute r i
dataTableCs = HP.class_ mdc_data_table

dataTableTableCs :: Maybe (HH.Attribute r i)
dataTableTableCs = Just (HP.class_ mdc_data_table__table)

dataTableContentCs :: HH.Attribute r i
dataTableContentCs = HP.class_ mdc_data_table__content

ariaLabelAttr :: Config r i -> Maybe (HH.Attribute r i)
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

dataTableRowSelectedCs :: HH.Attribute r i
dataTableRowSelectedCs = HP.class_ mdc_data_table__row____selected

headerRow :: Row r i -> Html r i
headerRow (Row { attributes, nodes }) = HH.tr ([ dataTableHeaderRowCs, attributes ] <> (Array.map headerCell nodes))

dataTableHeaderRowCs :: HH.Attribute r i
dataTableHeaderRowCs = HP.class_ mdc_data_table__header_row

bodyRow :: Row r i -> Html r i
bodyRow (Row { attributes, nodes }) = HH.tr ([ dataTableRowCs, attributes ] <> (Array.map bodyCell nodes))

dataTableRowCs :: HH.Attribute r i
dataTableRowCs = HP.class_ mdc_data_table__row

headerCell :: Cell r i -> Html r i
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

dataTableHeaderCellCs :: Maybe (HH.Attribute r i)
dataTableHeaderCellCs = Just (HP.class_ mdc_data_table__header_cell)

columnHeaderRoleAttr :: Maybe (HH.Attribute r i)
columnHeaderRoleAttr = Just (HH.Attributes.attribute "role" "columnheader")

colScopeAttr :: Maybe (HH.Attribute r i)
colScopeAttr = Just (HH.Attributes.attribute "scope" "col")

dataTableHeaderCellNumericCs :: Boolean -> Maybe (HH.Attribute r i)
dataTableHeaderCellNumericCs numeric =
  if numeric then
    Just (HP.class_ mdc_data_table__header_cell____numeric)
  else
    Nothing

dataTableHeaderCellCheckboxCs :: Maybe (HH.Attribute r i)
dataTableHeaderCellCheckboxCs = Just (HP.class_ mdc_data_table__header_cell____checkbox)

bodyCell :: Cell r i -> Html r i
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
    , nodes :: Array (Html r i)
    }
  | CheckboxCell
    { config_ :: Checkbox.Config r i
    , attributes :: Array (IProp r i)
    }

cell :: Array (IProp r i) -> Array (Html r i) -> Cell r i
cell attributes nodes = Cell { numeric = False, attributes = attributes, nodes = nodes }

{-| Numeric data table cell (right-aligned contents)
-}
numericCell :: Array (IProp r i) -> Array (Html r i) -> Cell r i
numericCell attributes nodes = Cell { numeric = True, attributes = attributes, nodes = nodes }

checkboxCell :: Array (IProp r i) -> Checkbox.Config r i -> Cell r i
checkboxCell attributes config_ = CheckboxCell { attributes = attributes, config_ = config_ }

dataTableCellCs :: Maybe (HH.Attribute r i)
dataTableCellCs = Just (HP.class_ mdc_data_table__cell)

dataTableCellNumericCs :: Boolean -> Maybe (HH.Attribute r i)
dataTableCellNumericCs numeric =
  if numeric then
    Just (HP.class_ mdc_data_table__cell____numeric)
  else
    Nothing

dataTableCellCheckboxCs :: Maybe (HH.Attribute r i)
dataTableCellCheckboxCs = Just (HP.class_ mdc_data_table__cell____checkbox)
