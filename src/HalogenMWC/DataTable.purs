module HalogenMWC.DataTable where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Halogen (AttrName(..), ElemName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.Checkbox as Checkbox
import Material.Classes.DataTable (mdc_data_table, mdc_data_table__cell, mdc_data_table__cell____checkbox, mdc_data_table__cell____numeric, mdc_data_table__content, mdc_data_table__header_cell, mdc_data_table__header_cell____checkbox, mdc_data_table__header_cell____numeric, mdc_data_table__header_row, mdc_data_table__row, mdc_data_table__row____selected, mdc_data_table__row_checkbox, mdc_data_table__table)

type Config i
  = { label :: Maybe String
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    }

data Cell r w i
  = Cell
    { numeric :: Boolean
    , attributes :: Array (IProp r i)
    , nodes :: Array (HH.HTML w i)
    }
  | CheckboxCell
    { config :: Checkbox.Config i
    , attributes :: Array (IProp r i)
    }

type Row cellR w i
  = { attributes :: Array (IProp I.HTMLtr i)
    , nodes :: Array (Cell cellR w i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { label: Nothing
  , additionalAttributes: []
  }

dataTable ::
  forall w i.
  Config i ->
  { thead :: Array (Row I.HTMLth w i)
  , tbody :: Array (Row I.HTMLtd w i)
  } ->
  HH.HTML w i
dataTable config { thead, tbody } =
  HH.element (ElemName "mdc-data-table")
    ([ HP.class_ mdc_data_table ] <> config.additionalAttributes)
    [ HH.table
        ( Array.catMaybes
            [ Just $ HP.class_ mdc_data_table__table
            , map (HP.attr (AttrName "aria-label")) config.label
            ]
        )
        [ HH.thead [] (map headerRow thead)
        , HH.tbody [ HP.class_ mdc_data_table__content ] (map bodyRow tbody)
        ]
    ]

selected :: forall r i. Array (IProp ( class :: String | r ) i)
selected =
  [ HP.class_ mdc_data_table__row____selected
  , HP.attr (AttrName "aria-selected") "true"
  ]

headerRow :: forall w i. Row I.HTMLth w i -> HH.HTML w i
headerRow row =
  HH.tr
    ([ HP.class_ mdc_data_table__header_row ] <> row.attributes)
    (map headerCell row.nodes)

bodyRow :: forall w i. Row I.HTMLtd w i -> HH.HTML w i
bodyRow row =
  HH.tr
    ([ HP.class_ mdc_data_table__row ] <> row.attributes)
    (map bodyCell row.nodes)

headerCell :: forall w i. Cell I.HTMLth w i -> HH.HTML w i
headerCell = case _ of
  Cell { numeric, attributes, nodes } ->
    HH.th
      ( [ HP.classes
            $ [ mdc_data_table__header_cell ]
            <> if numeric then [ mdc_data_table__header_cell____numeric ] else []
        , columnHeaderRoleAttr
        , colScopeAttr
        ]
          <> attributes
      )
      nodes
  CheckboxCell { attributes, config } ->
    HH.th
      ( [ HP.classes [ mdc_data_table__header_cell, mdc_data_table__header_cell____checkbox ]
        , columnHeaderRoleAttr
        , colScopeAttr
        ]
          <> attributes
      )
      [ Checkbox.checkbox
          ( config
              { additionalAttributes =
                [ HP.class_ mdc_data_table__row_checkbox ]
                  <> config.additionalAttributes
              }
          )
      ]

columnHeaderRoleAttr :: forall r i. IProp r i
columnHeaderRoleAttr = HP.attr (AttrName "role") "columnheader"

colScopeAttr :: forall r i. IProp r i
colScopeAttr = HP.attr (AttrName "scope") "col"

bodyCell :: forall w i. Cell I.HTMLtd w i -> HH.HTML w i
bodyCell = case _ of
  Cell { numeric, attributes, nodes } ->
    HH.td
      ( [ HP.classes
            $ Array.catMaybes
                [ Just mdc_data_table__cell
                , if numeric then Just mdc_data_table__cell____numeric else Nothing
                ]
        ]
          <> attributes
      )
      nodes
  CheckboxCell { attributes, config } ->
    HH.td
      ( [ HP.classes
            [ mdc_data_table__cell
            , mdc_data_table__cell____checkbox
            ]
        ]
          <> attributes
      )
      [ Checkbox.checkbox (config { additionalAttributes = [ HP.class_ mdc_data_table__row_checkbox ] <> config.additionalAttributes })
      ]
