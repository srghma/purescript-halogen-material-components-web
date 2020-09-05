module HalogenMWC.DataTable where

import Prelude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen (AttrName(..), ClassName, ElemName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.Checkbox as Checkbox
import Material.Classes.DataTable (mdc_data_table, mdc_data_table__cell, mdc_data_table__cell____checkbox, mdc_data_table__cell____numeric, mdc_data_table__content, mdc_data_table__header_cell, mdc_data_table__header_cell____checkbox, mdc_data_table__header_cell____numeric, mdc_data_table__header_row, mdc_data_table__row, mdc_data_table__row____selected, mdc_data_table__row_checkbox, mdc_data_table__table)

type Config i =
  { label :: Maybe String
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  , additionalClasses :: Array ClassName
  }

data Cell r w i
  = Cell
    { numeric :: Boolean
    , additionalClasses :: Array ClassName
    , additionalAttributes :: Array (IProp r i)
    , nodes :: Array (HH.HTML w i)
    }
  | CheckboxCell
    { config :: Checkbox.Config i
    , additionalAttributes :: Array (IProp r i)
    , additionalClasses :: Array ClassName
    }

type Row cellR w i =
  { additionalAttributes :: Array (IProp I.HTMLtr i)
  , additionalClasses :: Array ClassName
  , nodes :: Array (Cell cellR w i)
  }

defaultConfig :: forall i. Config i
defaultConfig =
  { label: Nothing
  , additionalAttributes: []
  , additionalClasses: []
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
    ( [ HP.classes $ Array.concat
        [ [ mdc_data_table__header_row ]
        , row.additionalClasses
        ]
      ]
      <> row.additionalAttributes
    )
    (map headerCell row.nodes)

bodyRow :: forall w i. Row I.HTMLtd w i -> HH.HTML w i
bodyRow row =
  HH.tr
    ( [ HP.classes $ Array.concat
        [ [ mdc_data_table__row ]
        , row.additionalClasses
        ]
      ]
      <> row.additionalAttributes
    )
    (map bodyCell row.nodes)

headerCell :: forall w i. Cell I.HTMLth w i -> HH.HTML w i
headerCell = case _ of
  Cell { numeric, additionalAttributes, additionalClasses, nodes } ->
    HH.th
      ( [ HP.classes
            $ [ mdc_data_table__header_cell ]
            <> if numeric then [ mdc_data_table__header_cell____numeric ] else []
            <> additionalClasses
        , columnHeaderRoleAttr
        , colScopeAttr
        ]
        <> additionalAttributes
      )
      nodes
  CheckboxCell { additionalAttributes, additionalClasses, config } ->
    HH.th
      ( [ HP.classes $ [ mdc_data_table__header_cell, mdc_data_table__header_cell____checkbox ] <> additionalClasses
        , columnHeaderRoleAttr
        , colScopeAttr
        ]
        <> additionalAttributes
      )
      [ Checkbox.checkbox
        ( config
          { additionalClasses = [ mdc_data_table__row_checkbox ] <> config.additionalClasses
          }
        )
      ]

columnHeaderRoleAttr :: forall r i. IProp r i
columnHeaderRoleAttr = HP.attr (AttrName "role") "columnheader"

colScopeAttr :: forall r i. IProp r i
colScopeAttr = HP.attr (AttrName "scope") "col"

bodyCell :: forall w i. Cell I.HTMLtd w i -> HH.HTML w i
bodyCell = case _ of
  Cell { numeric, additionalAttributes, additionalClasses, nodes } ->
    HH.td
      ( [ HP.classes $ Array.concat
            [ [ mdc_data_table__cell ]
            , if numeric then [ mdc_data_table__cell____numeric ] else []
            , additionalClasses
            ]
        ]
        <> additionalAttributes
      )
      nodes
  CheckboxCell { additionalAttributes, additionalClasses, config } ->
    HH.td
      ( [ HP.classes $
          [ mdc_data_table__cell
          , mdc_data_table__cell____checkbox
          ]
          <> additionalClasses
        ]
        <> additionalAttributes
      )
      [ Checkbox.checkbox
        ( config
          { additionalClasses = [ mdc_data_table__row_checkbox ] <> config.additionalClasses
          }
        )
      ]
