module HalogenMWC.DataTable
    ( Config, config


    , dataTable
    , Row, row
    , selected
    , Cell, cell
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
    =
        { label :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        }



config :: Config r i
config =
    Config
        { label = Nothing
        , additionalAttributes = []
        }


{-| Specify the data table's HTML5 aria-label attribute
-}
setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



dataTable :
    Config r i
    ->
        { thead :: Array (Row r i)
        , tbody :: Array (Row r i)
        }
    -> Html r i
dataTable ((Config { additionalAttributes }) as config_) { thead, tbody } =
    Html.node "mdc-data-table"
        (dataTableCs :: additionalAttributes)
        [ Html.table
            (Array.filterMap identity
                [ dataTableTableCs
                , ariaLabelAttr config_
                ]
            )
            [ Html.thead [] (Array.map headerRow thead)
            , Html.tbody [ dataTableContentCs ] (Array.map bodyRow tbody)
            ]
        ]


dataTableCs :: Html.Attribute r i
dataTableCs =
    HP.class_ mdc_data_table


dataTableTableCs :: Maybe (Html.Attribute r i)
dataTableTableCs =
    Just (HP.class_ mdc_data_table__table)


dataTableContentCs :: Html.Attribute r i
dataTableContentCs =
    HP.class_ mdc_data_table__content


ariaLabelAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaLabelAttr (Config { label }) =
    Maybe.map (Html.Attributes.attribute "aria-label") label



data Row r i
    = Row { attributes :: Array (IProp r i), nodes :: Array (Cell r i) }



row :: Array (IProp r i) -> Array (Cell r i) -> Row r i
row attributes nodes =
    Row { attributes = attributes, nodes = nodes }



selected :: Array (IProp r i)
selected =
    [ dataTableRowSelectedCs
    , Html.Attributes.attribute "aria-selected" "true"
    ]


dataTableRowSelectedCs :: Html.Attribute r i
dataTableRowSelectedCs =
    HP.class_ mdc_data_table__row____selected


headerRow :: Row r i -> Html r i
headerRow (Row { attributes, nodes }) =
    Html.tr (dataTableHeaderRowCs :: attributes) (Array.map headerCell nodes)


dataTableHeaderRowCs :: Html.Attribute r i
dataTableHeaderRowCs =
    HP.class_ mdc_data_table__header_row


bodyRow :: Row r i -> Html r i
bodyRow (Row { attributes, nodes }) =
    Html.tr (dataTableRowCs :: attributes) (Array.map bodyCell nodes)


dataTableRowCs :: Html.Attribute r i
dataTableRowCs =
    HP.class_ mdc_data_table__row


headerCell :: Cell r i -> Html r i
headerCell cell_ =
    case cell_ of
        Cell { numeric, attributes, nodes } ->
            Html.th
                (Array.filterMap identity
                    [ dataTableHeaderCellCs
                    , columnHeaderRoleAttr
                    , colScopeAttr
                    , dataTableHeaderCellNumericCs numeric
                    ]
                    ++ attributes
                )
                nodes

        CheckboxCell { attributes, config_ } ->
            Html.th
                (Array.filterMap identity
                    [ dataTableHeaderCellCs
                    , columnHeaderRoleAttr
                    , colScopeAttr
                    , dataTableHeaderCellCheckboxCs
                    ]
                    ++ attributes
                )
                [ Checkbox.checkbox
                    (case config_ of
                        Material.Checkbox.Internal.Config config__ ->
                            Material.Checkbox.Internal.Config
                                { config__
                                    | additionalAttributes =
                                        HP.class_ mdc_data_table__row_checkbox
                                            :: config__.additionalAttributes
                                }
                    )
                ]


dataTableHeaderCellCs :: Maybe (Html.Attribute r i)
dataTableHeaderCellCs =
    Just (HP.class_ mdc_data_table__header_cell)


columnHeaderRoleAttr :: Maybe (Html.Attribute r i)
columnHeaderRoleAttr =
    Just (Html.Attributes.attribute "role" "columnheader")


colScopeAttr :: Maybe (Html.Attribute r i)
colScopeAttr =
    Just (Html.Attributes.attribute "scope" "col")


dataTableHeaderCellNumericCs :: Boolean -> Maybe (Html.Attribute r i)
dataTableHeaderCellNumericCs numeric =
    if numeric then
        Just (HP.class_ mdc_data_table__header_cell____numeric)

    else
        Nothing


dataTableHeaderCellCheckboxCs :: Maybe (Html.Attribute r i)
dataTableHeaderCellCheckboxCs =
    Just (HP.class_ mdc_data_table__header_cell____checkbox)


bodyCell :: Cell r i -> Html r i
bodyCell cell_ =
    case cell_ of
        Cell { numeric, attributes, nodes } ->
            Html.td
                (Array.filterMap identity
                    [ dataTableCellCs
                    , dataTableCellNumericCs numeric
                    ]
                    ++ attributes
                )
                nodes

        CheckboxCell { attributes, config_ } ->
            Html.td
                (Array.filterMap identity
                    [ dataTableCellCs
                    , dataTableCellCheckboxCs
                    ]
                    ++ attributes
                )
                [ Checkbox.checkbox
                    (case config_ of
                        Material.Checkbox.Internal.Config config__ ->
                            Material.Checkbox.Internal.Config
                                { config__
                                    | additionalAttributes =
                                        HP.class_ mdc_data_table__row_checkbox
                                            :: config__.additionalAttributes
                                }
                    )
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
cell attributes nodes =
    Cell { numeric = False, attributes = attributes, nodes = nodes }


{-| Numeric data table cell (right-aligned contents)
-}
numericCell :: Array (IProp r i) -> Array (Html r i) -> Cell r i
numericCell attributes nodes =
    Cell { numeric = True, attributes = attributes, nodes = nodes }



checkboxCell :: Array (IProp r i) -> Checkbox.Config r i -> Cell r i
checkboxCell attributes config_ =
    CheckboxCell { attributes = attributes, config_ = config_ }


dataTableCellCs :: Maybe (Html.Attribute r i)
dataTableCellCs =
    Just (HP.class_ mdc_data_table__cell)


dataTableCellNumericCs :: Boolean -> Maybe (Html.Attribute r i)
dataTableCellNumericCs numeric =
    if numeric then
        Just (HP.class_ mdc_data_table__cell____numeric)

    else
        Nothing


dataTableCellCheckboxCs :: Maybe (Html.Attribute r i)
dataTableCellCheckboxCs =
    Just (HP.class_ mdc_data_table__cell____checkbox)
