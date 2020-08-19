module HalogenMWC.DataTable
    ( Config, config
    , setLabel
    , setAttributes
    , dataTable
    , Row, row
    , selected
    , Cell, cell
    , numericCell
    , checkboxCell
    ) where

{-| Data tables display information in a way that’s easy to scan, so that users
can look for patterns and insights.


# Table of Contents

  - [Resources](#resources)
  - [Configution](#configuration)
      - [Configuration Options](#configuration-options)
  - [Basic Usage](#basic-usage)
  - [Data Table](#data-table)
  - [Row](#row)
      - [Selected Row](#selected-row)
  - [Cell](#cell)
      - [Numeric Cell](#numeric-cell)
      - [Checkbox Cell](#checkbox-cell)


# Resources

  - [Demo: Data Table](https://aforemny.github.io/material-components-web-elm/#data-table)
  - [Material Design Guidelines: Data tables](https://material.io/go/design-data-tables)
  - [MDC Web: Data Table](https://github.com/material-components/material-components-web/tree/master/packages/mdc-data-table)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-data-table#sass-mixins)


# Basic Usage

    import Material.DataTable as DataTable

    main =
        DataTable.dataTable DataTable.config
            { thead =
                [ DataTable.row []
                    [ DataTable.cell [] [ text "Desert" ] ]
                ]
            , tbody =
                [ DataTable.row []
                    [ DataTable.cell [] [ text "Frozen yogurt" ]
                    ]
                ]
            }


# Configuration

@docs Config, config


## Configuration Options

@docs setLabel
@docs setAttributes


# Data Table

@docs dataTable


# Row

@docs Row, row


## Selected Row

    DataTable.row DataTable.selected []

@docs selected


# Cell

@docs Cell, cell


## Numeric Cell

    DataTable.numericCell [] [ text "9.000,00" ]

@docs numericCell


## Checkbox Cell

    import Material.Checkbox as Checkbox

    DataTable.checkboxCell [] Checkbox.config

@docs checkboxCell

-}

import Html (Html)
import Html.Attributes (class)
import Material.Checkbox as Checkbox
import Material.Checkbox.Internal


{-| Configuration of a data table
-}
type Config r i
    = Config
        { label :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        }


{-| Default configuration of a data table
-}
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


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Data table view function
-}
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
    class "mdc-data-table"


dataTableTableCs :: Maybe (Html.Attribute r i)
dataTableTableCs =
    Just (class "mdc-data-table__table")


dataTableContentCs :: Html.Attribute r i
dataTableContentCs =
    class "mdc-data-table__content"


ariaLabelAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaLabelAttr (Config { label }) =
    Maybe.map (Html.Attributes.attribute "aria-label") label


{-| Row type
-}
data Row r i
    = Row { attributes :: Array (IProp r i), nodes :: Array (Cell r i) }


{-| Row view function
-}
row :: Array (IProp r i) -> Array (Cell r i) -> Row r i
row attributes nodes =
    Row { attributes = attributes, nodes = nodes }


{-| Attribute to mark a row as selected

This has no effect on a header row.

Note that this is a list of attributes because it actually sets two HTML
attributes at once.

-}
selected :: Array (IProp r i)
selected =
    [ dataTableRowSelectedCs
    , Html.Attributes.attribute "aria-selected" "true"
    ]


dataTableRowSelectedCs :: Html.Attribute r i
dataTableRowSelectedCs =
    class "mdc-data-table__row--selected"


headerRow :: Row r i -> Html r i
headerRow (Row { attributes, nodes }) =
    Html.tr (dataTableHeaderRowCs :: attributes) (Array.map headerCell nodes)


dataTableHeaderRowCs :: Html.Attribute r i
dataTableHeaderRowCs =
    class "mdc-data-table__header-row"


bodyRow :: Row r i -> Html r i
bodyRow (Row { attributes, nodes }) =
    Html.tr (dataTableRowCs :: attributes) (Array.map bodyCell nodes)


dataTableRowCs :: Html.Attribute r i
dataTableRowCs =
    class "mdc-data-table__row"


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
                                        class "mdc-data-table__row-checkbox"
                                            :: config__.additionalAttributes
                                }
                    )
                ]


dataTableHeaderCellCs :: Maybe (Html.Attribute r i)
dataTableHeaderCellCs =
    Just (class "mdc-data-table__header-cell")


columnHeaderRoleAttr :: Maybe (Html.Attribute r i)
columnHeaderRoleAttr =
    Just (Html.Attributes.attribute "role" "columnheader")


colScopeAttr :: Maybe (Html.Attribute r i)
colScopeAttr =
    Just (Html.Attributes.attribute "scope" "col")


dataTableHeaderCellNumericCs :: Boolean -> Maybe (Html.Attribute r i)
dataTableHeaderCellNumericCs numeric =
    if numeric then
        Just (class "mdc-data-table__header-cell--numeric")

    else
        Nothing


dataTableHeaderCellCheckboxCs :: Maybe (Html.Attribute r i)
dataTableHeaderCellCheckboxCs =
    Just (class "mdc-data-table__header-cell--checkbox")


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
                                        class "mdc-data-table__row-checkbox"
                                            :: config__.additionalAttributes
                                }
                    )
                ]


{-| Cell type
-}
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


{-| Data table cell
-}
cell :: Array (IProp r i) -> Array (Html r i) -> Cell r i
cell attributes nodes =
    Cell { numeric = False, attributes = attributes, nodes = nodes }


{-| Numeric data table cell (right-aligned contents)
-}
numericCell :: Array (IProp r i) -> Array (Html r i) -> Cell r i
numericCell attributes nodes =
    Cell { numeric = True, attributes = attributes, nodes = nodes }


{-| Data table cell that contians a checkbox
-}
checkboxCell :: Array (IProp r i) -> Checkbox.Config r i -> Cell r i
checkboxCell attributes config_ =
    CheckboxCell { attributes = attributes, config_ = config_ }


dataTableCellCs :: Maybe (Html.Attribute r i)
dataTableCellCs =
    Just (class "mdc-data-table__cell")


dataTableCellNumericCs :: Boolean -> Maybe (Html.Attribute r i)
dataTableCellNumericCs numeric =
    if numeric then
        Just (class "mdc-data-table__cell--numeric")

    else
        Nothing


dataTableCellCheckboxCs :: Maybe (Html.Attribute r i)
dataTableCellCheckboxCs =
    Just (class "mdc-data-table__cell--checkbox")
