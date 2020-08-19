module HalogenMWC.LayoutGrid
    ( layoutGrid, cell, inner
    , span1, span2, span3, span4, span5, span6, span7, span8, span9, span10, span11, span12
    , alignTop, alignMiddle, alignBottom
    , alignLeft, alignRight
    , span1Desktop, span2Desktop, span3Desktop, span4Desktop, span5Desktop, span6Desktop, span7Desktop, span8Desktop, span9Desktop, span10Desktop, span11Desktop, span12Desktop
    , span1Tablet, span2Tablet, span3Tablet, span4Tablet, span5Tablet, span6Tablet, span7Tablet, span8Tablet
    , span1Phone, span2Phone, span3Phone, span4Phone
    ) where

{-| Material Design’s responsive user interface is based on a _column-variate_
grid layout: it has 12 columns on desktop, 8 columns on tablet and 4 columns on
phone.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Layout Grid](#layout-grid)
  - [Cell Spans](#cell-spans)
  - [Cell Alignment](#cell-alignment)
  - [Grid Alignment](#grid-alignment)
  - [Nested Grid](#nested-grid)
  - [Device-Specific Cell Spans](#device-specific-cell-spans)
      - [Desktop Cell Spans](#desktop-cell-spans)
      - [Tablet Cell Spans](#tablet-cell-spans)
      - [Phone Cell Spans](#phone-cell-spans)


# Resources

  - [Demo: Layout Grids](https://aforemny.github.io/material-components-web-elm/#layout-grid)
  - [Material Design Guidelines: Layout Grid](https://material.io/guidelines/layout/responsive-ui.html#responsive-ui-grid)
  - [MDC Web: Layout Grid](https://github.com/material-components/material-components-web/tree/master/packages/mdc-layout-grid)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-layout-grid#sass-mixins)


# Basic Usage

Note that you are expected to wrap cells in `LayoutGrid.inner`. This has to do
with nesting layout grids, but it is required for flat layout grids as
well.

    import Material.LayoutGrid as LayoutGrid

    main =
        LayoutGrid.layoutGrid []
            [ LayoutGrid.inner []
                [ LayoutGrid.cell [] []
                , LayoutGrid.cell [] []
                , LayoutGrid.cell [] []
                ]
            ]


# Layout Grid

@docs layoutGrid, cell, inner


# Cell Spans

Cells span by default four columns within the grid. Use one of the following
functions to make a cell span more or fewer columns.

@docs span1, span2, span3, span4, span5, span6, span7, span8, span9, span10, span11, span12


# Cell Alignment

Cells are by default stretched. You can add attributes `alignTop`,
`alignMiddle` or `alignBottom` to a cell to make them not extend beyond their
content and instead specify an alignment.

@docs alignTop, alignMiddle, alignBottom


# Grid Alignment

The layout grid is by default center aligned. You can add attributes
`alignLeft` or `alignRight` to the `layoutGrid` to change this behavior.

Note that effects will only be visible if the layout grid does not span the
entire available width.

@docs alignLeft, alignRight


# Nested Grid

When your contents need extra structure that cannot be supported by a single
layout grid, you can nest layout grids within each other. To nest layout grid,
add a new `inner` around nested `cell`s within an existing `cell`.

The nested layout grid behaves exactly like when they are not nested, e.g, they
have 12 columns on desktop, 8 columns on tablet and 4 columns on phone. They
also use the same gutter size as their parents, but margins are not
re-introduced since they are living within another cell.

However, the Material Design guidelines do not recommend having a deeply nested
grid as it might mean an over complicated user experience.

    LayoutGrid.layoutGrid []
        [ LayoutGrid.inner []
            [ LayoutGrid.cell []
                [ LayoutGrid.inner []
                    [ LayoutGrid.cell [] []
                    , LayoutGrid.cell [] []
                    ]
                ]
            , LayoutGrid.cell [] []
            ]
        ]


# Device-Specific Cell Spans


## Desktop Cell Spans

@docs span1Desktop, span2Desktop, span3Desktop, span4Desktop, span5Desktop, span6Desktop, span7Desktop, span8Desktop, span9Desktop, span10Desktop, span11Desktop, span12Desktop


## Tablet Cell Spans

@docs span1Tablet, span2Tablet, span3Tablet, span4Tablet, span5Tablet, span6Tablet, span7Tablet, span8Tablet


## Phone Cell Spans

@docs span1Phone, span2Phone, span3Phone, span4Phone

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA



{-| Layout grid view function
-}
layoutGrid :: Array (IProp r i) -> Array (Html r i) -> Html r i
layoutGrid attributes nodes =
    Html.node "mdc-layout-grid"
        (class "mdc-layout-grid" :: style "display" "block" :: attributes)
        nodes


{-| Layout grid cell view function
-}
cell :: Array (IProp r i) -> Array (Html r i) -> Html r i
cell attributes nodes =
    Html.div (class "mdc-layout-grid__cell" :: attributes) nodes


{-| Layout grid inner view function

It is mandatory to wrap `cell`s within `inner`. This has to do with nesting
layout grids, but it is mandatory for flat layout grids as well.

-}
inner :: Array (IProp r i) -> Array (Html r i) -> Html r i
inner attributes nodes =
    Html.div (class "mdc-layout-grid__inner" :: attributes) nodes


{-| Aligns a cell to the bottom
-}
alignBottom :: Html.Attribute r i
alignBottom =
    class "mdc-layout-grid__cell--align-bottom"


{-| Aligns the layout grid to the left
-}
alignLeft :: Html.Attribute r i
alignLeft =
    class "mdc-layout-grid--align-left"


{-| Aligns the layout grid to the right
-}
alignRight :: Html.Attribute r i
alignRight =
    class "mdc-layout-grid--align-right"


{-| Aligns a cell to the middle
-}
alignMiddle :: Html.Attribute r i
alignMiddle =
    class "mdc-layout-grid__cell--align-middle"


{-| Aligns a cell to the top
-}
alignTop :: Html.Attribute r i
alignTop =
    class "mdc-layout-grid__cell--align-top"


span :: Int -> Html.Attribute r i
span n =
    class ("mdc-layout-grid__cell--span-" ++ String.fromInt n)


spanDesktop :: Int -> Html.Attribute r i
spanDesktop n =
    class ("mdc-layout-grid__cell--span-" ++ String.fromInt n ++ "-desktop")


spanTablet :: Int -> Html.Attribute r i
spanTablet n =
    class ("mdc-layout-grid__cell--span-" ++ String.fromInt n ++ "-tablet")


spanPhone :: Int -> Html.Attribute r i
spanPhone n =
    class ("mdc-layout-grid__cell--span-" ++ String.fromInt n ++ "-phone")


{-| Change a cell to span one column
-}
span1 :: Html.Attribute r i
span1 =
    span 1


{-| Change a cell to span two columns
-}
span2 :: Html.Attribute r i
span2 =
    span 2


{-| Change a cell to span three columns
-}
span3 :: Html.Attribute r i
span3 =
    span 3


{-| Change a cell to span four columns
-}
span4 :: Html.Attribute r i
span4 =
    span 4


{-| Change a cell to span five columns
-}
span5 :: Html.Attribute r i
span5 =
    span 5


{-| Change a cell to span six columns
-}
span6 :: Html.Attribute r i
span6 =
    span 6


{-| Change a cell to span seven columns
-}
span7 :: Html.Attribute r i
span7 =
    span 7


{-| Change a cell to span eight columns
-}
span8 :: Html.Attribute r i
span8 =
    span 8


{-| Change a cell to span nine columns
-}
span9 :: Html.Attribute r i
span9 =
    span 9


{-| Change a cell to span ten columns
-}
span10 :: Html.Attribute r i
span10 =
    span 10


{-| Change a cell to span eleven columns
-}
span11 :: Html.Attribute r i
span11 =
    span 11


{-| Change a cell to span twelve columns
-}
span12 :: Html.Attribute r i
span12 =
    span 12


{-| Change a cell to span one column (desktop only)
-}
span1Desktop :: Html.Attribute r i
span1Desktop =
    spanDesktop 1


{-| Change a cell to span two columns (desktop only)
-}
span2Desktop :: Html.Attribute r i
span2Desktop =
    spanDesktop 2


{-| Change a cell to span three columns (desktop only)
-}
span3Desktop :: Html.Attribute r i
span3Desktop =
    spanDesktop 3


{-| Change a cell to span four columns (desktop only)
-}
span4Desktop :: Html.Attribute r i
span4Desktop =
    spanDesktop 4


{-| Change a cell to span five columns (desktop only)
-}
span5Desktop :: Html.Attribute r i
span5Desktop =
    spanDesktop 5


{-| Change a cell to span six columns (desktop only)
-}
span6Desktop :: Html.Attribute r i
span6Desktop =
    spanDesktop 6


{-| Change a cell to span seven columns (desktop only)
-}
span7Desktop :: Html.Attribute r i
span7Desktop =
    spanDesktop 7


{-| Change a cell to span eight columns (desktop only)
-}
span8Desktop :: Html.Attribute r i
span8Desktop =
    spanDesktop 8


{-| Change a cell to span nine columns (desktop only)
-}
span9Desktop :: Html.Attribute r i
span9Desktop =
    spanDesktop 9


{-| Change a cell to span ten columns (desktop only)
-}
span10Desktop :: Html.Attribute r i
span10Desktop =
    spanDesktop 10


{-| Change a cell to span eleven columns (desktop only)
-}
span11Desktop :: Html.Attribute r i
span11Desktop =
    spanDesktop 11


{-| Change a cell to span twelve columns (desktop only)
-}
span12Desktop :: Html.Attribute r i
span12Desktop =
    spanDesktop 12


{-| Change a cell to span one column (tablet only)
-}
span1Tablet :: Html.Attribute r i
span1Tablet =
    spanTablet 1


{-| Change a cell to span two columns (tablet only)
-}
span2Tablet :: Html.Attribute r i
span2Tablet =
    spanTablet 2


{-| Change a cell to span three columns (tablet only)
-}
span3Tablet :: Html.Attribute r i
span3Tablet =
    spanTablet 3


{-| Change a cell to span four columns (tablet only)
-}
span4Tablet :: Html.Attribute r i
span4Tablet =
    spanTablet 4


{-| Change a cell to span five columns (tablet only)
-}
span5Tablet :: Html.Attribute r i
span5Tablet =
    spanTablet 5


{-| Change a cell to span six columns (tablet only)
-}
span6Tablet :: Html.Attribute r i
span6Tablet =
    spanTablet 6


{-| Change a cell to span seven columns (tablet only)
-}
span7Tablet :: Html.Attribute r i
span7Tablet =
    spanTablet 7


{-| Change a cell to span eight columns (tablet only)
-}
span8Tablet :: Html.Attribute r i
span8Tablet =
    spanTablet 8


{-| Change a cell to span one column (phone only)
-}
span1Phone :: Html.Attribute r i
span1Phone =
    spanPhone 1


{-| Change a cell to span two columns (phone only)
-}
span2Phone :: Html.Attribute r i
span2Phone =
    spanPhone 2


{-| Change a cell to span three columns (phone only)
-}
span3Phone :: Html.Attribute r i
span3Phone =
    spanPhone 3


{-| Change a cell to span four columns (phone only)
-}
span4Phone :: Html.Attribute r i
span4Phone =
    spanPhone 4
