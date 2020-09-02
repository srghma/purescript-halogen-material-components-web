module Demo.Pages.LayoutGrid where

import Demo.HOC.CatalogPage
import Demo.Utils
import Halogen
import Material.Classes.Typography
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.LayoutGrid as LayoutGrid
import Material.Classes.LayoutGrid

catalogPage :: CatalogPage
catalogPage =
    { title: "Layout Grid"
    , prelude: "Material design’s responsive UI is based on a 12-column grid layout."
    , resources:
        { materialDesignGuidelines: Nothing
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-LayoutGrid"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-layout-grid"
        }
    , hero: mkComponentStatic $ HH.div_
        [ demoGrid [] [ HH.div [ HP.class_ mdc_layout_grid__inner ] (Array.replicate 3 (demoCell [])) ] ]
    , content: mkComponentStatic $ HH.div_
        [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Columns" ]
        , columnsGrid
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Grid Left Alignment" ]
        , HH.p
            [ HP.class_ mdc_typography____body1 ]
            [ HH.text "This requires a max-width on the top-level grid element." ]
        , leftAlignedGrid
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Grid Right Alignment" ]
        , HH.p
            [ HP.class_ mdc_typography____body1 ]
            [ HH.text "This requires a max-width on the top-level grid element." ]
        , rightAlignedGrid
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Cell Alignment" ]
        , HH.p
            [ HP.class_ mdc_typography____body1 ]
            [ HH.text "Cell alignment requires a cell height smaller than the inner height of the grid." ]
        , cellAlignmentGrid
        ]
    }

demoGrid :: forall r w i . Array (IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
demoGrid options =
  HH.div
  ([ HP.class_ mdc_layout_grid, HP.style "background: rgba(0,0,0,.2); min-width: 360px;" ] <> options)

demoCell :: forall r w i . Array (IProp r i) -> HH.HTML w i
demoCell options =
  HH.div
  ([ HP.class_ mdc_layout_grid__cell, HP.style "background: rgba(0,0,0,.2); height: 100px;" ] <> options)
  []

columnsGrid :: forall w i . HH.HTML w i
columnsGrid =
    demoGrid []
        [ HH.div [ HP.class_ mdc_layout_grid__inner ]
            [ demoCell [ mdc_layout_grid__cell____span_6 ]
            , demoCell [ mdc_layout_grid__cell____span_3 ]
            , demoCell [ mdc_layout_grid__cell____span_2 ]
            , demoCell [ mdc_layout_grid__cell____span_1 ]
            , demoCell [ mdc_layout_grid__cell____span_3 ]
            , demoCell [ mdc_layout_grid__cell____span_1 ]
            , demoCell [ mdc_layout_grid__cell____span_8 ]
            ]
        ]

leftAlignedGrid :: forall w i . HH.HTML w i
leftAlignedGrid =
    demoGrid
        [ HP.class_ mdc_layout_grid____align_left
        , HP.style "max-width: 800px;"
        ]
        [ HH.div [ HP.class_ mdc_layout_grid__inner ]
            [ demoCell []
            , demoCell []
            , demoCell []
            ]
        ]

rightAlignedGrid :: forall w i . HH.HTML w i
rightAlignedGrid =
    demoGrid
        [ HP.class_ mdc_layout_grid____align_right
        , HP.style "max-width: 800px;"
        ]
        [ HH.div [ HP.class_ mdc_layout_grid__inner ] (Array.replicate 3 (demoCell []))
        ]

cellAlignmentGrid :: forall w i . HH.HTML w i
cellAlignmentGrid =
    let
        innerHeight = HP.style "min-height: 200px;"
        cellHeight = HP.style "max-height: 50px;"
    in
    demoGrid
        [ HP.style "min-height: 200px;"
        ]
        [ HH.div [ HP.class_ mdc_layout_grid__inner ] innerHeight
            [ demoCell [ HP.class_ mdc_layout_grid__cell____align_top, cellHeight ]
            , demoCell [ HP.class_ mdc_layout_grid__cell____align_middle, cellHeight ]
            , demoCell [ HP.class_ mdc_layout_grid__cell____align_bottom, cellHeight ]
            ]
        ]
