module Demo.Pages.LayoutGrid where

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils (mkComponentStatic)
import Halogen (ClassName)
import Material.Classes.LayoutGrid (mdc_layout_grid, mdc_layout_grid____align_left, mdc_layout_grid____align_right, mdc_layout_grid__cell, mdc_layout_grid__cell____align_bottom, mdc_layout_grid__cell____align_middle, mdc_layout_grid__cell____align_top, mdc_layout_grid__cell____span_1, mdc_layout_grid__cell____span_2, mdc_layout_grid__cell____span_3, mdc_layout_grid__cell____span_6, mdc_layout_grid__cell____span_8, mdc_layout_grid__inner)
import Material.Classes.Typography (mdc_typography____body1, mdc_typography____subtitle1)
import Protolude

import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

config :: CatalogPage
config =
    { title: "Layout Grid"
    , prelude: "Material design’s responsive UI is based on a 12-column grid layout."
    , resources:
        { materialDesignGuidelines: Nothing
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-LayoutGrid"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-layout-grid"
        }
    , hero: mkComponentStatic $ HH.div_
        [ demoGrid
          { additionalClasses: [], additionalStyle: "" }
          [ HH.div
            [ HP.class_ mdc_layout_grid__inner ]
            (Array.replicate 3 (demoCell { additionalClasses: [], additionalStyle: "" }))
          ]
        ]
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

demoGrid :: forall w i . { additionalClasses :: Array ClassName, additionalStyle :: String } -> Array (HH.HTML w i) -> HH.HTML w i
demoGrid options =
  HH.div
  ( [ HP.classes $ [ mdc_layout_grid ] <> options.additionalClasses
    , HP.style $ "background: rgba(0,0,0,.2); min-width: 360px;" <> options.additionalStyle
    ]
  )

demoCell :: forall w i . { additionalClasses :: Array ClassName, additionalStyle :: String } -> HH.HTML w i
demoCell options =
  HH.div
  ( [ HP.classes $ [ mdc_layout_grid__cell ] <> options.additionalClasses
    , HP.style $ "background: rgba(0,0,0,.2); height: 100px;" <> options.additionalStyle
    ]
  )
  []

columnsGrid :: forall w i . HH.HTML w i
columnsGrid =
    demoGrid { additionalClasses: [], additionalStyle: "" }
        [ HH.div [ HP.class_ mdc_layout_grid__inner ]
            [ demoCell { additionalClasses: [ mdc_layout_grid__cell____span_6 ], additionalStyle: "" }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_3 ], additionalStyle: "" }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_2 ], additionalStyle: "" }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_1 ], additionalStyle: "" }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_3 ], additionalStyle: "" }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_1 ], additionalStyle: "" }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_8 ], additionalStyle: "" }
            ]
        ]

leftAlignedGrid :: forall w i . HH.HTML w i
leftAlignedGrid =
    demoGrid
      { additionalClasses: [ mdc_layout_grid____align_left ], additionalStyle: "max-width: 800px;" }
      [ HH.div [ HP.class_ mdc_layout_grid__inner ]
          [ demoCell { additionalClasses: [], additionalStyle: "" }
          , demoCell { additionalClasses: [], additionalStyle: "" }
          , demoCell { additionalClasses: [], additionalStyle: "" }
          ]
      ]

rightAlignedGrid :: forall w i . HH.HTML w i
rightAlignedGrid =
    demoGrid
      { additionalClasses: [ mdc_layout_grid____align_right ], additionalStyle: "max-width: 800px;" }
      [ HH.div [ HP.class_ mdc_layout_grid__inner ] (Array.replicate 3 (demoCell { additionalClasses: [], additionalStyle: "" }))
      ]

cellAlignmentGrid :: forall w i . HH.HTML w i
cellAlignmentGrid =
    let
        innerHeight = "min-height: 200px;"
        cellHeight = "max-height: 50px;"
    in
    demoGrid
      { additionalClasses: [], additionalStyle: "min-height: 200px;" }
      [ HH.div
        [ HP.class_ mdc_layout_grid__inner
        , HP.style innerHeight
        ]
        [ demoCell { additionalClasses: [ mdc_layout_grid__cell____align_top ], additionalStyle: cellHeight }
        , demoCell { additionalClasses: [ mdc_layout_grid__cell____align_middle ], additionalStyle: cellHeight }
        , demoCell { additionalClasses: [ mdc_layout_grid__cell____align_bottom ], additionalStyle: cellHeight }
        ]
      ]
