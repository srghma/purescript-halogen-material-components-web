module Demo.Pages.LayoutGrid where

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils
import Halogen
import Material.Classes.LayoutGrid
import Material.Classes.Typography
import Protolude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.LayoutGrid as LayoutGrid

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
        [ demoGrid
          { additionalClasses: [], additionalAttributes: [] }
          [ HH.div
            [ HP.class_ mdc_layout_grid__inner ]
            (Array.replicate 3 (demoCell { additionalClasses: [], additionalAttributes: [] }))
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

demoGrid :: forall w i . { additionalClasses :: Array ClassName, additionalAttributes :: Array (IProp I.HTMLdiv i) } -> Array (HH.HTML w i) -> HH.HTML w i
demoGrid options =
  HH.div
  ( [ HP.classes $ [ mdc_layout_grid ] <> options.additionalClasses
    , HP.style "background: rgba(0,0,0,.2); min-width: 360px;"
    ]
    <> options.additionalAttributes
  )

demoCell :: forall w i . { additionalClasses :: Array ClassName, additionalAttributes :: Array (IProp I.HTMLdiv i) } -> HH.HTML w i
demoCell options =
  HH.div
  ( [ HP.classes $ [ mdc_layout_grid__cell ] <> options.additionalClasses
    , HP.style "background: rgba(0,0,0,.2); height: 100px;"
    ]
    <> options.additionalAttributes)
  []

columnsGrid :: forall w i . HH.HTML w i
columnsGrid =
    demoGrid { additionalClasses: [], additionalAttributes: [] }
        [ HH.div [ HP.class_ mdc_layout_grid__inner ]
            [ demoCell { additionalClasses: [ mdc_layout_grid__cell____span_6 ], additionalAttributes: [] }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_3 ], additionalAttributes: [] }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_2 ], additionalAttributes: [] }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_1 ], additionalAttributes: [] }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_3 ], additionalAttributes: [] }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_1 ], additionalAttributes: [] }
            , demoCell { additionalClasses: [ mdc_layout_grid__cell____span_8 ], additionalAttributes: [] }
            ]
        ]

leftAlignedGrid :: forall w i . HH.HTML w i
leftAlignedGrid =
    demoGrid
      { additionalClasses: [ mdc_layout_grid____align_left ], additionalAttributes: [ HP.style "max-width: 800px;" ] }
      [ HH.div [ HP.class_ mdc_layout_grid__inner ]
          [ demoCell { additionalClasses: [], additionalAttributes: [] }
          , demoCell { additionalClasses: [], additionalAttributes: [] }
          , demoCell { additionalClasses: [], additionalAttributes: [] }
          ]
      ]

rightAlignedGrid :: forall w i . HH.HTML w i
rightAlignedGrid =
    demoGrid
      { additionalClasses: [ mdc_layout_grid____align_right ], additionalAttributes: [ HP.style "max-width: 800px;" ] }
      [ HH.div [ HP.class_ mdc_layout_grid__inner ] (Array.replicate 3 (demoCell { additionalClasses: [], additionalAttributes: [] }))
      ]

cellAlignmentGrid :: forall w i . HH.HTML w i
cellAlignmentGrid =
    let
        innerHeight = HP.style "min-height: 200px;"
        cellHeight = HP.style "max-height: 50px;"
    in
    demoGrid
      { additionalClasses: [], additionalAttributes: [ HP.style "min-height: 200px;" ] }
      [ HH.div
        [ HP.class_ mdc_layout_grid__inner
        , innerHeight
        ]
        [ demoCell { additionalClasses: [ mdc_layout_grid__cell____align_top ], additionalAttributes: [ cellHeight ] }
        , demoCell { additionalClasses: [ mdc_layout_grid__cell____align_middle ], additionalAttributes: [ cellHeight ] }
        , demoCell { additionalClasses: [ mdc_layout_grid__cell____align_bottom ], additionalAttributes: [ cellHeight ] }
        ]
      ]
