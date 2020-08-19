module HalogenMWC.LayoutGrid where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

layoutGrid :: Array (IProp r i) -> Array (Html r i) -> Html r i
layoutGrid attributes nodes =
  HH.element "mdc-layout-grid"
    ([ HP.class_ mdc_layout_grid, HP.attr (AttrName "style") "display" "block" ] <> attributes)
    nodes

cell :: Array (IProp r i) -> Array (Html r i) -> Html r i
cell attributes nodes = HH.div ([ HP.class_ mdc_layout_grid__cell ] <> attributes) nodes

inner :: Array (IProp r i) -> Array (Html r i) -> Html r i
inner attributes nodes = HH.div ([ HP.class_ mdc_layout_grid__inner ] <> attributes) nodes
