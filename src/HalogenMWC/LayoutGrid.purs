module HalogenMWC.LayoutGrid where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

layoutGrid :: Array (IProp r i) -> Array (Html r i) -> Html r i
layoutGrid attributes nodes =
    HH.node "mdc-layout-grid"
        (HP.class_ mdc_layout_grid :: style "display" "block" :: attributes)
        nodes

cell :: Array (IProp r i) -> Array (Html r i) -> Html r i
cell attributes nodes =
    HH.div (HP.class_ mdc_layout_grid__cell :: attributes) nodes

inner :: Array (IProp r i) -> Array (Html r i) -> Html r i
inner attributes nodes =
    HH.div (HP.class_ mdc_layout_grid__inner :: attributes) nodes

alignBottom :: HH.Attribute r i
alignBottom =
    HP.class_ "mdc-layout-grid__cell--align-bottom"

alignLeft :: HH.Attribute r i
alignLeft =
    HP.class_ "mdc-layout-grid--align-left"

alignRight :: HH.Attribute r i
alignRight =
    HP.class_ "mdc-layout-grid--align-right"

alignMiddle :: HH.Attribute r i
alignMiddle =
    HP.class_ "mdc-layout-grid__cell--align-middle"

alignTop :: HH.Attribute r i
alignTop =
    HP.class_ "mdc-layout-grid__cell--align-top"

span :: Int -> HH.Attribute r i
span n =
    HP.class_ ("mdc-layout-grid__cell--span-" <> String.fromInt n)

spanDesktop :: Int -> HH.Attribute r i
spanDesktop n =
    HP.class_ ("mdc-layout-grid__cell--span-" <> String.fromInt n <> "-desktop")

spanTablet :: Int -> HH.Attribute r i
spanTablet n =
    HP.class_ ("mdc-layout-grid__cell--span-" <> String.fromInt n <> "-tablet")

spanPhone :: Int -> HH.Attribute r i
spanPhone n =
    HP.class_ ("mdc-layout-grid__cell--span-" <> String.fromInt n <> "-phone")

span1 :: HH.Attribute r i
span1 =
    span 1

span2 :: HH.Attribute r i
span2 =
    span 2

span3 :: HH.Attribute r i
span3 =
    span 3

span4 :: HH.Attribute r i
span4 =
    span 4

span5 :: HH.Attribute r i
span5 =
    span 5

span6 :: HH.Attribute r i
span6 =
    span 6

span7 :: HH.Attribute r i
span7 =
    span 7

span8 :: HH.Attribute r i
span8 =
    span 8

span9 :: HH.Attribute r i
span9 =
    span 9

span10 :: HH.Attribute r i
span10 =
    span 10

span11 :: HH.Attribute r i
span11 =
    span 11

span12 :: HH.Attribute r i
span12 =
    span 12

{-| Change a cell to span one column (desktop only)
-}
span1Desktop :: HH.Attribute r i
span1Desktop =
    spanDesktop 1

{-| Change a cell to span two columns (desktop only)
-}
span2Desktop :: HH.Attribute r i
span2Desktop =
    spanDesktop 2

{-| Change a cell to span three columns (desktop only)
-}
span3Desktop :: HH.Attribute r i
span3Desktop =
    spanDesktop 3

{-| Change a cell to span four columns (desktop only)
-}
span4Desktop :: HH.Attribute r i
span4Desktop =
    spanDesktop 4

{-| Change a cell to span five columns (desktop only)
-}
span5Desktop :: HH.Attribute r i
span5Desktop =
    spanDesktop 5

{-| Change a cell to span six columns (desktop only)
-}
span6Desktop :: HH.Attribute r i
span6Desktop =
    spanDesktop 6

{-| Change a cell to span seven columns (desktop only)
-}
span7Desktop :: HH.Attribute r i
span7Desktop =
    spanDesktop 7

{-| Change a cell to span eight columns (desktop only)
-}
span8Desktop :: HH.Attribute r i
span8Desktop =
    spanDesktop 8

{-| Change a cell to span nine columns (desktop only)
-}
span9Desktop :: HH.Attribute r i
span9Desktop =
    spanDesktop 9

{-| Change a cell to span ten columns (desktop only)
-}
span10Desktop :: HH.Attribute r i
span10Desktop =
    spanDesktop 10

{-| Change a cell to span eleven columns (desktop only)
-}
span11Desktop :: HH.Attribute r i
span11Desktop =
    spanDesktop 11

{-| Change a cell to span twelve columns (desktop only)
-}
span12Desktop :: HH.Attribute r i
span12Desktop =
    spanDesktop 12

{-| Change a cell to span one column (tablet only)
-}
span1Tablet :: HH.Attribute r i
span1Tablet =
    spanTablet 1

{-| Change a cell to span two columns (tablet only)
-}
span2Tablet :: HH.Attribute r i
span2Tablet =
    spanTablet 2

{-| Change a cell to span three columns (tablet only)
-}
span3Tablet :: HH.Attribute r i
span3Tablet =
    spanTablet 3

{-| Change a cell to span four columns (tablet only)
-}
span4Tablet :: HH.Attribute r i
span4Tablet =
    spanTablet 4

{-| Change a cell to span five columns (tablet only)
-}
span5Tablet :: HH.Attribute r i
span5Tablet =
    spanTablet 5

{-| Change a cell to span six columns (tablet only)
-}
span6Tablet :: HH.Attribute r i
span6Tablet =
    spanTablet 6

{-| Change a cell to span seven columns (tablet only)
-}
span7Tablet :: HH.Attribute r i
span7Tablet =
    spanTablet 7

{-| Change a cell to span eight columns (tablet only)
-}
span8Tablet :: HH.Attribute r i
span8Tablet =
    spanTablet 8

{-| Change a cell to span one column (phone only)
-}
span1Phone :: HH.Attribute r i
span1Phone =
    spanPhone 1

{-| Change a cell to span two columns (phone only)
-}
span2Phone :: HH.Attribute r i
span2Phone =
    spanPhone 2

{-| Change a cell to span three columns (phone only)
-}
span3Phone :: HH.Attribute r i
span3Phone =
    spanPhone 3

{-| Change a cell to span four columns (phone only)
-}
span4Phone :: HH.Attribute r i
span4Phone =
    spanPhone 4
