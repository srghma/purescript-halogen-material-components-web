module HalogenMWC.LayoutGrid
    ( layoutGrid, cell, inner
    , span1, span2, span3, span4, span5, span6, span7, span8, span9, span10, span11, span12
    , alignTop, alignMiddle, alignBottom
    , alignLeft, alignRight
    , span1Desktop, span2Desktop, span3Desktop, span4Desktop, span5Desktop, span6Desktop, span7Desktop, span8Desktop, span9Desktop, span10Desktop, span11Desktop, span12Desktop
    , span1Tablet, span2Tablet, span3Tablet, span4Tablet, span5Tablet, span6Tablet, span7Tablet, span8Tablet
    , span1Phone, span2Phone, span3Phone, span4Phone
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




layoutGrid :: Array (IProp r i) -> Array (Html r i) -> Html r i
layoutGrid attributes nodes =
    Html.node "mdc-layout-grid"
        (HP.class_ "mdc-layout-grid" :: style "display" "block" :: attributes)
        nodes



cell :: Array (IProp r i) -> Array (Html r i) -> Html r i
cell attributes nodes =
    Html.div (HP.class_ "mdc-layout-grid__cell" :: attributes) nodes



inner :: Array (IProp r i) -> Array (Html r i) -> Html r i
inner attributes nodes =
    Html.div (HP.class_ "mdc-layout-grid__inner" :: attributes) nodes



alignBottom :: Html.Attribute r i
alignBottom =
    HP.class_ "mdc-layout-grid__cell--align-bottom"



alignLeft :: Html.Attribute r i
alignLeft =
    HP.class_ "mdc-layout-grid--align-left"



alignRight :: Html.Attribute r i
alignRight =
    HP.class_ "mdc-layout-grid--align-right"



alignMiddle :: Html.Attribute r i
alignMiddle =
    HP.class_ "mdc-layout-grid__cell--align-middle"



alignTop :: Html.Attribute r i
alignTop =
    HP.class_ "mdc-layout-grid__cell--align-top"


span :: Int -> Html.Attribute r i
span n =
    HP.class_ ("mdc-layout-grid__cell--span-" ++ String.fromInt n)


spanDesktop :: Int -> Html.Attribute r i
spanDesktop n =
    HP.class_ ("mdc-layout-grid__cell--span-" ++ String.fromInt n ++ "-desktop")


spanTablet :: Int -> Html.Attribute r i
spanTablet n =
    HP.class_ ("mdc-layout-grid__cell--span-" ++ String.fromInt n ++ "-tablet")


spanPhone :: Int -> Html.Attribute r i
spanPhone n =
    HP.class_ ("mdc-layout-grid__cell--span-" ++ String.fromInt n ++ "-phone")



span1 :: Html.Attribute r i
span1 =
    span 1



span2 :: Html.Attribute r i
span2 =
    span 2



span3 :: Html.Attribute r i
span3 =
    span 3



span4 :: Html.Attribute r i
span4 =
    span 4



span5 :: Html.Attribute r i
span5 =
    span 5



span6 :: Html.Attribute r i
span6 =
    span 6



span7 :: Html.Attribute r i
span7 =
    span 7



span8 :: Html.Attribute r i
span8 =
    span 8



span9 :: Html.Attribute r i
span9 =
    span 9



span10 :: Html.Attribute r i
span10 =
    span 10



span11 :: Html.Attribute r i
span11 =
    span 11



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
