module HalogenMWC.Elevation
    ( z0, z1, z2, z3, z4, z5, z6, z7, z8
    , z9, z10, z11, z12, z13, z14, z15, z16
    , z17, z18, z19, z20, z21, z22, z23, z24
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA



{-| 0dp elevation (no elevation)
-}
z0 :: HH.Attribute r i
z0 =
    z 0



z1 :: HH.Attribute r i
z1 =
    z 1



z2 :: HH.Attribute r i
z2 =
    z 2



z3 :: HH.Attribute r i
z3 =
    z 3



z4 :: HH.Attribute r i
z4 =
    z 4



z5 :: HH.Attribute r i
z5 =
    z 5



z6 :: HH.Attribute r i
z6 =
    z 6



z7 :: HH.Attribute r i
z7 =
    z 7



z8 :: HH.Attribute r i
z8 =
    z 8



z9 :: HH.Attribute r i
z9 =
    z 9



z10 :: HH.Attribute r i
z10 =
    z 10



z11 :: HH.Attribute r i
z11 =
    z 11



z12 :: HH.Attribute r i
z12 =
    z 12



z13 :: HH.Attribute r i
z13 =
    z 13



z14 :: HH.Attribute r i
z14 =
    z 14



z15 :: HH.Attribute r i
z15 =
    z 15



z16 :: HH.Attribute r i
z16 =
    z 16



z17 :: HH.Attribute r i
z17 =
    z 17



z18 :: HH.Attribute r i
z18 =
    z 18



z19 :: HH.Attribute r i
z19 =
    z 19



z20 :: HH.Attribute r i
z20 =
    z 20



z21 :: HH.Attribute r i
z21 =
    z 21



z22 :: HH.Attribute r i
z22 =
    z 22



z23 :: HH.Attribute r i
z23 =
    z 23



z24 :: HH.Attribute r i
z24 =
    z 24


z :: Int -> HH.Attribute r i
z n =
    HP.class_ ("mdc-elevation--z" ++ String.fromInt n)
