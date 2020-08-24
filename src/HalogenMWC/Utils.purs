module HalogenMWC.Utils where


import Halogen (ClassName, PropName)
import Halogen.HTML (IProp)
import Halogen.HTML.Core (Prop(..), PropValue)
import Unsafe.Coerce (unsafeCoerce)

prop :: forall value r i . PropName value -> PropValue -> IProp r i
prop = unsafeCoerce Property

propFromArrayInt :: Array Int -> PropValue
propFromArrayInt = unsafeCoerce

propFromArrayString :: Array String -> PropValue
propFromArrayString = unsafeCoerce

propFromArrayClassName :: Array ClassName -> PropValue
propFromArrayClassName = unsafeCoerce
