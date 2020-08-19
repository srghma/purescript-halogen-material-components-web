module HalogenMWC.LinearProgress where

import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.LinearProgress

type Config r i
  = { reverse :: Boolean
    , closed :: Boolean
    , additionalAttributes :: Array (IProp r i)
    }

data Variant
  = Indeterminate
  | Determinate Float
  | Buffered Float Float

defaultConfig :: Config r i
defaultConfig =
  { reverse: false
  , closed: false
  , additionalAttributes: []
  }

linearProgress :: Variant -> Config r i -> HH.HTML w i
linearProgress variant (config_@{ additionalAttributes }) =
  HH.element "mdc-linear-progress"
    ( Array.catMaybes
        [ rootCs
        , displayCss
        , roleAttr
        , variantCs variant
        , determinateProp variant
        , progressProp variant
        , bufferProp variant
        , reverseProp config_
        , closedProp config_
        ]
        <> additionalAttributes
    )
    [ bufferingDotsElt
    , bufferElt
    , primaryBarElt
    , secondaryBarElt
    ]

indeterminate :: Config r i -> HH.HTML w i
indeterminate config_ = linearProgress Indeterminate config_

determinate :: Config r i -> { progress :: Float } -> HH.HTML w i
determinate config_ { progress } = linearProgress (Determinate progress) config_

buffered :: Config r i -> { progress :: Float, buffered :: Float } -> HH.HTML w i
buffered config_ data_ = linearProgress (Buffered data_.progress data_.buffered) config_

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_linear_progress)

displayCss :: Maybe (IProp r i)
displayCss = Just (style "display" "block")

roleAttr :: Maybe (IProp r i)
roleAttr = Just (HP.attr "role" "progressbar")

variantCs :: Variant -> Maybe (IProp r i)
variantCs variant = case variant of
  Indeterminate -> Just (HP.class_ mdc_linear_progress____indeterminate)
  _ -> Nothing

determinateProp :: Variant -> Maybe (IProp r i)
determinateProp variant = Just (HP.prop "determinate" (Encode.bool (variant /= Indeterminate)))

progressProp :: Variant -> Maybe (IProp r i)
progressProp variant =
  Just
    ( HP.prop "progress"
        ( Encode.float
            ( case variant of
                Determinate progress -> progress
                Buffered progress _ -> progress
                _ -> 0
            )
        )
    )

bufferProp :: Variant -> Maybe (IProp r i)
bufferProp variant =
  Just
    ( HP.prop "buffer"
        ( Encode.float
            ( case variant of
                Buffered _ buffer -> buffer
                _ -> 0
            )
        )
    )

reverseProp :: Config r i -> Maybe (IProp r i)
reverseProp { reverse } = Just (HP.prop "reverse" reverse)

closedProp :: Config r i -> Maybe (IProp r i)
closedProp { closed } = Just (HP.prop "closed" closed)

bufferingDotsElt :: HH.HTML w i
bufferingDotsElt = HH.div [ HP.class_ mdc_linear_progress__buffering_dots ] []

bufferElt :: HH.HTML w i
bufferElt = HH.div [ HP.class_ mdc_linear_progress__buffer ] []

primaryBarElt :: HH.HTML w i
primaryBarElt =
  HH.div [ HP.class_ "mdc-linear-progress__bar mdc-linear-progress__primary-bar" ]
    [ barInnerElt ]

secondaryBarElt :: HH.HTML w i
secondaryBarElt =
  HH.div [ HP.class_ "mdc-linear-progress__bar mdc-linear-progress__secondary-bar" ]
    [ barInnerElt ]

barInnerElt :: HH.HTML w i
barInnerElt = HH.div [ HP.class_ mdc_linear_progress__bar_inner ] []
