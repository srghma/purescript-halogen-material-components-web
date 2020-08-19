module HalogenMWC.LinearProgress
    ( Config, config



    , indeterminate
    , determinate
    , buffered
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA





type Config r i
    =
        { reverse :: Boolean
        , closed :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


data Variant
    = Indeterminate
    | Determinate Float
    | Buffered Float Float



config :: Config r i
config =
    Config
        { reverse = False
        , closed = False
        , additionalAttributes = []
        }

















linearProgress :: Variant -> Config r i -> Html r i
linearProgress variant ({ additionalAttributes } as config_) =
    HH.node "mdc-linear-progress"
        (Array.filterMap identity
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



indeterminate :: Config r i -> Html r i
indeterminate config_ =
    linearProgress Indeterminate config_



determinate :: Config r i -> { progress :: Float } -> Html r i
determinate config_ { progress } =
    linearProgress (Determinate progress) config_



buffered :: Config r i -> { progress :: Float, buffered :: Float } -> Html r i
buffered config_ data =
    linearProgress (Buffered data.progress data.buffered) config_


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_linear_progress)


displayCss :: Maybe (HH.Attribute r i)
displayCss =
    Just (style "display" "block")


roleAttr :: Maybe (HH.Attribute r i)
roleAttr =
    Just (HH.Attributes.attribute "role" "progressbar")


variantCs :: Variant -> Maybe (HH.Attribute r i)
variantCs variant =
    case variant of
        Indeterminate ->
            Just (HP.class_ mdc_linear_progress____indeterminate)

        _ ->
            Nothing


determinateProp :: Variant -> Maybe (HH.Attribute r i)
determinateProp variant =
    Just (HH.Attributes.property "determinate" (Encode.bool (variant /= Indeterminate)))


progressProp :: Variant -> Maybe (HH.Attribute r i)
progressProp variant =
    Just
        (HH.Attributes.property "progress"
            (Encode.float
                (case variant of
                    Determinate progress ->
                        progress

                    Buffered progress _ ->
                        progress

                    _ ->
                        0
                )
            )
        )


bufferProp :: Variant -> Maybe (HH.Attribute r i)
bufferProp variant =
    Just
        (HH.Attributes.property "buffer"
            (Encode.float
                (case variant of
                    Buffered _ buffer ->
                        buffer

                    _ ->
                        0
                )
            )
        )


reverseProp :: Config r i -> Maybe (HH.Attribute r i)
reverseProp { reverse } =
    Just (HH.Attributes.property "reverse" (Encode.bool reverse))


closedProp :: Config r i -> Maybe (HH.Attribute r i)
closedProp { closed } =
    Just (HH.Attributes.property "closed" (Encode.bool closed))


bufferingDotsElt :: Html r i
bufferingDotsElt =
    HH.div [ HP.class_ mdc_linear_progress__buffering_dots ] []


bufferElt :: Html r i
bufferElt =
    HH.div [ HP.class_ mdc_linear_progress__buffer ] []


primaryBarElt :: Html r i
primaryBarElt =
    HH.div [ HP.class_ "mdc-linear-progress__bar mdc-linear-progress__primary-bar" ]
        [ barInnerElt ]


secondaryBarElt :: Html r i
secondaryBarElt =
    HH.div [ HP.class_ "mdc-linear-progress__bar mdc-linear-progress__secondary-bar" ]
        [ barInnerElt ]


barInnerElt :: Html r i
barInnerElt =
    HH.div [ HP.class_ mdc_linear_progress__bar_inner ] []
