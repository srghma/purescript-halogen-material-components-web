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



setClosed :: Boolean -> Config r i -> Config r i
setClosed closed (Config config_) =
    Config { config_ | closed = closed }



setReverse :: Boolean -> Config r i -> Config r i
setReverse reverse (Config config_) =
    Config { config_ | reverse = reverse }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


linearProgress :: Variant -> Config r i -> Html r i
linearProgress variant ((Config { additionalAttributes }) as config_) =
    Html.node "mdc-linear-progress"
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
            ++ additionalAttributes
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


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-linear-progress")


displayCss :: Maybe (Html.Attribute r i)
displayCss =
    Just (style "display" "block")


roleAttr :: Maybe (Html.Attribute r i)
roleAttr =
    Just (Html.Attributes.attribute "role" "progressbar")


variantCs :: Variant -> Maybe (Html.Attribute r i)
variantCs variant =
    case variant of
        Indeterminate ->
            Just (class "mdc-linear-progress--indeterminate")

        _ ->
            Nothing


determinateProp :: Variant -> Maybe (Html.Attribute r i)
determinateProp variant =
    Just (Html.Attributes.property "determinate" (Encode.bool (variant /= Indeterminate)))


progressProp :: Variant -> Maybe (Html.Attribute r i)
progressProp variant =
    Just
        (Html.Attributes.property "progress"
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


bufferProp :: Variant -> Maybe (Html.Attribute r i)
bufferProp variant =
    Just
        (Html.Attributes.property "buffer"
            (Encode.float
                (case variant of
                    Buffered _ buffer ->
                        buffer

                    _ ->
                        0
                )
            )
        )


reverseProp :: Config r i -> Maybe (Html.Attribute r i)
reverseProp (Config { reverse }) =
    Just (Html.Attributes.property "reverse" (Encode.bool reverse))


closedProp :: Config r i -> Maybe (Html.Attribute r i)
closedProp (Config { closed }) =
    Just (Html.Attributes.property "closed" (Encode.bool closed))


bufferingDotsElt :: Html r i
bufferingDotsElt =
    Html.div [ class "mdc-linear-progress__buffering-dots" ] []


bufferElt :: Html r i
bufferElt =
    Html.div [ class "mdc-linear-progress__buffer" ] []


primaryBarElt :: Html r i
primaryBarElt =
    Html.div [ class "mdc-linear-progress__bar mdc-linear-progress__primary-bar" ]
        [ barInnerElt ]


secondaryBarElt :: Html r i
secondaryBarElt =
    Html.div [ class "mdc-linear-progress__bar mdc-linear-progress__secondary-bar" ]
        [ barInnerElt ]


barInnerElt :: Html r i
barInnerElt =
    Html.div [ class "mdc-linear-progress__bar-inner" ] []
