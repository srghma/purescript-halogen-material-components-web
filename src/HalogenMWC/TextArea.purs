module HalogenMWC.TextArea
    ( Config, config














    , filled
    , outlined
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA







type Config r i
    =
        { label :: Maybe String
        , fullwidth :: Boolean
        , value :: Maybe String
        , placeholder :: Maybe String
        , rows :: Maybe Int
        , cols :: Maybe Int
        , disabled :: Boolean
        , required :: Boolean
        , valid :: Boolean
        , minLength :: Maybe Int
        , maxLength :: Maybe Int
        , additionalAttributes :: Array (IProp r i)
        , onInput :: Maybe (String -> r i)
        , onChange :: Maybe (String -> r i)
        }



config :: Config r i
config =
    Config
        { label = Nothing
        , fullwidth = False
        , value = Nothing
        , placeholder = Nothing
        , rows = Nothing
        , cols = Nothing
        , disabled = False
        , required = False
        , valid = True
        , minLength = Nothing
        , maxLength = Nothing
        , additionalAttributes = []
        , onInput = Nothing
        , onChange = Nothing
        }



setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }



setFullwidth :: Boolean -> Config r i -> Config r i
setFullwidth fullwidth (Config config_) =
    Config { config_ | fullwidth = fullwidth }



setValue :: Maybe String -> Config r i -> Config r i
setValue value (Config config_) =
    Config { config_ | value = value }



setPlaceholder :: Maybe String -> Config r i -> Config r i
setPlaceholder placeholder (Config config_) =
    Config { config_ | placeholder = placeholder }



setRows :: Maybe Int -> Config r i -> Config r i
setRows rows (Config config_) =
    Config { config_ | rows = rows }



setCols :: Maybe Int -> Config r i -> Config r i
setCols cols (Config config_) =
    Config { config_ | cols = cols }



setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }



setRequired :: Boolean -> Config r i -> Config r i
setRequired required (Config config_) =
    Config { config_ | required = required }



setValid :: Boolean -> Config r i -> Config r i
setValid valid (Config config_) =
    Config { config_ | valid = valid }



setMinLength :: Maybe Int -> Config r i -> Config r i
setMinLength minLength (Config config_) =
    Config { config_ | minLength = minLength }



setMaxLength :: Maybe Int -> Config r i -> Config r i
setMaxLength maxLength (Config config_) =
    Config { config_ | maxLength = maxLength }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnInput :: (String -> r i) -> Config r i -> Config r i
setOnInput onInput (Config config_) =
    Config { config_ | onInput = Just onInput }



setOnChange :: (String -> r i) -> Config r i -> Config r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }



filled :: Config r i -> Html r i
filled config_ =
    textArea False config_



outlined :: Config r i -> Html r i
outlined config_ =
    textArea True config_


textArea :: Boolean -> Config r i -> Html r i
textArea outlined_ ((Config { additionalAttributes, fullwidth }) as config_) =
    Html.node "mdc-text-field"
        (Array.filterMap identity
            [ rootCs
            , noLabelCs config_
            , outlinedCs outlined_
            , fullwidthCs config_
            , disabledCs config_
            , valueProp config_
            , disabledProp config_
            , requiredProp config_
            , validProp config_
            , minLengthAttr config_
            , maxLengthAttr config_
            ]
            ++ additionalAttributes
        )
        (Array.concat
            [ if fullwidth then
                [ inputElt config_
                , notchedOutlineElt config_
                ]

              else
                [ inputElt config_
                , notchedOutlineElt config_
                ]
            ]
        )


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ "mdc-text-field mdc-text-field--textarea")


outlinedCs :: Boolean -> Maybe (Html.Attribute r i)
outlinedCs outlined_ =
    if outlined_ then
        Just (HP.class_ "mdc-text-field--outlined")

    else
        Nothing


fullwidthCs :: Config r i -> Maybe (Html.Attribute r i)
fullwidthCs (Config { fullwidth }) =
    if fullwidth then
        Just (HP.class_ "mdc-text-field--fullwidth")

    else
        Nothing


disabledCs :: Config r i -> Maybe (Html.Attribute r i)
disabledCs (Config { disabled }) =
    if disabled then
        Just (HP.class_ "mdc-text-field--disabled")

    else
        Nothing


requiredProp :: Config r i -> Maybe (Html.Attribute r i)
requiredProp (Config { required }) =
    Just (Html.Attributes.property "required" (Encode.bool required))


validProp :: Config r i -> Maybe (Html.Attribute r i)
validProp (Config { valid }) =
    Just (Html.Attributes.property "valid" (Encode.bool valid))


minLengthAttr :: Config r i -> Maybe (Html.Attribute r i)
minLengthAttr (Config { minLength }) =
    Maybe.map (Html.Attributes.attribute "minLength" << String.fromInt) minLength


maxLengthAttr :: Config r i -> Maybe (Html.Attribute r i)
maxLengthAttr (Config { maxLength }) =
    Maybe.map (Html.Attributes.attribute "maxLength" << String.fromInt) maxLength


valueProp :: Config r i -> Maybe (Html.Attribute r i)
valueProp (Config { value }) =
    Maybe.map (Html.Attributes.property "value" << Encode.string) value


placeholderAttr :: Config r i -> Maybe (Html.Attribute r i)
placeholderAttr (Config { placeholder }) =
    Maybe.map Html.Attributes.placeholder placeholder


inputHandler :: Config r i -> Maybe (Html.Attribute r i)
inputHandler (Config { onInput }) =
    Maybe.map Html.Events.onInput onInput


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { onChange }) =
    Maybe.map (\f -> Html.Events.on "change" (Decode.map f Html.Events.targetValue))
        onChange


inputElt :: Config r i -> Html r i
inputElt config_ =
    Html.textarea
        (Array.filterMap identity
            [ inputCs
            , ariaLabelAttr config_
            , rowsAttr config_
            , colsAttr config_
            , placeholderAttr config_
            , inputHandler config_
            , changeHandler config_
            , minLengthAttr config_
            , maxLengthAttr config_
            ]
        )
        []


inputCs :: Maybe (Html.Attribute r i)
inputCs =
    Just (HP.class_ mdc_text_field__input)


rowsAttr :: Config r i -> Maybe (Html.Attribute r i)
rowsAttr (Config { rows }) =
    Maybe.map Html.Attributes.rows rows


colsAttr :: Config r i -> Maybe (Html.Attribute r i)
colsAttr (Config { cols }) =
    Maybe.map Html.Attributes.cols cols


ariaLabelAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaLabelAttr (Config { fullwidth, placeholder, label }) =
    if fullwidth then
        Maybe.map (Html.Attributes.attribute "aria-label") label

    else
        Nothing


disabledProp :: Config r i -> Maybe (Html.Attribute r i)
disabledProp (Config { disabled }) =
    Just (Html.Attributes.property "disabled" (Encode.bool disabled))


labelElt :: Config r i -> Html r i
labelElt (Config { label, value }) =
    let
        floatingLabelCs =
            "mdc-floating-label"

        floatingLabelFloatAboveCs =
            "mdc-floating-label--float-above"
    in
    case label of
        Just str ->
            Html.div
                [ if Maybe.withDefault "" value /= "" then
                    HP.class_ (floatingLabelCs ++ " " ++ floatingLabelFloatAboveCs)

                  else
                    HP.class_ floatingLabelCs
                , Html.Attributes.property "foucClassNames"
                    (Encode.list Encode.string [ floatingLabelFloatAboveCs ])
                ]
                [ text str ]

        Nothing ->
            text ""


noLabelCs :: Config r i -> Maybe (Html.Attribute r i)
noLabelCs (Config { label }) =
    if label == Nothing then
        Just (HP.class_ "mdc-text-field--no-label")

    else
        Nothing


notchedOutlineElt :: Config r i -> Html r i
notchedOutlineElt config_ =
    Html.div [ HP.class_ mdc_notched_outline ]
        [ notchedOutlineLeadingElt
        , notchedOutlineNotchElt config_
        , notchedOutlineTrailingElt
        ]


notchedOutlineLeadingElt :: Html r i
notchedOutlineLeadingElt =
    Html.div [ HP.class_ mdc_notched_outline__leading ] []


notchedOutlineTrailingElt :: Html r i
notchedOutlineTrailingElt =
    Html.div [ HP.class_ mdc_notched_outline__trailing ] []


notchedOutlineNotchElt :: Config r i -> Html r i
notchedOutlineNotchElt config_ =
    Html.div [ HP.class_ mdc_notched_outline__notch ] [ labelElt config_ ]
