module HalogenMWC.TextField
    ( Config, config



















    , filled
    , outlined
    , Icon, icon
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import HalogenMWC.Icon as Icon



type Config r i
    =
        { label :: Maybe String
        , fullwidth :: Boolean
        , value :: Maybe String
        , placeholder :: Maybe String
        , disabled :: Boolean
        , required :: Boolean
        , valid :: Boolean
        , minLength :: Maybe Int
        , maxLength :: Maybe Int
        , pattern :: Maybe String
        , type_ :: Maybe String
        , min :: Maybe Int
        , max :: Maybe Int
        , step :: Maybe Int
        , leadingIcon :: Maybe (Icon r i)
        , trailingIcon :: Maybe (Icon r i)
        , additionalAttributes :: Array (IProp r i)
        , onInput :: Maybe (String -> r i)
        , onChange :: Maybe (String -> r i)
        }


{-| Text field trailing or leading icon -
-}
data Icon r i
    = Icon (Html r i)



config :: Config r i
config =
    Config
        { label = Nothing
        , fullwidth = False
        , value = Nothing
        , placeholder = Nothing
        , disabled = False
        , required = False
        , valid = True
        , minLength = Nothing
        , maxLength = Nothing
        , pattern = Nothing
        , type_ = Nothing
        , min = Nothing
        , max = Nothing
        , step = Nothing
        , leadingIcon = Nothing
        , trailingIcon = Nothing
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



setPattern :: Maybe String -> Config r i -> Config r i
setPattern pattern (Config config_) =
    Config { config_ | pattern = pattern }



setType :: Maybe String -> Config r i -> Config r i
setType type_ (Config config_) =
    Config { config_ | type_ = type_ }



setMin :: Maybe Int -> Config r i -> Config r i
setMin min (Config config_) =
    Config { config_ | min = min }



setMax :: Maybe Int -> Config r i -> Config r i
setMax max (Config config_) =
    Config { config_ | max = max }



setStep :: Maybe Int -> Config r i -> Config r i
setStep step (Config config_) =
    Config { config_ | step = step }



setLeadingIcon :: Maybe (Icon r i) -> Config r i -> Config r i
setLeadingIcon leadingIcon (Config config_) =
    Config { config_ | leadingIcon = leadingIcon }



setTrailingIcon :: Maybe (Icon r i) -> Config r i -> Config r i
setTrailingIcon trailingIcon (Config config_) =
    Config { config_ | trailingIcon = trailingIcon }



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
    textField False config_



outlined :: Config r i -> Html r i
outlined config_ =
    textField True config_


textField :: Boolean -> Config r i -> Html r i
textField outlined_ ((Config { additionalAttributes, fullwidth }) as config_) =
    Html.node "mdc-text-field"
        (Array.filterMap identity
            [ rootCs
            , noLabelCs config_
            , outlinedCs outlined_
            , fullwidthCs config_
            , disabledCs config_
            , withLeadingIconCs config_
            , withTrailingIconCs config_
            , valueProp config_
            , disabledProp config_
            , requiredProp config_
            , validProp config_
            , patternProp config_
            , minLengthProp config_
            , maxLengthProp config_
            , minProp config_
            , maxProp config_
            , stepProp config_
            ]
            ++ additionalAttributes
        )
        (Array.concat
            [ leadingIconElt config_
            , if fullwidth then
                if outlined_ then
                    [ inputElt config_
                    , notchedOutlineElt config_
                    ]

                else
                    [ inputElt config_
                    , lineRippleElt
                    ]

              else if outlined_ then
                [ inputElt config_
                , notchedOutlineElt config_
                ]

              else
                [ inputElt config_
                , labelElt config_
                , lineRippleElt
                ]
            , trailingIconElt config_
            ]
        )



icon :: Array (IProp r i) -> String -> Icon r i
icon additionalAttributes iconName =
    Icon (Icon.icon (HP.class_ mdc_text_field__icon :: additionalAttributes) iconName)


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ mdc_text_field)


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


withLeadingIconCs :: Config r i -> Maybe (Html.Attribute r i)
withLeadingIconCs (Config { leadingIcon }) =
    if leadingIcon /= Nothing then
        Just (HP.class_ "mdc-text-field--with-leading-icon")

    else
        Nothing


withTrailingIconCs :: Config r i -> Maybe (Html.Attribute r i)
withTrailingIconCs (Config { trailingIcon }) =
    if trailingIcon /= Nothing then
        Just (HP.class_ "mdc-text-field--with-trailing-icon")

    else
        Nothing


requiredProp :: Config r i -> Maybe (Html.Attribute r i)
requiredProp (Config { required }) =
    Just (Html.Attributes.property "required" (Encode.bool required))


validProp :: Config r i -> Maybe (Html.Attribute r i)
validProp (Config { valid }) =
    Just (Html.Attributes.property "valid" (Encode.bool valid))


minLengthProp :: Config r i -> Maybe (Html.Attribute r i)
minLengthProp (Config { minLength }) =
    Just
        (Html.Attributes.property "minLength"
            (Encode.int (Maybe.withDefault -1 minLength))
        )


maxLengthProp :: Config r i -> Maybe (Html.Attribute r i)
maxLengthProp (Config { maxLength }) =
    Just
        (Html.Attributes.property "maxLength"
            (Encode.int (Maybe.withDefault -1 maxLength))
        )


minLengthAttr :: Config r i -> Maybe (Html.Attribute r i)
minLengthAttr (Config { minLength }) =
    Maybe.map (Html.Attributes.attribute "minLength" << String.fromInt) minLength


maxLengthAttr :: Config r i -> Maybe (Html.Attribute r i)
maxLengthAttr (Config { maxLength }) =
    Maybe.map (Html.Attributes.attribute "maxLength" << String.fromInt) maxLength


minProp :: Config r i -> Maybe (Html.Attribute r i)
minProp (Config { min }) =
    Just
        (Html.Attributes.property "min"
            (Encode.string (Maybe.withDefault "" (Maybe.map String.fromInt min)))
        )


maxProp :: Config r i -> Maybe (Html.Attribute r i)
maxProp (Config { max }) =
    Just
        (Html.Attributes.property "max"
            (Encode.string (Maybe.withDefault "" (Maybe.map String.fromInt max)))
        )


stepProp :: Config r i -> Maybe (Html.Attribute r i)
stepProp (Config { step }) =
    Just
        (Html.Attributes.property "step"
            (Encode.string (Maybe.withDefault "" (Maybe.map String.fromInt step)))
        )


valueProp :: Config r i -> Maybe (Html.Attribute r i)
valueProp (Config { value }) =
    Maybe.map (Html.Attributes.property "value" << Encode.string) value


placeholderAttr :: Config r i -> Maybe (Html.Attribute r i)
placeholderAttr (Config { placeholder }) =
    Maybe.map Html.Attributes.placeholder placeholder


leadingIconElt :: Config r i -> Array (Html r i)
leadingIconElt (Config { leadingIcon }) =
    case leadingIcon of
        Nothing ->
            []

        Just (Icon html) ->
            [ html ]


trailingIconElt :: Config r i -> Array (Html r i)
trailingIconElt (Config { trailingIcon }) =
    case trailingIcon of
        Nothing ->
            []

        Just (Icon html) ->
            [ html ]


inputHandler :: Config r i -> Maybe (Html.Attribute r i)
inputHandler (Config { onInput }) =
    Maybe.map Html.Events.onInput onInput


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { onChange }) =
    Maybe.map (\f -> Html.Events.on "change" (Decode.map f Html.Events.targetValue))
        onChange


inputElt :: Config r i -> Html r i
inputElt config_ =
    Html.input
        (Array.filterMap identity
            [ inputCs
            , typeAttr config_
            , ariaLabelAttr config_
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


patternProp :: Config r i -> Maybe (Html.Attribute r i)
patternProp (Config { pattern }) =
    Just
        (Html.Attributes.property "pattern"
            (Maybe.withDefault Encode.null (Maybe.map Encode.string pattern))
        )


typeAttr :: Config r i -> Maybe (Html.Attribute r i)
typeAttr (Config { type_ }) =
    Maybe.map Html.Attributes.type_ type_


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


lineRippleElt :: Html r i
lineRippleElt =
    Html.div [ HP.class_ mdc_line_ripple ] []


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
