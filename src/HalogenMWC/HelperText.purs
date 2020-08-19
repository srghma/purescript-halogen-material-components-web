module HalogenMWC.HelperText
    ( Config, config


    , helperText
    , helperLine, characterCounter
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




type Config r i
    =
        { persistent :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }



config :: Config r i
config =
    Config
        { persistent = False
        , additionalAttributes = []
        }



setPersistent :: Boolean -> Config r i -> Config r i
setPersistent persistent (Config config_) =
    Config { config_ | persistent = persistent }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



helperText :: Config r i -> String -> Html r i
helperText ((Config { additionalAttributes }) as config_) string =
    Html.div
        (Array.filterMap identity
            [ helperTextCs
            , persistentCs config_
            , ariaHiddenAttr
            ]
            ++ additionalAttributes
        )
        [ text string ]



helperLine :: Array (IProp r i) -> Array (Html r i) -> Html r i
helperLine additionalAttributes nodes =
    Html.div (helperLineCs :: additionalAttributes) nodes


helperTextCs :: Maybe (Html.Attribute r i)
helperTextCs =
    Just (HP.class_ mdc_text_field_helper_text)


helperLineCs :: Html.Attribute r i
helperLineCs =
    HP.class_ mdc_text_field_helper_line


persistentCs :: Config r i -> Maybe (Html.Attribute r i)
persistentCs (Config config_) =
    if config_.persistent then
        Just (HP.class_ "mdc-text-field-helper-text--persistent")

    else
        Nothing


ariaHiddenAttr :: Maybe (Html.Attribute r i)
ariaHiddenAttr =
    Just (Html.Attributes.attribute "aria-hidden" "true")



characterCounter :: Array (IProp r i) -> Html r i
characterCounter additionalAttributes =
    Html.div (characterCounterCs :: additionalAttributes) []


characterCounterCs :: Html.Attribute r i
characterCounterCs =
    HP.class_ mdc_text_field_character_counter
