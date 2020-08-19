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













helperText :: Config r i -> String -> Html r i
helperText ((Config { additionalAttributes }) as config_) string =
    HH.div
        (Array.filterMap identity
            [ helperTextCs
            , persistentCs config_
            , ariaHiddenAttr
            ]
            <> additionalAttributes
        )
        [ text string ]



helperLine :: Array (IProp r i) -> Array (Html r i) -> Html r i
helperLine additionalAttributes nodes =
    HH.div ([helperLineCs] <> additionalAttributes) nodes


helperTextCs :: Maybe (HH.Attribute r i)
helperTextCs =
    Just (HP.class_ mdc_text_field_helper_text)


helperLineCs :: HH.Attribute r i
helperLineCs =
    HP.class_ mdc_text_field_helper_line


persistentCs :: Config r i -> Maybe (HH.Attribute r i)
persistentCs (Config config_) =
    if config_.persistent then
        Just (HP.class_ mdc_text_field_helper_text____persistent)

    else
        Nothing


ariaHiddenAttr :: Maybe (HH.Attribute r i)
ariaHiddenAttr =
    Just (HH.Attributes.attribute "aria-hidden" "true")



characterCounter :: Array (IProp r i) -> Html r i
characterCounter additionalAttributes =
    HH.div ([characterCounterCs] <> additionalAttributes) []


characterCounterCs :: HH.Attribute r i
characterCounterCs =
    HP.class_ mdc_text_field_character_counter
