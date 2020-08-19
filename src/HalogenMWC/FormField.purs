module HalogenMWC.FormField
    ( Config, config




    , formField
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
        , for :: Maybe String
        , alignEnd :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        }



setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }



setFor :: Maybe String -> Config r i -> Config r i
setFor for (Config config_) =
    Config { config_ | for = for }



setAlignEnd :: Boolean -> Config r i -> Config r i
setAlignEnd alignEnd (Config config_) =
    Config { config_ | alignEnd = alignEnd }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }



config :: Config r i
config =
    Config
        { label = Nothing
        , for = Nothing
        , alignEnd = False
        , additionalAttributes = []
        , onClick = Nothing
        }



formField :: Config r i -> Array (Html r i) -> Html r i
formField ((Config { additionalAttributes }) as config_) nodes =
    Html.node "mdc-form-field"
        (Array.filterMap identity
            [ rootCs
            , alignEndCs config_
            ]
            ++ additionalAttributes
        )
        (nodes ++ [ labelElt config_ ])


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ "mdc-form-field")


alignEndCs :: Config r i -> Maybe (Html.Attribute r i)
alignEndCs (Config { alignEnd }) =
    if alignEnd then
        Just (HP.class_ "mdc-form-field--align-end")

    else
        Nothing


forAttr :: Config r i -> Maybe (Html.Attribute r i)
forAttr (Config { for }) =
    Maybe.map Html.Attributes.for for


clickHandler :: Config r i -> Maybe (Html.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick


labelElt :: Config r i -> Html r i
labelElt ((Config { label }) as config_) =
    Html.label
        (Array.filterMap identity
            [ forAttr config_
            , clickHandler config_
            ]
        )
        [ text (Maybe.withDefault "" label) ]
