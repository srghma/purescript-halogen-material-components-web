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
    HH.node "mdc-form-field"
        (Array.filterMap identity
            [ rootCs
            , alignEndCs config_
            ]
            ++ additionalAttributes
        )
        (nodes ++ [ labelElt config_ ])


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_form_field)


alignEndCs :: Config r i -> Maybe (HH.Attribute r i)
alignEndCs (Config { alignEnd }) =
    if alignEnd then
        Just (HP.class_ "mdc-form-field--align-end")

    else
        Nothing


forAttr :: Config r i -> Maybe (HH.Attribute r i)
forAttr (Config { for }) =
    Maybe.map HH.Attributes.for for


clickHandler :: Config r i -> Maybe (HH.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map HH.Events.onClick onClick


labelElt :: Config r i -> Html r i
labelElt ((Config { label }) as config_) =
    HH.label
        (Array.filterMap identity
            [ forAttr config_
            , clickHandler config_
            ]
        )
        [ text (Maybe.withDefault "" label) ]
