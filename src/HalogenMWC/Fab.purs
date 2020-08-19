module HalogenMWC.Fab
    ( Config, config




    , fab
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA






type Config r i
    =
        { mini :: Boolean
        , exited :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        }



config :: Config r i
config =
    Config
        { mini = False
        , exited = False
        , onClick = Nothing
        , additionalAttributes = []
        }



setMini :: Boolean -> Config r i -> Config r i
setMini mini (Config config_) =
    Config { config_ | mini = mini }



setExited :: Boolean -> Config r i -> Config r i
setExited exited (Config config_) =
    Config { config_ | exited = exited }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }



fab :: Config r i -> String -> Html r i
fab ((Config { additionalAttributes }) as config_) iconName =
    Html.node "mdc-fab"
        (Array.filterMap identity
            [ rootCs
            , miniCs config_
            , exitedCs config_
            , clickHandler config_
            , tabIndexProp 0
            ]
            ++ additionalAttributes
        )
        [ rippleElt
        , iconElt iconName
        ]


tabIndexProp :: Int -> Maybe (Html.Attribute r i)
tabIndexProp tabIndex =
    Just (Html.Attributes.property "tabIndex" (Encode.int tabIndex))


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ "mdc-fab")


miniCs :: Config r i -> Maybe (Html.Attribute r i)
miniCs (Config { mini }) =
    if mini then
        Just (HP.class_ "mdc-fab--mini")

    else
        Nothing


exitedCs :: Config r i -> Maybe (Html.Attribute r i)
exitedCs (Config { exited }) =
    if exited then
        Just (HP.class_ "mdc-fab--exited")

    else
        Nothing


rippleElt :: Html r i
rippleElt =
    Html.div [ HP.class_ "mdc-fab__ripple" ] []


iconElt :: String -> Html r i
iconElt iconName =
    Html.span [ HP.class_ "material-icons", HP.class_ "mdc-fab__icon" ] [ text iconName ]


clickHandler :: Config r i -> Maybe (Html.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick
