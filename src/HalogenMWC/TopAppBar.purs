module HalogenMWC.TopAppBar
    ( Config, config



    , regular
    , row, section, alignEnd, alignStart
    , navigationIcon, title
    , actionItem
    , fixedAdjust
    , denseFixedAdjust
    , denseProminentFixedAdjust
    , prominentFixedAdjust
    , shortFixedAdjust
    , short
    , shortCollapsed
    , prominent
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




type Config r i
    =
        { dense :: Boolean
        , fixed :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


data Variant
    = Regular
    | Short
    | ShortCollapsed
    | Prominent



config :: Config r i
config =
    Config
        { dense = False
        , fixed = False
        , additionalAttributes = []
        }



setDense :: Boolean -> Config r i -> Config r i
setDense dense (Config config_) =
    Config { config_ | dense = dense }



setFixed :: Boolean -> Config r i -> Config r i
setFixed fixed (Config config_) =
    Config { config_ | fixed = fixed }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


genericTopAppBar :: Variant -> Config r i -> Array (Html r i) -> Html r i
genericTopAppBar variant ((Config { additionalAttributes }) as config_) nodes =
    Html.node "mdc-top-app-bar"
        (Array.filterMap identity
            [ rootCs
            , variantCs variant
            , denseCs config_
            , fixedCs config_
            ]
            ++ additionalAttributes
        )
        nodes



regular :: Config r i -> Array (Html r i) -> Html r i
regular config_ nodes =
    genericTopAppBar Regular config_ nodes



short :: Config r i -> Array (Html r i) -> Html r i
short config_ nodes =
    genericTopAppBar Short config_ nodes



shortCollapsed :: Config r i -> Array (Html r i) -> Html r i
shortCollapsed config_ nodes =
    genericTopAppBar ShortCollapsed config_ nodes



prominent :: Config r i -> Array (Html r i) -> Html r i
prominent config_ nodes =
    genericTopAppBar Prominent config_ nodes



row :: Array (IProp r i) -> Array (Html r i) -> Html r i
row attributes nodes =
    Html.section ([ class "mdc-top-app-bar__row" ] ++ attributes) nodes


{-| Sections subdivide the top app bar's rows. A section may be start- or
end-aligned. Usually, the first section is start-aligned and contains the top
app bar's navigation icon and title.
-}
section :: Array (IProp r i) -> Array (Html r i) -> Html r i
section attributes nodes =
    Html.section ([ class "mdc-top-app-bar__section" ] ++ attributes) nodes


{-| Start-align a top app bar's `section`
-}
alignStart :: Html.Attribute r i
alignStart =
    class "mdc-top-app-bar__section--align-start"


{-| End-align a top app bar's `section`
-}
alignEnd :: Html.Attribute r i
alignEnd =
    class "mdc-top-app-bar__section--align-end"



navigationIcon :: Html.Attribute r i
navigationIcon =
    class "mdc-top-app-bar__navigation-icon"



title :: Html.Attribute r i
title =
    class "mdc-top-app-bar__title"



actionItem :: Html.Attribute r i
actionItem =
    class "mdc-top-app-bar__action-item"


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-top-app-bar")


variantCs :: Variant -> Maybe (Html.Attribute r i)
variantCs variant =
    case variant of
        Regular ->
            Nothing

        Short ->
            Just (class "mdc-top-app-bar--short")

        ShortCollapsed ->
            Just (class "mdc-top-app-bar--short mdc-top-app-bar--short-collapsed")

        Prominent ->
            Just (class "mdc-top-app-bar--prominent")


denseCs :: Config r i -> Maybe (Html.Attribute r i)
denseCs (Config { dense }) =
    if dense then
        Just (class "mdc-top-app-bar--dense")

    else
        Nothing


fixedCs :: Config r i -> Maybe (Html.Attribute r i)
fixedCs (Config { fixed }) =
    if fixed then
        Just (class "mdc-top-app-bar--fixed")

    else
        Nothing



fixedAdjust :: Html.Attribute r i
fixedAdjust =
    class "mdc-top-app-bar--fixed-adjust"



denseFixedAdjust :: Html.Attribute r i
denseFixedAdjust =
    class "mdc-top-app-bar--dense-fixed-adjust"



shortFixedAdjust :: Html.Attribute r i
shortFixedAdjust =
    class "mdc-top-app-bar--short-fixed-adjust"



prominentFixedAdjust :: Html.Attribute r i
prominentFixedAdjust =
    class "mdc-top-app-bar--prominent-fixed-adjust"



denseProminentFixedAdjust :: Html.Attribute r i
denseProminentFixedAdjust =
    class "mdc-top-app-bar--dense-prominent-fixed-adjust"
