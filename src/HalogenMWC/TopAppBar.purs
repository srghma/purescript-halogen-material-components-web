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

defaultConfig :: Config r i
defaultConfig =
        { dense: False
        , fixed: False
        , additionalAttributes: []
        }

genericTopAppBar :: Variant -> Config r i -> Array (Html r i) -> Html r i
genericTopAppBar variant (config_@{ additionalAttributes }) nodes =
    HH.node "mdc-top-app-bar"
        (Array.filterMap identity
            [ rootCs
            , variantCs variant
            , denseCs config_
            , fixedCs config_
            ]
            <> additionalAttributes
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
    HH.section ([ HP.class_ mdc_top_app_bar__row ] <> attributes) nodes

{-| Sections subdivide the top app bar's rows. A section may be start- or
end-aligned. Usually, the first section is start-aligned and contains the top
app bar's navigation icon and title.
-}
section :: Array (IProp r i) -> Array (Html r i) -> Html r i
section attributes nodes =
    HH.section ([ HP.class_ mdc_top_app_bar__section ] <> attributes) nodes

{-| Start-align a top app bar's `section`
-}
alignStart :: HH.Attribute r i
alignStart =
    HP.class_ "mdc-top-app-bar__section--align-start"

{-| End-align a top app bar's `section`
-}
alignEnd :: HH.Attribute r i
alignEnd =
    HP.class_ "mdc-top-app-bar__section--align-end"

navigationIcon :: HH.Attribute r i
navigationIcon =
    HP.class_ mdc_top_app_bar__navigation_icon

title :: HH.Attribute r i
title =
    HP.class_ mdc_top_app_bar__title

actionItem :: HH.Attribute r i
actionItem =
    HP.class_ mdc_top_app_bar__action_item

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_top_app_bar)

variantCs :: Variant -> Maybe (HH.Attribute r i)
variantCs variant =
    case variant of
        Regular ->
            Nothing

        Short ->
            Just (HP.class_ mdc_top_app_bar____short)

        ShortCollapsed ->
            Just (HP.class_ "mdc-top-app-bar--short mdc-top-app-bar--short-collapsed")

        Prominent ->
            Just (HP.class_ mdc_top_app_bar____prominent)

denseCs :: Config r i -> Maybe (HH.Attribute r i)
denseCs { dense } =
    if dense then
        Just (HP.class_ mdc_top_app_bar____dense)

    else
        Nothing

fixedCs :: Config r i -> Maybe (HH.Attribute r i)
fixedCs { fixed } =
    if fixed then
        Just (HP.class_ mdc_top_app_bar____fixed)

    else
        Nothing

fixedAdjust :: HH.Attribute r i
fixedAdjust =
    HP.class_ "mdc-top-app-bar--fixed-adjust"

denseFixedAdjust :: HH.Attribute r i
denseFixedAdjust =
    HP.class_ "mdc-top-app-bar--dense-fixed-adjust"

shortFixedAdjust :: HH.Attribute r i
shortFixedAdjust =
    HP.class_ "mdc-top-app-bar--short-fixed-adjust"

prominentFixedAdjust :: HH.Attribute r i
prominentFixedAdjust =
    HP.class_ "mdc-top-app-bar--prominent-fixed-adjust"

denseProminentFixedAdjust :: HH.Attribute r i
denseProminentFixedAdjust =
    HP.class_ "mdc-top-app-bar--dense-prominent-fixed-adjust"
