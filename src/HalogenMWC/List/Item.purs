module HalogenMWC.Array.Item where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { disabled :: Boolean
    , selection :: Maybe Selection
    , href :: Maybe String
    , target :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    , node :: Html r i
    }

data Selection
  = Selected
  | Activated

data ArrayItem r i
  = ArrayItem (Config r i)
  | ArrayItemDivider (Html r i)
  | ArrayGroupSubheader (Html r i)

defaultConfig :: Config r i
defaultConfig =
        { disabled: false
        , selection: Nothing
        , href: Nothing
        , target: Nothing
        , additionalAttributes: []
        , onClick: Nothing
        , node: HH.text ""
        }

selected :: Selection
selected =
    Selected

activated :: Selection
activated =
    Activated

listItem :: Config r i -> Array (Html r i) -> ArrayItem r i
listItem config_ nodes =
    ArrayItem (config_ { node = listItemView config_ nodes })

listItemView :: Config r i -> Array (Html r i) -> Html r i
listItemView config_ nodes =
    (\attributes ->
        if config_.href /= Nothing then
            HH.element "mdc-list-item" [] [ HH.a attributes nodes ]

        else
            HH.element "mdc-list-item" attributes nodes
    )
    (Array.filterMap identity
        [ HP.class_ mdc_list_item
        , hrefAttr config_
        , targetAttr config_
        , disabledCs config_
        , selectedCs config_
        , activatedCs config_
        , ariaSelectedAttr config_
        ]
        <> config_.additionalAttributes
    )

disabledCs :: Config r i -> Maybe (IProp r i)
disabledCs { disabled } =
    if disabled then
        Just (HP.class_ mdc_list_item____disabled)

    else
        Nothing

selectedCs :: Config r i -> Maybe (IProp r i)
selectedCs { selection } =
    if selection == Just Selected then
        Just (HP.class_ mdc_list_item____selected)

    else
        Nothing

activatedCs :: Config r i -> Maybe (IProp r i)
activatedCs { selection } =
    if selection == Just Activated then
        Just (HP.class_ mdc_list_item____activated)
    else
        Nothing

ariaSelectedAttr :: Config r i -> Maybe (IProp r i)
ariaSelectedAttr { selection } =
    if selection /= Nothing then
        Just (HH.Attributes.attribute "aria-selected" "true")
    else
        Nothing

hrefAttr :: Config r i -> Maybe (IProp r i)
hrefAttr { href } =
    map HH.Attributes.href href

targetAttr :: Config r i -> Maybe (IProp r i)
targetAttr { href, target } =
    if href /= Nothing then
        map HH.Attributes.target target

    else
        Nothing

{-| Two-line list item's text
-}
text ::
    Array (IProp r i)
    ->
        { primary :: Array (Html r i)
        , secondary :: Array (Html r i)
        }
    -> Html r i
text additionalAttributes { primary, secondary } =
    HH.div ([HP.class_ mdc_list_item__text] <> additionalAttributes)
        [ primaryText [] primary
        , secondaryText [] secondary
        ]

primaryText :: Array (IProp r i) -> Array (Html r i) -> Html r i
primaryText additionalAttributes nodes =
    HH.div ([HP.class_ mdc_list_item__primary_text] <> additionalAttributes) nodes

secondaryText :: Array (IProp r i) -> Array (Html r i) -> Html r i
secondaryText additionalAttributes nodes =
    HH.div ([HP.class_ mdc_list_item__secondary_text] <> additionalAttributes) nodes

graphic :: Array (IProp r i) -> Array (Html r i) -> Html r i
graphic additionalAttributes nodes =
    HH.div ([HP.class_ mdc_list_item__graphic] <> additionalAttributes) nodes

meta :: Array (IProp r i) -> Array (Html r i) -> Html r i
meta additionalAttributes nodes =
    HH.div ([HP.class_ mdc_list_item__meta] <> additionalAttributes) nodes
