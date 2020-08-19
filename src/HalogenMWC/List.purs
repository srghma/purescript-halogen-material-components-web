module HalogenMWC.Array
    ( Config, config






    , list
    , group, subheader
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import HalogenMWC.Array.Item (Config, ArrayItem)
import HalogenMWC.Array.Item.Internal as ArrayItem



type Config r i
    =
        { nonInteractive :: Boolean
        , dense :: Boolean
        , avatarArray :: Boolean
        , twoLine :: Boolean
        , vertical :: Boolean
        , wrapFocus :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }



config :: Config r i
config =
    Config
        { nonInteractive = False
        , dense = False
        , avatarArray = False
        , twoLine = False
        , vertical = False
        , wrapFocus = False
        , additionalAttributes = []
        }


{-| Specify whether a list should be non-interactive

Non-interactive lists do not feature keyboard interaction and list items have
no visual interaction effect.

-}
setNonInteractive :: Boolean -> Config r i -> Config r i
setNonInteractive nonInteractive (Config config_) =
    Config { config_ | nonInteractive = nonInteractive }



setDense :: Boolean -> Config r i -> Config r i
setDense dense (Config config_) =
    Config { config_ | dense = dense }



setAvatarArray :: Boolean -> Config r i -> Config r i
setAvatarArray avatarArray (Config config_) =
    Config { config_ | avatarArray = avatarArray }



setTwoLine :: Boolean -> Config r i -> Config r i
setTwoLine twoLine (Config config_) =
    Config { config_ | twoLine = twoLine }



setWrapFocus :: Boolean -> Config r i -> Config r i
setWrapFocus wrapFocus (Config config_) =
    Config { config_ | wrapFocus = wrapFocus }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Array view function

The list view function takes its list items as two arguments. The first
argument represents the first list item, and the second argument reresents the
remaining list items. This way we guarantee lists to be non-empty.

-}
list :: Config r i -> ArrayItem r i -> Array (ArrayItem r i) -> Html r i
list ((Config { additionalAttributes }) as config_) firstArrayItem remainingArrayItems =
    let
        listItems =
            firstArrayItem :: remainingArrayItems
    in
    Html.node "mdc-list"
        (Array.filterMap identity
            [ rootCs
            , nonInteractiveCs config_
            , denseCs config_
            , avatarArrayCs config_
            , twoLineCs config_
            , wrapFocusProp config_
            , clickHandler listItems
            , selectedIndexProp listItems
            ]
            ++ additionalAttributes
        )
        (Array.map
            (\listItem_ ->
                case listItem_ of
                    ArrayItem.ArrayItem (ArrayItem.Config { node }) ->
                        node

                    ArrayItem.ArrayItemDivider node ->
                        node

                    ArrayItem.ArrayGroupSubheader node ->
                        node
            )
            listItems
        )


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ mdc_list)


nonInteractiveCs :: Config r i -> Maybe (Html.Attribute r i)
nonInteractiveCs (Config { nonInteractive }) =
    if nonInteractive then
        Just (HP.class_ "mdc-list--non-interactive")

    else
        Nothing


denseCs :: Config r i -> Maybe (Html.Attribute r i)
denseCs (Config { dense }) =
    if dense then
        Just (HP.class_ "mdc-list--dense")

    else
        Nothing


avatarArrayCs :: Config r i -> Maybe (Html.Attribute r i)
avatarArrayCs (Config { avatarArray }) =
    if avatarArray then
        Just (HP.class_ "mdc-list--avatar-list")

    else
        Nothing


twoLineCs :: Config r i -> Maybe (Html.Attribute r i)
twoLineCs (Config { twoLine }) =
    if twoLine then
        Just (HP.class_ "mdc-list--two-line")

    else
        Nothing


clickHandler :: Array (ArrayItem r i) -> Maybe (Html.Attribute r i)
clickHandler listItems =
    let
        getOnClick listItem_ =
            case listItem_ of
                ArrayItem.ArrayItem (ArrayItem.Config { onClick }) ->
                    Just onClick

                ArrayItem.ArrayItemDivider _ ->
                    Nothing

                ArrayItem.ArrayGroupSubheader _ ->
                    Nothing

        nthOnClick index =
            listItems
                # Array.map getOnClick
                # Array.filterMap identity
                # Array.drop index
                # Array.head
                # Maybe.andThen identity

        mergedClickHandler =
            Decode.at [ "detail", "index" ] Decode.int
                # Decode.andThen
                    (\index ->
                        case nthOnClick index of
                            Just msg_ ->
                                Decode.succeed msg_

                            Nothing ->
                                Decode.fail ""
                    )
    in
    Just (Html.Events.on "MDCArray:action" mergedClickHandler)


selectedIndexProp :: Array (ArrayItem r i) -> Maybe (Html.Attribute r i)
selectedIndexProp listItems =
    let
        selectedIndex =
            listItems
                # Array.filter
                    (\listItem_ ->
                        case listItem_ of
                            ArrayItem.ArrayItem _ ->
                                True

                            ArrayItem.ArrayItemDivider _ ->
                                False

                            ArrayItem.ArrayGroupSubheader _ ->
                                False
                    )
                # Array.indexedMap
                    (\index listItem_ ->
                        case listItem_ of
                            ArrayItem.ArrayItem (ArrayItem.Config { selection }) ->
                                if selection /= Nothing then
                                    Just index

                                else
                                    Nothing

                            ArrayItem.ArrayItemDivider _ ->
                                Nothing

                            ArrayItem.ArrayGroupSubheader _ ->
                                Nothing
                    )
                # Array.filterMap identity
    in
    Just (Html.Attributes.property "selectedIndex" (Encode.list Encode.int selectedIndex))



group :: Array (IProp r i) -> Array (Html r i) -> Html r i
group additionalAttributes nodes =
    Html.div (listGroupCs :: additionalAttributes) nodes


listGroupCs :: Html.Attribute r i
listGroupCs =
    HP.class_ mdc_list_group



subheader :: Array (IProp r i) -> Array (Html r i) -> Html r i
subheader additionalAttributes nodes =
    Html.span (listGroupSubheaderCs :: additionalAttributes) nodes


listGroupSubheaderCs :: Html.Attribute r i
listGroupSubheaderCs =
    HP.class_ mdc_list_group__subheader


wrapFocusProp :: Config r i -> Maybe (Html.Attribute r i)
wrapFocusProp (Config { wrapFocus }) =
    Just (Html.Attributes.property "wrapFocus" (Encode.bool wrapFocus))
