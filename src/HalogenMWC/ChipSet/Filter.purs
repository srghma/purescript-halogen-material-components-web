module HalogenMWC.ChipSet.Filter where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Filter as Chip
import HalogenMWC.Chip.Filter
import Halogen.SVG.Elements as Halogen.SVG.Elements
import Halogen.SVG.Attributes as Halogen.SVG.Attributes

chipSet :: Array (IProp r i) -> Array (Chip r i) -> HH.HTML w i
chipSet additionalAttributes chips =
  HH.element "mdc-chip-set"
    ([ HP.class_ mdc_chip_set, HP.class_ mdc_chip_set____filter, HH.Attributes.attribute "role" "grid" ] <> additionalAttributes)
    (map chip chips)

chip :: Chip r i -> HH.HTML w i
chip (Chip (config_@{ additionalAttributes }) label) =
  HH.div [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.element "mdc-chip"
        ( Array.filterMap identity
            [ HP.class_ mdc_chip
            , HP.class_ mdc_chip____touch
            , HH.Attributes.attribute "role" "row"
            , selectedProp config_
            , interactionHandler config_
            ]
            <> additionalAttributes
        )
        ( Array.filterMap identity
            [ rippleElt
            , leadingIconElt config_
            , checkmarkElt
            , primaryActionElt label
            ]
        )
    ]

selectedProp :: Chip.Config r i -> Maybe (IProp r i)
selectedProp { selected } = Just (HH.Attributes.property "selected" (Encode.bool selected))

interactionHandler :: Chip.Config r i -> Maybe (IProp r i)
interactionHandler { onChange } = map (HH.Events.on "MDCChip:interaction" << Decode.succeed) onChange

rippleElt :: Maybe (HH.HTML w i)
rippleElt = Just (HH.div [ HP.class_ mdc_chip__ripple ] [])

leadingIconElt :: Chip.Config r i -> Maybe (HH.HTML w i)
leadingIconElt { icon, selected } =
  map
    ( \iconName ->
        HH.i
          [ HP.class_ material_icons
          , HP.class_ "mdc-chip__icon mdc-chip__icon--leading"
          ]
          [ text iconName ]
    )
    icon

checkmarkElt :: Maybe (HH.HTML w i)
checkmarkElt =
  Just
    ( HH.div [ HP.class_ mdc_chip__checkmark ]
        [ Halogen.SVG.Elements.svg
            [ Halogen.SVG.Attributes.class_ "mdc-chip__checkmark-svg"
            , Halogen.SVG.Attributes.viewBox "-2 -3 30 30"
            ]
            [ Halogen.SVG.Elements.path
                [ Halogen.SVG.Attributes.class_ "mdc-chip__checkmark-path"
                , Halogen.SVG.Attributes.fill "none"
                , Halogen.SVG.Attributes.stroke "black"
                , Halogen.SVG.Attributes.d "M1.73,12.91 8.1,19.28 22.79,4.59"
                ]
                []
            ]
        ]
    )

primaryActionElt :: String -> Maybe (HH.HTML w i)
primaryActionElt label =
  Just
    $ HH.span [ HP.class_ mdc_chip__primary_action, HH.Attributes.attribute "role" "gridcell" ]
        (Array.filterMap identity [ textElt label, touchElt ])

textElt :: String -> Maybe (HH.HTML w i)
textElt label = Just (HH.span [ HP.class_ mdc_chip__text, HH.Attributes.attribute "role" "button" ] [ text label ])

touchElt :: Maybe (HH.HTML w i)
touchElt = Just (HH.div [ HP.class_ mdc_chip__touch ] [])
