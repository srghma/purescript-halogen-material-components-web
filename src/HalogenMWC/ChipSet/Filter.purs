module HalogenMWC.ChipSet.Filter (chipSet) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Filter.Internal as Chip
import Halogen.SVG.Elements as Halogen.SVG.Elements
import Halogen.SVG.Attributes as Halogen.SVG.Attributes

chipSet :: Array (IProp r i) -> Array (Chip r i) -> Html r i
chipSet additionalAttributes chips =
  HH.node "mdc-chip-set"
    ([ chipSetCs, chipSetFilterCs, gridRole ] <> additionalAttributes)
    (Array.map chip chips)

chip :: Chip r i -> Html r i
chip (Chip (config_@{ additionalAttributes }) label) =
  HH.div [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.node "mdc-chip"
        ( Array.filterMap identity
            [ chipCs
            , chipTouchCs
            , rowRole
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

chipSetCs :: HH.Attribute r i
chipSetCs = HP.class_ mdc_chip_set

chipSetFilterCs :: HH.Attribute r i
chipSetFilterCs = HP.class_ mdc_chip_set____filter

gridRole :: HH.Attribute r i
gridRole = HH.Attributes.attribute "role" "grid"

chipCs :: Maybe (HH.Attribute r i)
chipCs = Just (HP.class_ mdc_chip)

chipTextCs :: HH.Attribute r i
chipTextCs = HP.class_ mdc_chip__text

chipTouchCs :: Maybe (HH.Attribute r i)
chipTouchCs = Just (HP.class_ mdc_chip____touch)

chipPrimaryActionCs :: HH.Attribute r i
chipPrimaryActionCs = HP.class_ mdc_chip__primary_action

selectedProp :: Chip.Config r i -> Maybe (HH.Attribute r i)
selectedProp { selected } = Just (HH.Attributes.property "selected" (Encode.bool selected))

buttonRole :: HH.Attribute r i
buttonRole = HH.Attributes.attribute "role" "button"

rowRole :: Maybe (HH.Attribute r i)
rowRole = Just (HH.Attributes.attribute "role" "row")

gridcellRole :: HH.Attribute r i
gridcellRole = HH.Attributes.attribute "role" "gridcell"

interactionHandler :: Chip.Config r i -> Maybe (HH.Attribute r i)
interactionHandler { onChange } = map (HH.Events.on "MDCChip:interaction" << Decode.succeed) onChange

rippleElt :: Maybe (Html r i)
rippleElt = Just (HH.div [ HP.class_ mdc_chip__ripple ] [])

leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
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

checkmarkElt :: Maybe (Html r i)
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

primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
  Just
    $ HH.span [ chipPrimaryActionCs, gridcellRole ]
        (Array.filterMap identity [ textElt label, touchElt ])

textElt :: String -> Maybe (Html r i)
textElt label = Just (HH.span [ chipTextCs, buttonRole ] [ text label ])

touchElt :: Maybe (Html r i)
touchElt = Just (HH.div [ HP.class_ mdc_chip__touch ] [])
