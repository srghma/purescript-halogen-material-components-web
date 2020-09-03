module Demo.Pages.TextField where

import Halogen
import Material.Classes.Typography
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils (mkComponentStatic)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.HelperText as HelperText
import HalogenMWC.TextArea as TextArea
import HalogenMWC.TextField as TextField
import Demo.MkComponent.WithFocus as WithFocus

config :: CatalogPage
config =
    { title: "Text Field"
    , prelude: "Text fields allow users to input, edit, and select text. Text fields typically reside in forms but can appear in other places, like dialog boxes and search."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-text-fields"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-TextField"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-textfield"
        }
    , hero: mkComponentStatic $
        let
          textFieldContainerHero :: forall r i . Array (IProp ( style :: String | r ) i)
          textFieldContainerHero =
            [ HP.style "min-width: 200px; padding: 20px;"
            ]
        in
          HH.div
          [ HP.style "display: -ms-flexbox; display: flex; -ms-flex: 1 1 100%; flex: 1 1 100%; -ms-flex-pack: distribute; justify-content: space-around; -ms-flex-wrap: wrap; flex-wrap: wrap;"
          ]
          [ HH.div textFieldContainerHero
              [ TextField.filled
                  (TextField.defaultConfig
                      { label = Just "Standard"
                      }
                  )
              ]
          , HH.div textFieldContainerHero
              [ TextField.outlined
                  (TextField.defaultConfig
                      { label = Just "Standard"
                      }
                  )
              ]
          ]
    , content: WithFocus.mkComponent $ HH.div_
        [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Filled" ]
        , filledTextFields
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Shaped Filled" ]
        , shapedFilledTextFields
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Outlined" ]
        , outlinedTextFields
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Shaped Outlined (TODO)" ]
        , shapedOutlinedTextFields
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Text Fields without Label" ]
        , textFieldsWithoutLabel
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Text Fields with Character Counter" ]
        , textFieldsWithCharacterCounter
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Textarea" ]
        , textareaTextField
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Full Width" ]
        , fullwidthTextField
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Full Width Textarea" ]
        , fullwidthTextareaTextField
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Text Field" ]
        , focusTextField
        ]
    }

filledTextFields :: forall r w i . HH.HTML w i
filledTextFields =
  HH.div textFieldRow
    [ HH.div textFieldContainer
      [ TextField.filled
        (TextField.defaultConfig
          { label = Just "Standard"
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.filled
        (TextField.defaultConfig
          { label = Just "Standard"
          , leadingIcon = Just (TextField.icon [] "event")
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.filled
        (TextField.defaultConfig
          { label = Just "Standard"
          , trailingIcon = Just (TextField.icon [] "delete")
          }
        )
      , demoHelperText
      ]
    ]

shapedFilledTextFields :: forall r w i . HH.HTML w i
shapedFilledTextFields =
  HH.div textFieldRow
    [ HH.div textFieldContainer
      [ TextField.filled
        (TextField.defaultConfig
          { label = Just "Standard"
          , additionalAttributes = [ HP.style "border-radius: 16px 16px 0 0;" ]
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.filled
        (TextField.defaultConfig
          { label = Just "Standard"
          , leadingIcon = Just (TextField.icon [] "event")
          , additionalAttributes = [ HP.style "border-radius: 16px 16px 0 0;" ]
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.filled
        (TextField.defaultConfig
          { label = Just "Standard"
          , trailingIcon = Just (TextField.icon [] "delete")
          , additionalAttributes = [ HP.style "border-radius: 16px 16px 0 0;" ]
          }
        )
      , demoHelperText
      ]
    ]

outlinedTextFields :: forall r w i . HH.HTML w i
outlinedTextFields =
  HH.div textFieldRow
    [ HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { label = Just "Standard"
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { label = Just "Standard"
          , leadingIcon = Just (TextField.icon [] "event")
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { label = Just "Standard"
          , trailingIcon = Just (TextField.icon [] "delete")
          }
        )
      , demoHelperText
      ]
    ]

shapedOutlinedTextFields :: forall r w i . HH.HTML w i
shapedOutlinedTextFields =
  HH.div textFieldRow
    [ HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { label = Just "Standard"
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { label = Just "Standard"
          , leadingIcon = Just (TextField.icon [] "event")
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { label = Just "Standard"
          , trailingIcon = Just (TextField.icon [] "delete")
          }
        )
      , demoHelperText
      ]
    ]

textFieldsWithoutLabel :: forall r w i . HH.HTML w i
textFieldsWithoutLabel =
  HH.div textFieldRow
    [ HH.div textFieldContainer
      [ TextField.outlined TextField.defaultConfig
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { leadingIcon = Just (TextField.icon [] "event")
          }
        )
      , demoHelperText
      ]
    , HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { trailingIcon = Just (TextField.icon [] "delete")
          }
        )
      , demoHelperText
      ]
    ]

textFieldsWithCharacterCounter :: forall r w i . HH.HTML w i
textFieldsWithCharacterCounter =
  HH.div textFieldRow
    [ HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig { maxLength = Just 18 })
      , demoHelperTextWithCharacterCounter
      ]
    , HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { leadingIcon = Just (TextField.icon [] "event")
          , maxLength = Just 18
          }
        )
      , demoHelperTextWithCharacterCounter
      ]
    , HH.div textFieldContainer
      [ TextField.outlined
        (TextField.defaultConfig
          { trailingIcon = Just (TextField.icon [] "delete")
          , maxLength = Just 18
          }
        )
      , demoHelperTextWithCharacterCounter
      ]
    ]

textareaTextField :: forall r w i . HH.HTML w i
textareaTextField =
  HH.div textFieldContainer
    [ TextArea.outlined (TextArea.defaultConfig { label = Just "Standard" })
    , demoHelperText
    ]

fullwidthTextField :: forall r w i . HH.HTML w i
fullwidthTextField =
  HH.div textFieldContainer
    [ TextField.filled
      (TextField.defaultConfig
        { placeholder = Just "Standard"
        , fullwidth = true
        }
      )
    , demoHelperText
    ]

fullwidthTextareaTextField :: forall r w i . HH.HTML w i
fullwidthTextareaTextField =
  HH.div textFieldRowFullwidth
    [ HH.div textFieldContainer
      [ TextArea.outlined
        (TextArea.defaultConfig
          { label = Just "Standard"
          , fullwidth = true
          }
        )
      , demoHelperText
      ]
    ]

focusTextField :: forall r w i . HH.HTML w WithFocus.Action
focusTextField =
  HH.div_
    [ TextField.filled
      (TextField.defaultConfig
        { additionalAttributes = [ HP.id_ "my-text-field" ]
        }
      )
    , HH.text "\x00A0"
    , Button.button Button.Raised
      (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ WithFocus.Focus "my-text-field") ] })
      [ HH.text "Focus" ]
    ]

textFieldContainer :: forall r w i . Array (IProp ( style :: String | r ) i)
textFieldContainer =
    [ HP.style "min-width: 200px;"
    ]

textFieldRow :: forall r w i . Array (IProp ( style :: String | r ) i)
textFieldRow =
    [ HP.style "display: flex; align-items: flex-start; justify-content: space-between; flex-wrap: wrap;"
    ]

textFieldRowFullwidth :: forall r w i . Array (IProp ( style :: String | r ) i)
textFieldRowFullwidth =
    [ HP.style "display: block;"
    ]

demoHelperText :: forall w i . HH.HTML w i
demoHelperText =
  HelperText.helperLine
    [ HelperText.helperText
      (HelperText.defaultConfig { persistent = true })
      "Helper Text"
    ]

demoHelperTextWithCharacterCounter :: forall w i . HH.HTML w i
demoHelperTextWithCharacterCounter =
  HelperText.helperLine
    [ HelperText.helperText
      (HelperText.defaultConfig { persistent = true })
      "Helper Text"
    , HelperText.characterCounter
    ]
