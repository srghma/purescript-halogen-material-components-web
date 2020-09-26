module Demo.Pages.TextField where

import Material.Classes.Typography (mdc_typography____subtitle1)
import Protolude

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils (mkComponentStatic)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.TextField.Filled as TextField.Filled
import Demo.MkComponent.WithFocus as WithFocus
import Demo.Pages.TextField.Css as Css
import Demo.Pages.TextField.Hero as Demo.Pages.TextField.Hero

config :: CatalogPage
config =
    { title: "Text Field"
    , prelude: "Text fields allow users to input, edit, and select text. Text fields typically reside in forms but can appear in other places, like dialog boxes and search."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-text-fields"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-TextField"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-textfield"
        }
    , hero: Demo.Pages.TextField.Hero.component
    , content: WithFocus.mkComponent $ HH.div_
        [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Filled" ]
        -- | , filledTextFields
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Shaped Filled" ]
        -- | , shapedFilledTextFields
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Outlined" ]
        -- | , outlinedTextFields
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Shaped Outlined (TODO)" ]
        -- | , shapedOutlinedTextFields
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Text Fields without Label" ]
        -- | , textFieldsWithoutLabel
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Text Fields with Character Counter" ]
        -- | , textFieldsWithCharacterCounter
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Textarea" ]
        -- | , textareaTextField
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Full Width" ]
        -- | , fullwidthTextField
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Full Width Textarea" ]
        -- | , fullwidthTextareaTextField
        -- | , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Text Field" ]
        -- | , focusTextField
        ]
    }

-- | filledTextFields :: forall r w i . HH.HTML w i
-- | filledTextFields =
-- |   HH.div Css.styles.textFieldRow
-- |     [ HH.div Css.styles.textFieldContainer
-- |       [ TextField.filled
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.filled
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , leadingIcon = Just (TextField.iconMaterialIcons [] "event")
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.filled
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , trailingIcon = Just (TextField.iconMaterialIcons [] "delete")
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     ]

-- | shapedFilledTextFields :: forall r w i . HH.HTML w i
-- | shapedFilledTextFields =
-- |   HH.div Css.styles.textFieldRow
-- |     [ HH.div Css.styles.textFieldContainer
-- |       [ TextField.filled
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , additionalAttributes = [ HP.style "border-radius: 16px 16px 0 0;" ]
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.filled
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , leadingIcon = Just (TextField.iconMaterialIcons [] "event")
-- |           , additionalAttributes = [ HP.style "border-radius: 16px 16px 0 0;" ]
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.filled
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , trailingIcon = Just (TextField.iconMaterialIcons [] "delete")
-- |           , additionalAttributes = [ HP.style "border-radius: 16px 16px 0 0;" ]
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     ]

-- | outlinedTextFields :: forall r w i . HH.HTML w i
-- | outlinedTextFields =
-- |   HH.div Css.styles.textFieldRow
-- |     [ HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , leadingIcon = Just (TextField.iconMaterialIcons [] "event")
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , trailingIcon = Just (TextField.iconMaterialIcons [] "delete")
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     ]

-- | shapedOutlinedTextFields :: forall r w i . HH.HTML w i
-- | shapedOutlinedTextFields =
-- |   HH.div Css.styles.textFieldRow
-- |     [ HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , leadingIcon = Just (TextField.iconMaterialIcons [] "event")
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { label = Just "Standard"
-- |           , trailingIcon = Just (TextField.iconMaterialIcons [] "delete")
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     ]

-- | textFieldsWithoutLabel :: forall r w i . HH.HTML w i
-- | textFieldsWithoutLabel =
-- |   HH.div Css.styles.textFieldRow
-- |     [ HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined TextField.defaultConfig
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { leadingIcon = Just (TextField.iconMaterialIcons [] "event")
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { trailingIcon = Just (TextField.iconMaterialIcons [] "delete")
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     ]

-- | textFieldsWithCharacterCounter :: forall r w i . HH.HTML w i
-- | textFieldsWithCharacterCounter =
-- |   HH.div Css.styles.textFieldRow
-- |     [ HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig { maxLength = Just 18 })
-- |       , demoHelperTextWithCharacterCounter
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { leadingIcon = Just (TextField.iconMaterialIcons [] "event")
-- |           , maxLength = Just 18
-- |           }
-- |         )
-- |       , demoHelperTextWithCharacterCounter
-- |       ]
-- |     , HH.div Css.styles.textFieldContainer
-- |       [ TextField.outlined
-- |         (TextField.defaultConfig
-- |           { trailingIcon = Just (TextField.iconMaterialIcons [] "delete")
-- |           , maxLength = Just 18
-- |           }
-- |         )
-- |       , demoHelperTextWithCharacterCounter
-- |       ]
-- |     ]

-- | textareaTextField :: forall r w i . HH.HTML w i
-- | textareaTextField =
-- |   HH.div Css.styles.textFieldContainer
-- |     [ TextArea.outlined (TextArea.defaultConfig { label = Just "Standard" })
-- |     , demoHelperText
-- |     ]

-- | fullwidthTextField :: forall r w i . HH.HTML w i
-- | fullwidthTextField =
-- |   HH.div Css.styles.textFieldContainer
-- |     [ TextField.filled
-- |       (TextField.defaultConfig
-- |         { placeholder = Just "Standard"
-- |         , fullwidth = true
-- |         }
-- |       )
-- |     , demoHelperText
-- |     ]

-- | fullwidthTextareaTextField :: forall r w i . HH.HTML w i
-- | fullwidthTextareaTextField =
-- |   HH.div Css.styles.textFieldRowFullwidth
-- |     [ HH.div Css.styles.textFieldContainer
-- |       [ TextArea.outlined
-- |         (TextArea.defaultConfig
-- |           { label = Just "Standard"
-- |           , fullwidth = true
-- |           }
-- |         )
-- |       , demoHelperText
-- |       ]
-- |     ]

-- | focusTextField :: forall r w i . HH.HTML w WithFocus.Action
-- | focusTextField =
-- |   HH.div_
-- |     [ TextField.filled
-- |       (TextField.defaultConfig
-- |         { additionalAttributes = [ HP.id_ "my-text-field" ]
-- |         }
-- |       )
-- |     , HH.text "\x00A0"
-- |     , Button.buttonView Button.Raised
-- |       (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ WithFocus.Focus "my-text-field") ] })
-- |       [ HH.text "Focus" ]
-- |     ]

-- | demoHelperText :: forall w i . HH.HTML w i
-- | demoHelperText =
-- |   HelperText.helperLine
-- |     [ HelperText.helperText
-- |       (HelperText.defaultConfig { persistent = true })
-- |       "Helper Text"
-- |     ]

-- | demoHelperTextWithCharacterCounter :: forall w i . HH.HTML w i
-- | demoHelperTextWithCharacterCounter =
-- |   HelperText.helperLine
-- |     [ HelperText.helperText
-- |       (HelperText.defaultConfig { persistent = true })
-- |       "Helper Text"
-- |     , HelperText.characterCounter
-- |     ]
