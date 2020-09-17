module Demo.Pages.Button where

import Protolude

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.MkComponent.WithFocus as WithFocus
import Demo.Pages.Button.Css as Demo.Pages.Button.Css
import Demo.Utils (mkComponentStatic)
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.Button.Link as Button.Link
import Material.Classes.Typography (mdc_typography____subtitle1)

type State = Unit

type Query = Const Void

type Input = Unit

type Message = Void

config :: CatalogPage
config =
  { title: "Button"
  , prelude: "Buttons communicate an action a user can take. They are typically placed throughout your UI, in places like dialogs, forms, cards, and toolbars."
  , resources:
    { materialDesignGuidelines: Just "https://material.io/go/design-buttons"
    , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Button"
    , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-button"
    }
  , hero:
    let
      config = Button.defaultConfig { additionalClasses = [ Demo.Pages.Button.Css.styles.heroMargin ] }

      render =
        HH.div_
          [ HH.slot_ (SProxy :: SProxy "text-button") unit Button.button ({ variant: Button.Text, config, content: [ HH.text "Text" ] })
          , HH.slot_ (SProxy :: SProxy "raised-button") unit Button.button ({ variant: Button.Raised, config, content: [ HH.text "Raised" ] })
          , HH.slot_ (SProxy :: SProxy "unelevated-button") unit Button.button ({ variant: Button.Unelevated, config, content: [ HH.text "Unelevated" ] })
          , HH.slot_ (SProxy :: SProxy "outlined-button") unit Button.button ({ variant: Button.Outlined, config, content: [ HH.text "Outlined" ] })
          ]
    in
      mkComponentStatic render
  , content:
    let
      render :: HH.ComponentHTML WithFocus.Action _ Aff
      render =
        HH.div_ $
          [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Text Button" ]

          , HH.slot_ (SProxy :: SProxy "text-button-default") unit Button.button
            { variant: Button.Text
            , config: Button.defaultConfig { additionalClasses = buttonsRow.default.additionalClasses }
            , content: buttonsRow.default.html
            }
          , HH.slot_ (SProxy :: SProxy "text-button-dense") unit Button.button
            { variant: Button.Text
            , config: Button.defaultConfig { additionalClasses = buttonsRow.dense.additionalClasses }
            , content: buttonsRow.dense.html
            }
          , HH.slot_ (SProxy :: SProxy "text-button-icon") unit Button.button
            { variant: Button.Text
            , config: Button.defaultConfig { additionalClasses = buttonsRow.icon.additionalClasses }
            , content: buttonsRow.icon.html
            }

          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Raised Button" ]

          , HH.slot_ (SProxy :: SProxy "raised-button-default") unit Button.button
            { variant: Button.Raised
            , config: Button.defaultConfig { additionalClasses = buttonsRow.default.additionalClasses }
            , content: buttonsRow.default.html
            }
          , HH.slot_ (SProxy :: SProxy "raised-button-dense") unit Button.button
            { variant: Button.Raised
            , config: Button.defaultConfig { additionalClasses = buttonsRow.dense.additionalClasses }
            , content: buttonsRow.dense.html
            }
          , HH.slot_ (SProxy :: SProxy "raised-button-icon") unit Button.button
            { variant: Button.Raised
            , config: Button.defaultConfig { additionalClasses = buttonsRow.icon.additionalClasses }
            , content: buttonsRow.icon.html
            }

          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Unelevated Button" ]

          , HH.slot_ (SProxy :: SProxy "unelevated-button-default") unit Button.button
            { variant: Button.Unelevated
            , config: Button.defaultConfig { additionalClasses = buttonsRow.default.additionalClasses }
            , content: buttonsRow.default.html
            }
          , HH.slot_ (SProxy :: SProxy "unelevated-button-dense") unit Button.button
            { variant: Button.Unelevated
            , config: Button.defaultConfig { additionalClasses = buttonsRow.dense.additionalClasses }
            , content: buttonsRow.dense.html
            }
          , HH.slot_ (SProxy :: SProxy "unelevated-button-icon") unit Button.button
            { variant: Button.Unelevated
            , config: Button.defaultConfig { additionalClasses = buttonsRow.icon.additionalClasses }
            , content: buttonsRow.icon.html
            }

          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Outlined Button" ]

          , HH.slot_ (SProxy :: SProxy "outlined-button-default") unit Button.button
            { variant: Button.Outlined
            , config: Button.defaultConfig { additionalClasses = buttonsRow.default.additionalClasses }
            , content: buttonsRow.default.html
            }
          , HH.slot_ (SProxy :: SProxy "outlined-button-dense") unit Button.button
            { variant: Button.Outlined
            , config: Button.defaultConfig { additionalClasses = buttonsRow.dense.additionalClasses }
            , content: buttonsRow.dense.html
            }
          , HH.slot_ (SProxy :: SProxy "outlined-button-icon") unit Button.button
            { variant: Button.Outlined
            , config: Button.defaultConfig { additionalClasses = buttonsRow.icon.additionalClasses }
            , content: buttonsRow.icon.html
            }

          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Shaped Button" ]

          , HH.slot_ (SProxy :: SProxy "outlined-button-default") unit Button.button
            { variant: Button.Outlined
            , config: Button.defaultConfig { additionalClasses = buttonsRow.default.additionalClasses <>  [ Demo.Pages.Button.Css.styles.shapedButtons ] }
            , content: buttonsRow.default.html
            }
          , HH.slot_ (SProxy :: SProxy "outlined-button-dense") unit Button.button
            { variant: Button.Outlined
            , config: Button.defaultConfig { additionalClasses = buttonsRow.dense.additionalClasses <> [ Demo.Pages.Button.Css.styles.shapedButtons ] }
            , content: buttonsRow.dense.html
            }
          , HH.slot_ (SProxy :: SProxy "outlined-button-icon") unit Button.button
            { variant: Button.Outlined
            , config: Button.defaultConfig { additionalClasses = buttonsRow.icon.additionalClasses <> [ Demo.Pages.Button.Css.styles.shapedButtons ] }
            , content: buttonsRow.icon.html
            }

          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Link Button" ]
          ]

          <> linkButtons

          <> [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Button" ]
             ]

          <> focusButton
    in
      WithFocus.mkComponent render
  }

linkButtons :: Array (HH.ComponentHTML WithFocus.Action _ Aff)
linkButtons =
  let
    additionalAttributes = [ HP.href "#buttons" ]
  in
    [ HH.slot_ (SProxy :: SProxy "link-default") unit Button.Link.buttonLink
      { variant: Button.Text
      , config: Button.Link.defaultConfig { additionalAttributes = additionalAttributes, additionalClasses = buttonsRow.default.additionalClasses }
      , content: buttonsRow.default.html
      }
    , HH.slot_ (SProxy :: SProxy "link-dense") unit Button.Link.buttonLink
      { variant: Button.Text
      , config: Button.Link.defaultConfig { additionalAttributes = additionalAttributes, additionalClasses = buttonsRow.dense.additionalClasses }
      , content: buttonsRow.dense.html
      }
    , HH.slot_ (SProxy :: SProxy "link-icon") unit Button.Link.buttonLink
      { variant: Button.Text
      , config: Button.Link.defaultConfig { additionalAttributes = additionalAttributes, additionalClasses = buttonsRow.icon.additionalClasses }
      , content: buttonsRow.icon.html
      }
    ]

focusButton :: Array (HH.ComponentHTML WithFocus.Action _ Aff)
focusButton =
  [ HH.slot_ (SProxy :: SProxy "focus-target-1") unit Button.button
    { variant: Button.Raised
    , config: Button.defaultConfig { additionalAttributes = [ HP.id_ "focus-target-1" ] }
    , content: [ HH.text "Button" ]
    }

  , HH.text "\x00A0"

  , HH.slot (SProxy :: SProxy "focus-maker-1") unit
    Button.button
    { variant: Button.Raised
    , config: Button.defaultConfig
    , content: [ HH.text "Focus with ripple" ]
    }
    (case _ of
          Button.Clicked -> WithFocus.Focus "focus-target-1"
    )

  , HH.text "\x00A0"

  , HH.slot_ (SProxy :: SProxy "focus-target-2") unit Button.Link.buttonLink
    { variant: Button.Raised
    , config: Button.Link.defaultConfig { additionalAttributes = [ HP.id_ "focus-target-2", HP.href "#buttons" ] }
    , content: [ HH.text "Link button" ]
    }

  , HH.text "\x00A0"

  , Button.buttonView Button.Raised (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ WithFocus.Focus "focus-target-2") ] }) [ HH.text "Focus without ripple" ]
  ]

buttonsRow =
  { default:
    { additionalClasses: [ Demo.Pages.Button.Css.styles.rowMargin ]
    , html: [ HH.text "Default" ]
    }
  , dense:
    { additionalClasses: [ Demo.Pages.Button.Css.styles.rowMargin, Demo.Pages.Button.Css.styles.my_button_dense ]
    , html: [ HH.text "Dense" ]
    }
  , icon:
    { additionalClasses: [ Demo.Pages.Button.Css.styles.rowMargin ]
    , html: [ Button.buttonIconMaterialIcons "favorite", HH.text "Icon" ]
    }
  }
