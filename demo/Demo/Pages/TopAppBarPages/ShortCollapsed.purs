module Demo.Pages.TopAppBarPages.ShortCollapsed where

import Demo.HOC.TopAppBarPage (TopAppBarPage)
import HalogenMWC.IconButton as IconButton
import HalogenMWC.TopAppBar as TopAppBar
import Material.Classes.TopAppBar (mdc_top_app_bar____short_fixed_adjust, mdc_top_app_bar__action_item, mdc_top_app_bar__navigation_icon, mdc_top_app_bar__row, mdc_top_app_bar__section, mdc_top_app_bar__section____align_end, mdc_top_app_bar__section____align_start)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

config :: TopAppBarPage
config =
    { fixedAdjust: mdc_top_app_bar____short_fixed_adjust
    , topAppBar:
        TopAppBar.topAppBar TopAppBar.ShortCollapsed TopAppBar.defaultConfig
            [ HH.section
              [ HP.class_ mdc_top_app_bar__row ]
              [ HH.section
                [ HP.classes [ mdc_top_app_bar__section, mdc_top_app_bar__section____align_start ] ]
                [ IconButton.iconButtonMaterialIcons
                    (IconButton.defaultConfig
                      { additionalClasses = [ mdc_top_app_bar__navigation_icon ]
                      }
                    )
                    "menu"
                ]
              , HH.section
                [ HP.classes [ mdc_top_app_bar__section, mdc_top_app_bar__section____align_end ] ]
                [ IconButton.iconButtonMaterialIcons
                    (IconButton.defaultConfig
                      { additionalClasses = [ mdc_top_app_bar__action_item ]
                      }
                    )
                    "file_download"
                ]
              ]
            ]
    }
