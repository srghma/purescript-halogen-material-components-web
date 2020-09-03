module Demo.Pages.TopAppBarPages.ShortCollapsed where

import Demo.HOC.TopAppBarPage
import HalogenMWC.IconButton as IconButton
import HalogenMWC.TopAppBar as TopAppBar
import Demo.Utils
import Material.Classes.TopAppBar
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.IconButton as IconButton

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
