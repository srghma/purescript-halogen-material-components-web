module Demo.Pages.TopAppBarPages.Prominent where

import Demo.HOC.TopAppBarPage (TopAppBarPage)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.TopAppBar as TopAppBar
import Material.Classes.TopAppBar (mdc_top_app_bar____prominent_fixed_adjust, mdc_top_app_bar__action_item, mdc_top_app_bar__navigation_icon, mdc_top_app_bar__row, mdc_top_app_bar__section, mdc_top_app_bar__section____align_end, mdc_top_app_bar__section____align_start, mdc_top_app_bar__title)
import HalogenMWC.IconButton as IconButton

config :: TopAppBarPage
config =
    { fixedAdjust: mdc_top_app_bar____prominent_fixed_adjust
    , topAppBar:
        TopAppBar.topAppBar TopAppBar.Prominent TopAppBar.defaultConfig
            [ HH.section [ HP.class_ mdc_top_app_bar__row ]
                [ HH.section [ HP.classes [ mdc_top_app_bar__section, mdc_top_app_bar__section____align_start ] ]
                    [ IconButton.iconButtonMaterialIcons
                        (IconButton.defaultConfig
                            { additionalClasses = [ mdc_top_app_bar__navigation_icon ]
                            }
                        )
                        "menu"
                    , HH.span [ HP.class_ mdc_top_app_bar__title ] [ HH.text "Prominent" ]
                    ]
                , HH.section [ HP.classes [ mdc_top_app_bar__section, mdc_top_app_bar__section____align_end ] ]
                    [ IconButton.iconButtonMaterialIcons
                        (IconButton.defaultConfig
                            { additionalClasses = [ mdc_top_app_bar__action_item ]
                            }
                        )
                        "file_download"
                    , IconButton.iconButtonMaterialIcons
                        (IconButton.defaultConfig
                            { additionalClasses = [ mdc_top_app_bar__action_item ]
                            }
                        )
                        "print"
                    , IconButton.iconButtonMaterialIcons
                        (IconButton.defaultConfig
                            { additionalClasses = [ mdc_top_app_bar__action_item ]
                            }
                        )
                        "bookmark"
                    ]
                ]
            ]
    }
