module Demo.Pages.TopAppBarPages.Fixed where

import Demo.HOC.TopAppBarPage
import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.TopAppBar as TopAppBar
import Demo.Utils
import Material.Classes.TopAppBar
import HalogenMWC.IconButton as IconButton

config :: TopAppBarPage
config =
    { fixedAdjust: mdc_top_app_bar____fixed_adjust
    , topAppBar:
      TopAppBar.topAppBar TopAppBar.Regular (TopAppBar.defaultConfig { fixed = true })
            [ HH.section [ HP.class_ mdc_top_app_bar__row ]
                [ HH.section [ HP.classes [ mdc_top_app_bar__section, mdc_top_app_bar__section____align_start ] ]
                    [ IconButton.iconButtonMaterialIcons
                        (IconButton.defaultConfig
                            { additionalClasses = [ mdc_top_app_bar__navigation_icon ]
                            }
                        )
                        "menu"
                    , HH.span [ HP.class_ mdc_top_app_bar__title ] [ HH.text "Fixed" ]
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
