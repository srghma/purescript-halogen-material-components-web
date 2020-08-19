module Material.Tab.Internal exposing
    ( Config(..)
    , Content
    , Tab(..)
    )

import Html


data Config msg
    = Config
        { active :: Bool
        , additionalAttributes :: List (Html.Attribute msg)
        , onClick :: Maybe msg
        , content :: Content
        }


data Content =
    { label :: String
    , icon :: Maybe String
    }


data Tab msg
    = Tab (Config msg)
