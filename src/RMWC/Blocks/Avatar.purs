module RMWC.Blocks.Avatar where

import Protolude (($), (<>))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML (ClassName)
import RMWC.Classes.Icon (rmwc_icon, rmwc_icon____component)
import RMWC.Classes.Avatar (rmwc_avatar, rmwc_avatar____has_image, rmwc_avatar____large, rmwc_avatar____medium, rmwc_avatar____small, rmwc_avatar____square, rmwc_avatar____xlarge, rmwc_avatar____xsmall, rmwc_avatar__icon, rmwc_avatar__text, rmwc_avatar__text_inner)
import MaterialIconsFont.Classes (material_icons)
import Protolude.Url (Url)
import Protolude.Url as Url

data Size
  = Xsmall
  | Small
  | Medium
  | Large
  | Xlarge

sizeToClassName :: Size -> ClassName
sizeToClassName Xsmall = rmwc_avatar____xsmall
sizeToClassName Small = rmwc_avatar____small
sizeToClassName Medium = rmwc_avatar____medium
sizeToClassName Large = rmwc_avatar____large
sizeToClassName Xlarge = rmwc_avatar____xlarge

foreign import getInitialsForName :: String -> String

containToStyle :: Boolean -> String
containToStyle true = "contain"
containToStyle false = "cover"

squareToClassName :: Boolean -> Array ClassName
squareToClassName true = [ rmwc_avatar____square ]
squareToClassName false = []

avatarImage ::
  forall w i.
  { size :: Size
  , url :: Url
  , name :: String
  , square :: Boolean
  , contain :: Boolean
  } ->
  HH.HTML w i
avatarImage config =
  HH.span
    [ HP.title config.name
    , HP.classes
        $ [ rmwc_icon
          , rmwc_icon____component
          , material_icons
          , rmwc_avatar
          , sizeToClassName config.size
          , rmwc_avatar____has_image
          ]
        <> squareToClassName config.square
    ]
    [ HH.div
        [ HP.classes [ rmwc_avatar__icon ]
        , HP.attr (AttrName "style") $ "background-image: url(" <> Url.unUrl config.url <> "); background-size: " <> containToStyle config.contain <> ";"
        ]
        []
    , HH.div
        [ HP.classes [ rmwc_avatar__text ] -- TODO: image wont be visible without it (width is 0)
        ]
        []
    ]

avatarInitials ::
  forall w i.
  { size :: Size
  , name :: String
  , square :: Boolean
  , contain :: Boolean
  } ->
  HH.HTML w i
avatarInitials config =
  HH.span
    [ HP.title config.name
    , HP.classes
        $ [ rmwc_icon
          , rmwc_icon____component
          , material_icons
          , rmwc_avatar
          , sizeToClassName config.size
          ]
        <> squareToClassName config.square
    ]
    [ HH.div
        [ HP.class_ rmwc_avatar__text ]
        [ HH.div
            [ HP.class_ rmwc_avatar__text_inner ]
            [ HH.text (getInitialsForName config.name) ]
        ]
    ]
