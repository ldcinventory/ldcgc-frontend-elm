module Pages.Home_ exposing (page)

import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Tailwind.Utilities as Tw
import View exposing (View)


page : View msg
page =
    { title = "LDC GC Homepage"
    , body =
        [ Html.div
            [ Attr.style "font-family" "'Libre Barcode 128'"
            , Attr.css [ Tw.text_4xl, Tw.text_center, Tw.mt_8 ]
            ]
            [ Html.text "Hello, world!"
            ]
        ]
    }
