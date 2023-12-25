module Pages.Home_ exposing (page)

import Html.Styled as Html
import View exposing (View)


page : View msg
page =
    { title = "Login"
    , body = [ Html.text "Hello, world!" ]
    }
