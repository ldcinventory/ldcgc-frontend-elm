module Components.Pagination exposing (view)

import Components.Icons as Icon
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Number.Bounded as Bounded exposing (Bounded)
import Set


type alias PagerOptions a =
    { innerWindow : Int
    , outerWindow : Int
    , pageNumberView : Int -> Bool -> a
    , gapView : a
    }


{-| Code blatantly stolen from elm-paginate 🤭 |
-}
elidedPager : PagerOptions b -> Bounded Int -> List b
elidedPager options currentPage_ =
    let
        currentPageNumber =
            Bounded.value currentPage_

        leftWindow =
            if options.outerWindow <= 0 then
                []

            else
                List.range
                    (Bounded.minBound currentPage_)
                    (Bounded.set (Bounded.minBound currentPage_ + (options.outerWindow - 1)) currentPage_ |> Bounded.value)

        rightWindow =
            if options.outerWindow <= 0 then
                []

            else
                List.range
                    (Bounded.set (Bounded.maxBound currentPage_ - (options.outerWindow - 1)) currentPage_ |> Bounded.value)
                    (Bounded.maxBound currentPage_)

        innerWindow =
            List.range
                (Basics.clamp (Bounded.minBound currentPage_) currentPageNumber (currentPageNumber - options.innerWindow))
                (Basics.clamp currentPageNumber (Bounded.maxBound currentPage_) (currentPageNumber + options.innerWindow))
    in
    leftWindow
        ++ innerWindow
        ++ rightWindow
        |> Set.fromList
        |> Set.toList
        |> groupWindows
        |> List.map (List.map (\i -> options.pageNumberView i (i == currentPageNumber)))
        |> List.intersperse [ options.gapView ]
        |> List.concat


groupWindows : List Int -> List (List Int)
groupWindows pages =
    List.foldl accumulateWindowGroups [] pages
        |> List.map (Tuple.mapSecond List.reverse)
        |> List.reverse
        |> List.map (\( x, xs ) -> x :: xs)


accumulateWindowGroups : Int -> List ( Int, List Int ) -> List ( Int, List Int )
accumulateWindowGroups page_ windows =
    case windows of
        [] ->
            [ ( page_, [] ) ]

        currentWindow :: remainingWindows ->
            let
                prevPage =
                    case List.head (Tuple.second currentWindow) of
                        Just prevPage_ ->
                            prevPage_

                        Nothing ->
                            Tuple.first currentWindow
            in
            if page_ - prevPage > 1 then
                ( page_, [] ) :: windows

            else
                Tuple.mapSecond (\list -> page_ :: list) currentWindow :: remainingWindows


view :
    { itemsPerPage : Int
    , numItems : Int
    , currentPage : Int
    , totalPages : Int
    , elementsThisPage : Int
    , next : msg
    , prev : msg
    }
    -> Html msg
view config =
    let
        prevDisabled : Bool
        prevDisabled =
            config.currentPage == 1

        nextDisabled : Bool
        nextDisabled =
            config.currentPage == config.totalPages

        pages : List String
        pages =
            elidedPager
                { innerWindow = 1
                , outerWindow = 1
                , pageNumberView = \index _ -> String.fromInt index
                , gapView = "..."
                }
                (Bounded.between 1 config.totalPages |> Bounded.set config.currentPage)
    in
    Html.nav
        [ Attr.class "flex flex-col md:flex-row justify-between items-start md:items-center space-y-3 md:space-y-0 p-4"
        , Attr.attribute "aria-label" "Table navigation"
        ]
        [ Html.span
            [ Attr.class "text-sm font-normal text-gray-500 dark:text-gray-400"
            ]
            [ Html.text "Showing "
            , Html.span
                [ Attr.class "font-semibold text-gray-900 dark:text-white"
                ]
                [ Html.text <| "1-" ++ String.fromInt config.elementsThisPage ]
            , Html.text " of "
            , Html.span
                [ Attr.class "font-semibold text-gray-900 dark:text-white"
                ]
                [ Html.text <| String.fromInt config.totalPages ]
            , Html.text " page/s"
            ]
        , Html.ul
            [ Attr.class "inline-flex items-stretch -space-x-px"
            ]
            (Html.li []
                [ Html.button
                    [ Events.onClick config.prev
                    , Attr.class "flex items-center justify-center disabled:opacity-50 h-full py-1.5 px-3 ml-0 text-gray-500 bg-white rounded-l-lg border border-gray-300  dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400"
                    , Attr.disabled prevDisabled

                    -- We only want hover styles if the button is not disabled
                    , Attr.classList
                        [ ( "hover:bg-gray-100 hover:text-gray-700 dark:hover:bg-gray-700 dark:hover:text-white"
                          , not prevDisabled
                          )
                        ]
                    ]
                    [ Html.span [ Attr.class "sr-only" ] [ Html.text "Previous" ]
                    , Icon.chevronLeft
                    ]
                ]
                :: (pages
                        |> List.map
                            (\page ->
                                Html.li []
                                    [ Html.a
                                        [ Attr.href "#"
                                        , Attr.attribute "aria-current" "page"
                                        , Attr.class "cursor-default flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                                        , Attr.classList
                                            [ ( "border border-primary-300 bg-primary-100 text-primary-700 dark:border-gray-700 dark:bg-gray-700 dark:text-white"
                                              , page == String.fromInt config.currentPage
                                              )
                                            ]
                                        ]
                                        [ Html.text page ]
                                    ]
                            )
                   )
                ++ [ Html.li []
                        [ Html.button
                            [ Attr.class "flex items-center justify-center disabled:opacity-50 h-full py-1.5 px-3 leading-tight text-gray-500 bg-white rounded-r-lg border border-gray-300  dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400"
                            , Attr.disabled nextDisabled

                            -- We only want hover styles if the button is not disabled
                            , Attr.classList
                                [ ( "hover:bg-gray-100 hover:text-gray-700 dark:hover:bg-gray-700 dark:hover:text-white"
                                  , not nextDisabled
                                  )
                                ]
                            , Events.onClick config.next
                            ]
                            [ Html.span [ Attr.class "sr-only" ] [ Html.text "Next" ]
                            , Icon.chevronRight
                            ]
                        ]
                   ]
            )
        ]
