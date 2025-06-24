module MarkdownText exposing (MarkdownText(..), source, removeFormatting, toHtmlString, escapeTextForXml)

import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer

type MarkdownText = MarkdownText String


source : MarkdownText -> String
source markdown = case markdown of
    MarkdownText src -> src


removeFormatting : MarkdownText -> String
removeFormatting body = 
        source body
        |> Markdown.Parser.parse
        |> Result.toMaybe
        |> Maybe.andThen
            (\blocks ->
                Markdown.Renderer.render removeFormattingRenderer blocks
                    |> Result.map (String.join "\n")
                    |> Result.toMaybe
            )
        |> Maybe.withDefault (source body)


toHtmlString : MarkdownText -> String
toHtmlString body = 
    source body
        |> Markdown.Parser.parse
        |> Result.toMaybe
        |> Maybe.andThen
            (\blocks ->
                Markdown.Renderer.render toHtmlStringRenderer blocks
                    |> Result.map (String.join "\n")
                    |> Result.toMaybe
            )
        |> Maybe.withDefault (source body)


escapeTextForXml : String -> String
escapeTextForXml text = text
    |> String.replace "<" "&lt;"
    |> String.replace ">" "&gt;"
    |> String.replace "&" "&amp;"
    |> String.replace "'" "&apos;"
    |> String.replace "\"" "&quot;"


----------------------------------------------------------------------------------------------------------------
-- removeFormatting

superscriptNumber : Char -> Char
superscriptNumber s = case s of
    '0' -> '⁰'
    '1' -> '¹'
    '2' -> '²'
    '3' -> '³'
    '4' -> '⁴'
    '5' -> '⁵'
    '6' -> '⁶'
    '7' -> '⁷'
    '8' -> '⁸'
    '9' -> '⁹'
    c -> c


removeFormattingRenderer : Markdown.Renderer.Renderer (String)
removeFormattingRenderer = 
    { heading = \{ rawText } -> rawText
    , paragraph = String.join ""
    , blockQuote = String.join ""
    , html = Markdown.Html.oneOf
        [ Markdown.Html.tag "youtube"
            (\_ _ ->
                ""
            )
            |> Markdown.Html.withAttribute "id"
        , Markdown.Html.tag "ref"
            (\id _ ->
                String.map superscriptNumber id
            )
            |> Markdown.Html.withAttribute "id"
        , Markdown.Html.tag "footnote"
            (\id _ ->
                "[" ++ id ++ "]"
            )
            |> Markdown.Html.withAttribute "id"
        ]
    , text = \x -> x
    , codeSpan = \x -> x
    , strong = String.join ""
    , emphasis = String.join ""
    , strikethrough = String.join ""
    , hardLineBreak = "\n\n"
    , link = \_ body -> String.join "" body
    , image = \_ -> ""
    , unorderedList = \items -> String.join "\n" <| List.map removeFormattingUnorderedListItem items
    , orderedList = \startingIndex items -> String.join "\n" <| List.map2
        (\number body -> String.fromInt number ++ ". " ++ String.join "" body)
        (List.range startingIndex (List.length items))
        items
    , codeBlock = \{ body } -> body
    , thematicBreak = ""
    , table = \rows -> String.join "\n" rows
    , tableHeader = \children -> String.join "" children
    , tableBody = \rows -> String.join "\n" rows
    , tableRow = \columns -> String.join " | " columns
    , tableCell = \_ children -> String.join "" children
    , tableHeaderCell = \_ children -> String.join "" children
    }


removeFormattingUnorderedListItem : Markdown.Block.ListItem String -> String
removeFormattingUnorderedListItem (Markdown.Block.ListItem task children) = 
    let
        bullet =
            case task of
                Markdown.Block.IncompleteTask ->
                    "[ ] "

                Markdown.Block.CompletedTask ->
                    "[x] "

                Markdown.Block.NoTask ->
                    "- "
    in
        bullet ++ String.join "" children


----------------------------------------------------------------------------------------------------------------
-- toHtmlString

toHtmlStringRenderer : Markdown.Renderer.Renderer (String)
toHtmlStringRenderer = 
    { heading = toHtmlStringHeading
    , paragraph = \children -> "<p>" ++ String.join "" children ++ "</p>"
    , blockQuote = \children -> "<blockquote>" ++ String.join "" children ++ "</blockquote>"
    , html = Markdown.Html.oneOf
        [ Markdown.Html.tag "youtube"
            (\id _ ->
                "<iframe src=\"https://www.youtube.com/embed/" ++ id ++ "\" allow=\"accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen=\"true\" style=\"aspect-ratio: 16 / 9;\"></iframe>"
            )
            |> Markdown.Html.withAttribute "id"
        , Markdown.Html.tag "ref"
            (\id _ ->
                "<sup>[" ++ id ++ "]</sup>"
            )
            |> Markdown.Html.withAttribute "id"
        , Markdown.Html.tag "footnote"
            (\id _ ->
                "[" ++ id ++ "]"
            )
            |> Markdown.Html.withAttribute "id"
        ]
    , text = \x -> escapeTextForXml x
    , codeSpan = \text -> "<code>" ++ escapeTextForXml text ++ "</code>"
    , strong = \children -> "<b>" ++ String.join "" children ++ "</b>"
    , emphasis = \children -> "<i>" ++ String.join "" children ++ "</i>"
    , strikethrough = \children -> "<s>" ++ String.join "" children ++ "</s>"
    , hardLineBreak = "<br>"
    , link = \{destination} body -> "<a href=\"" ++ destination ++ "\">" ++ String.join "" body ++ "</a>"
    , image = \{src, alt} -> "<img src=\"" ++ src ++ "\" alt=\"" ++ escapeTextForXml alt ++ "\" />"
    , unorderedList = \items -> "<ul>" ++ String.join "" (List.map toHtmlStringUnorderedListItem items) ++ "</ul>"
    , orderedList = \startingIndex items -> "<ol start=\"" ++ String.fromInt startingIndex ++ "\">" ++ String.join "" (List.map toHtmlStringListItem items) ++ "</ol>"
    , codeBlock = \{body} -> "<code>" ++ escapeTextForXml body ++ "</code>"
    , thematicBreak = "<hr />"
    , table = \rows -> "<table>" ++ String.join "" rows ++ "</table>"
    , tableHeader = \children -> "<tr>" ++ String.join "" children ++ "</tr>"
    , tableBody = \rows -> String.join "\n" rows
    , tableRow = \columns -> "<tr>" ++ String.join "" columns ++ "</tr>"
    , tableCell = \_ children -> "<td>" ++ String.join "" children ++ "</td>"
    , tableHeaderCell = \_ children -> "<th>" ++ String.join "" children ++ "</th>"
    }


toHtmlStringHeading : { level : Markdown.Block.HeadingLevel, rawText : String, children : List String } -> String
toHtmlStringHeading { level, children } = 
    let
        levelStr = String.fromInt <| Markdown.Block.headingLevelToInt level
        childrenStr = String.join "" children
    in
        "<h" ++ levelStr ++ ">" ++ childrenStr ++ "</h" ++ levelStr ++ ">"


toHtmlStringUnorderedListItem : Markdown.Block.ListItem String -> String
toHtmlStringUnorderedListItem (Markdown.Block.ListItem _ children) = toHtmlStringListItem children

toHtmlStringListItem : List String -> String
toHtmlStringListItem children = "<li>" ++ String.join "" children ++ "</li>"
