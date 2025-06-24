module Widgets exposing 
    ( layout
    , link
    , blueLink
    , complexHeading
    , horizontalSeparator
    , markdownBody
    , MarkdownMsg(..)
    )

import Colors
import MarkdownText exposing (MarkdownText)
import Fontawesome
import Style
import SyntaxHighlight

import Browser
import Element as UI exposing (px)
import Element.Background as UI_Background
import Element.Border as UI_Border
import Element.Font as UI_Font
import Element.Input as UI_Input
import Element.Region as UI_Region
import Html
import Html.Attributes
import Markdown.Renderer.ElmUi as MarkdownRenderer exposing (Error(..))
import Markdown.Parser
import Markdown.Renderer
import Markdown.Block
import Markdown.Html
import Parser
import Parser.Advanced
import Url


layout : (String, UI.Element msg) -> Browser.Document msg
layout (title, content) =
    { title = title
    , body = 
        [ UI.layout 
            [ UI_Background.color Colors.background
            , UI_Font.color <| UI.rgb255 211 207 201
            , UI.width UI.fill
            , UI.height UI.fill
            ]
            <| UI.el 
                [ UI.width <| UI.maximum 750 UI.fill
                , UI.centerX
                , UI.padding 20
                ]
                content
        ]
    }


link : List (UI.Attribute msg) -> { url : String, label : UI.Element msg } -> UI.Element msg
link attributes args =
    UI.link (UI.mouseOver [ UI_Font.color Colors.linkBlue ] :: attributes) args


blueLink : List (UI.Attribute msg) -> { url : String, label : UI.Element msg } -> UI.Element msg
blueLink attributes args =
    UI.link (UI_Font.color Colors.linkBlue :: attributes) args


complexHeading : List (UI.Attribute msg) -> Int -> String -> List (UI.Element msg) -> UI.Element msg
complexHeading attributes level label children =
    let
        id =
            label
                |> String.trim
                |> String.toLower
                |> String.replace " " "-"
                |> Url.percentEncode

        font_size =
            case level of
                1 ->
                    32

                2 ->
                    28

                3 ->
                    26

                4 ->
                    24

                5 ->
                    22

                _ ->
                    20
    in
    UI.el
        [ UI.width UI.fill
        ]
    <|
        UI.el
            ([ UI_Font.size font_size
             , UI_Region.heading level
             , UI.htmlAttribute <| Html.Attributes.id id
             , UI.width UI.fill
             , UI.inFront <| if not <| String.isEmpty label
                then link
                    [ UI.paddingXY 10 0
                    , UI.centerY
                    , UI.alpha 0
                    , UI.mouseOver [ UI.alpha 1 ]
                    , UI.width UI.fill
                    , UI.moveLeft 50
                    ]
                    { url = "#" ++ id, label = Fontawesome.text [] "\u{F0C1}" }
                else UI.none
             ]
                ++ attributes
            )
        <|
            UI.paragraph [ UI.width UI.fill, UI_Font.bold ] children


horizontalSeparator : Int -> UI.Element msg
horizontalSeparator width =
    UI.el
        [ UI.height (px width)
        , UI_Background.color Colors.horizontalSeparator
        , UI.width UI.fill
        ]
        UI.none


type MarkdownMsg
    = MarkdownMsg_Run { language : String, code : String }


markdownBody : MarkdownText -> List (UI.Element MarkdownMsg)
markdownBody body =
    let
        parsed_markdown =
            parseMarkdown <| MarkdownText.source body
    in
    case parsed_markdown of
        Ok value ->
            value

        Err error ->
            [ UI.text <| "Error at parsing markdown: " ++ markdownErrorToString error ]


----------------------------------------------------------------------------------------------------------------------
-- Markdown impl

parseMarkdown : String -> Result Error (List (UI.Element MarkdownMsg))
parseMarkdown markdown_source =
    Markdown.Parser.parse markdown_source
        |> Result.mapError ParseError
        |> Result.andThen
            (\blocks ->
                Markdown.Renderer.render markdownRenderer blocks
                    |> Result.mapError RenderError
            )


markdownRenderer : Markdown.Renderer.Renderer (UI.Element MarkdownMsg)
markdownRenderer =
    let
        defaultRenderer =
            MarkdownRenderer.renderer
    in
    { defaultRenderer
        | html = Markdown.Html.oneOf []
        , paragraph = UI.paragraph [ UI_Font.justify, UI.width UI.fill ]
        , unorderedList = unorderedList
        , link = \{ destination } body -> blueLink [] { url = destination, label = UI.paragraph [] body }
        , heading = \{ level, rawText, children } -> complexHeading [ UI.paddingEach { top = 10, left = 0, bottom = 0, right = 0 } ] (Markdown.Block.headingLevelToInt level) rawText children
        , codeBlock = codeBlock
        , codeSpan = codeSpan
        , blockQuote = blockQuote
        , image = imageMarkdown
    }


unorderedListItem : Markdown.Block.ListItem (UI.Element msg) -> UI.Element msg
unorderedListItem (Markdown.Block.ListItem _ children) =
    let
        bullet =
            UI.el
                [ UI.paddingEach { top = 4, bottom = 0, left = 2, right = 8 }
                , UI.alignTop
                ]
            <|
                UI.text "•"
    in
    UI.row []
        [ bullet
        , UI.paragraph [ UI.width UI.fill ] children
        ]


unorderedList : List (Markdown.Block.ListItem (UI.Element msg)) -> UI.Element msg
unorderedList items =
    List.map unorderedListItem items
        |> UI.column [ UI.spacing 5 ]


codeSpan : String -> UI.Element msg
codeSpan raw_text = UI.el 
    [ UI_Font.family [ UI_Font.monospace ]
    , UI_Font.size Style.inlineMonospaceFontSize
    , UI_Background.color Colors.widgetBackground
    ] 
    <| UI.text raw_text


isRunnableLanguage : String -> Bool
isRunnableLanguage language_identifier = language_identifier == "sh" || language_identifier == "python"


codeBlock : { body : String, language : Maybe String } -> UI.Element MarkdownMsg
codeBlock { body, language } = 
    let
        maybe_syntax = Maybe.andThen SyntaxHighlight.syntax_for language
        color_to_string {r, g, b} = "rgb(" ++ String.fromInt r ++ ", " ++ String.fromInt g ++ ", " ++ String.fromInt b ++ ")"
        render block = Html.span [ Html.Attributes.style "color" (color_to_string block.color) ] [ Html.text block.text ]
        block_content = maybe_syntax
            |> Maybe.map (\syntax -> SyntaxHighlight.highlight syntax body)
            |> Maybe.map (List.map render)
            |> Maybe.withDefault [ Html.text body ]
        code_block = Html.pre [] block_content |> UI.html
        title language_identifier language_name = if isRunnableLanguage language_identifier
            then UI.row
                [ UI_Font.size Style.regularFontSize
                , UI.paddingXY 0 5
                , UI.width UI.fill
                , UI.spacing 5
                ]
                [ UI.text language_name
                , UI_Input.button
                    [ UI.alignRight
                    , UI_Background.color Colors.runButtonColor
                    , UI_Border.rounded 5 
                    , UI.paddingXY 10 5
                    , UI_Font.size Style.smallFontSize
                    , UI_Font.color Colors.black
                    ]
                    { label = UI.paragraph [] [ Fontawesome.text [] "\u{F04B}" {- fa-play -}, UI.text " Run" ]
                    , onPress = Just <| MarkdownMsg_Run { language = language_identifier, code = body }
                    }
                ]
            else UI.el
                [ UI_Font.size Style.regularFontSize
                , UI.paddingXY 0 5
                ]
            <| UI.text language_name
        content = case maybe_syntax of
            Nothing -> [ code_block ]
            Just syntax ->
                [ title syntax.identifier syntax.name
                , horizontalSeparator 1
                , code_block
                ]
    in
        UI.column
            [ UI_Border.width 1
            , UI_Border.color Colors.footerBorder
            , UI_Background.color Colors.widgetBackground
            , UI_Border.rounded 10
            , UI.paddingXY 15 0
            , UI.width UI.fill
            , UI_Font.size Style.blockMonospaceFontSize
            , UI.scrollbarX
            -- This is a hack to make UI.scrollbarX work. Otherwise the browser will make the div have a height of 1 px for some reason.
            , UI.htmlAttribute <| Html.Attributes.style "flex-basis" "auto"
            ]
            content

blockQuote : List (UI.Element msg) -> UI.Element msg
blockQuote paragraphs =
    UI.el [ UI.paddingXY 0 4 ] <|
        UI.column
            [ UI.padding 10
            , UI.spacing 20
            , UI_Border.widthEach { top = 0, bottom = 0, right = 0, left = 10 }
            , UI_Border.color Colors.blockQuoteLeftBar
            ]
            paragraphs


imageMarkdown : { src : String, alt : String, title : Maybe String } -> UI.Element msg
imageMarkdown { src, alt, title } =
    let 
        base = Html.img
            [ Html.Attributes.src src
            , Html.Attributes.alt alt
            , Html.Attributes.style "max-width" "100%"
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "margin-left" "auto"
            , Html.Attributes.style "margin-right" "auto"
            , Html.Attributes.title alt
            ] []
            |> UI.html

        withTitle : String -> UI.Element msg
        withTitle t =
            UI.column [ UI.spacingXY 0 8 ]
                [ base
                , UI.el [ UI.centerX ] <| UI.text t
                ]
    in
        Maybe.map withTitle title |> Maybe.withDefault base


type alias MarkdownParserError =
    Parser.Advanced.DeadEnd String Parser.Problem


markdownErrorToString : MarkdownRenderer.Error -> String
markdownErrorToString error =
    case error of
        ParseError parser_error ->
            "Parse error: " ++ markdownParserErrorsToString parser_error

        RenderError error_message ->
            "Render error: " ++ error_message


markdownParserErrorsToString : List MarkdownParserError -> String
markdownParserErrorsToString errors =
    errors
        |> List.map markdownParserErrorToString
        |> String.join "\n\n"


markdownParserErrorToString : MarkdownParserError -> String
markdownParserErrorToString error =
    let
        context_stack_as_string =
            error.contextStack
                |> List.map contextStackFrameToString
                |> List.map (\s -> "\n - " ++ s)
                |> String.join ""
    in
    "(row: " ++ String.fromInt error.row ++ " col: " ++ String.fromInt error.col ++ ") " ++ parserProblemToString error.problem ++ context_stack_as_string


contextStackFrameToString : { row : Int, col : Int, context : String } -> String
contextStackFrameToString frame =
    "(row: " ++ String.fromInt frame.row ++ " col: " ++ String.fromInt frame.col ++ ") " ++ frame.context


parserProblemToString : Parser.Problem -> String
parserProblemToString problem =
    case problem of
        Parser.Expecting str ->
            "Expecting " ++ str

        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol str ->
            "Expecting symbol " ++ str

        Parser.ExpectingKeyword str ->
            "Expecting keyword " ++ str

        Parser.ExpectingEnd ->
            "Expecting end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem str ->
            "Problem: " ++ str

        Parser.BadRepeat ->
            "Bad repeat"
