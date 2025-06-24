module SyntaxHighlight exposing (Syntax, ColoredText, Color, rgb, highlight, syntax_for, supported_syntaxes, syntax_cpp, syntax_elm)

import Parser exposing (Parser)
import Parser exposing ((|.), (|=))
import Parser
import Set

type alias Color =
    { r : Int
    , g : Int
    , b : Int
    }

rgb : Int -> Int -> Int -> Color
rgb r g b = { r = r, g = g, b = b }

type alias Syntax = 
    { name : String
    , identifier : String
    , default_color : Color
    , keywords : List String
    , keyword_color : Color
    , comments : ColorCategory
    , string_literals : ColorCategory
    , number_literal_color : Color
    , punctuation : List String
    , punctuation_color : Color
    , other_colors : List ColorCategory
    }

type alias RangeSyntax =
    { start : String
    , end : String
    , escapes : List String
    }

type alias ColorCategory =
    { syntax : List RangeSyntax
    , color : Color
    }

type alias ColoredText =
    { text : String
    , color : Color
    }

highlight : Syntax -> String -> List ColoredText
highlight syntax text =
    if text == ""
        then []
        else text
            |> Parser.run (make_parser syntax)
            |> Result.withDefault [ { text = text, color = syntax.default_color } ]


make_parser : Syntax -> Parser (List ColoredText)
make_parser syntax =
    let
        category_parsers = List.map make_parser_for_category <| [ syntax.comments, syntax.string_literals ] ++ syntax.other_colors
        punctuation_parsers = List.map (make_parser_for_punctuation syntax.punctuation_color) syntax.punctuation
        keyword_parsers = List.map (make_parser_for_keyword syntax.keyword_color) syntax.keywords
        other_parsers = [ number_parser syntax.number_literal_color, default_parser syntax.default_color ]
        parse_one = Parser.oneOf (category_parsers ++ punctuation_parsers ++ keyword_parsers ++ other_parsers)
        step : List ColoredText -> Parser (Parser.Step (List ColoredText) (List ColoredText))
        step state =
            let
                update_state next = case state of
                    [] -> [next]
                    (last::rest) -> if last.color == next.color
                        then { text = last.text ++ next.text, color = last.color } :: rest
                        else next :: state

                merge_trailing_whitespace trailing_whitespace = case state of 
                    [] -> [ { text = trailing_whitespace, color = syntax.default_color } ]
                    (last::rest) -> { text = last.text ++ trailing_whitespace, color = last.color } :: rest
            in
                whitespace_parser |> Parser.andThen (\trailing_whitespace ->
                    Parser.oneOf
                    [ parse_one |> Parser.map (merge_whitespace trailing_whitespace >> update_state >> Parser.Loop)
                    , Parser.succeed () |> Parser.map (\_ -> Parser.Done (trailing_whitespace |> merge_trailing_whitespace |> List.reverse))
                    ]
                )
    in
        Parser.loop [] step

make_parser_for_category : ColorCategory -> Parser ColoredText
make_parser_for_category category = 
    let
        parser syntax = 
            Parser.symbol syntax.start 
            |. Parser.chompUntilEndOr syntax.end
            |. Parser.oneOf [ Parser.token syntax.end, Parser.succeed () ]
            |> Parser.getChompedString
            |> Parser.map (\s -> { text = s, color = category.color })
    in
        Parser.oneOf <| List.map parser category.syntax

make_parser_for_punctuation : Color -> String -> Parser ColoredText
make_parser_for_punctuation color punctuation = 
    Parser.symbol punctuation
    |> Parser.getChompedString
    |> Parser.map (\s -> { text = s, color = color })

make_parser_for_keyword : Color -> String -> Parser ColoredText
make_parser_for_keyword color keyword =
    Parser.keyword keyword
    |> Parser.getChompedString
    |> Parser.map (\s -> { text = s, color = color })

number_parser : Color -> Parser ColoredText
number_parser color = Parser.number
    { int = Just (always ())
    , hex = Just (always ())
    , octal = Nothing
    , binary = Just (always ())
    , float = Just (always ())
    }
    |> Parser.getChompedString
    |> Parser.map (\s -> { text = s, color = color })
    |> Parser.backtrackable

default_parser : Color -> Parser ColoredText
default_parser color =
    Parser.variable
    { start = \c -> Char.toCode c > 127 || Char.isAlpha c || c == '_'
    , inner = \c -> Char.toCode c > 127 || Char.isAlphaNum c || c == '_'
    , reserved = Set.empty
    }
    |> Parser.map (\s -> { text = s, color = color })

whitespace_parser : Parser String
whitespace_parser = Parser.chompWhile (\c -> c <= ' ') |> Parser.getChompedString

merge_whitespace : String -> ColoredText -> ColoredText
merge_whitespace whitespace colored_text = { text = whitespace ++ colored_text.text, color = colored_text.color }

syntax_for : String -> Maybe Syntax
syntax_for identifier = supported_syntaxes
    |> List.filter (\syntax -> syntax.identifier == identifier)
    |> List.head

supported_syntaxes : List Syntax
supported_syntaxes = [ syntax_cpp, syntax_elm, syntax_sh ]

syntax_cpp : Syntax
syntax_cpp =
    { name = "C++"
    , identifier = "cpp"
    , default_color = rgb 220 220 220
    , keywords = 
        [ "alignas", "alignof", "asm", "auto", "bool", "break", "case", "catch", "char", "char8_t", "char16_t", "char32_t", "class"
        , "concept", "const", "consteval", "constexpr", "constinit", "const_cast", "continue", "co_await", "co_return", "co_yield"
        , "decltype", "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern", "false"
        , "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable", "namespace", "new", "noexcept", "nullptr"
        , "operator", "private", "protected", "public", "register", "reinterpret_cast", "requires", "return", "short", "signed"
        , "sizeof", "static", "static_assert", "static_cast", "struct", "switch", "template", "this", "thread_local", "throw", "true"
        , "try", "typedef", "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while"
        ]
    , keyword_color = rgb 65 151 214
    , comments = { syntax = 
        [ { start = "/*", end = "*/", escapes = [] }
        , { start = "//", end = "\n", escapes = [] }
        ], color = rgb 82 165 72 }
    , string_literals = { syntax = 
        [ { start = "\"", end = "\"", escapes = ["\\\""] }
        , { start = "R\"(", end = ")\"", escapes = [] }
        , { start = "L\"", end = "\"", escapes = ["\\\""] }
        , { start = "LR\"(", end = ")\"", escapes = [] }
        , { start = "u8\"", end = "\"", escapes = ["\\\""] }
        , { start = "u8R\"(", end = ")\"", escapes = [] }
        , { start = "u\"", end = "\"", escapes = ["\\\""] }
        , { start = "uR\"(", end = ")\"", escapes = [] }
        , { start = "U\"", end = "\"", escapes = ["\\\""] }
        , { start = "UR\"(", end = ")\"", escapes = [] }
        , { start = "'", end = "'", escapes = ["\\'"] }
        , { start = "L'", end = "'", escapes = ["\\'"] }
        , { start = "u8'", end = "'", escapes = ["\\'"] }
        , { start = "u'", end = "'", escapes = ["\\'"] }
        , { start = "U'", end = "'", escapes = ["\\'"] }
        ], color = rgb 214 156 117 }
    , number_literal_color = rgb 175 206 139
    , punctuation = ["!", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "\\", "]", "^", "`", "{", "|", "}", "~"]
    , punctuation_color = rgb 152 175 180
    , other_colors = 
        [ { syntax = [ { start = "#", end = "\n", escapes = [] } ], color = rgb 155 155 155 } -- Preprocessor directives
        ]
    }

syntax_elm : Syntax
syntax_elm =
    { name = "Elm"
    , identifier = "elm"
    , default_color = rgb 220 220 220
    , keywords = 
        [ "True", "False", "Bool", "Int", "Float", "String", "List", "type", "alias", "case", "of", "Char", "if"
        , "then", "else", "as", "exposing", "import", "let", "in", "module"
        ]
    , keyword_color = rgb 65 151 214
    , comments = { syntax = 
        [ { start = "{-", end = "-}", escapes = [] }
        , { start = "--", end = "\n", escapes = [] }
        ], color = rgb 82 165 72 }
    , string_literals = { syntax = 
        [ { start = "\"\"\"", end = "\"\"\"", escapes = [] }
        , { start = "\"", end = "\"", escapes = ["\\\""] }
        , { start = "'", end = "'", escapes = ["\\'"] }
        ], color = rgb 214 156 117 }
    , number_literal_color = rgb 175 206 139
    , punctuation = ["!", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "\\", "]", "^", "`", "{", "|", "}", "~"]
    , punctuation_color = rgb 152 175 180
    , other_colors = []
    }

syntax_sh : Syntax
syntax_sh =
    { name = "sh"
    , identifier = "sh"
    , default_color = rgb 220 220 220
    , keywords = 
        [ "echo", "ls", "cd" ]
    , keyword_color = rgb 65 151 214
    , comments = { syntax = 
        [ { start = "#", end = "\n", escapes = [] }
        ], color = rgb 82 165 72 }
    , string_literals = { syntax = 
        [ { start = "\"", end = "\"", escapes = ["\\\""] }
        , { start = "'", end = "'", escapes = ["\\'"] }
        ], color = rgb 214 156 117 }
    , number_literal_color = rgb 175 206 139
    , punctuation = ["!", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "\\", "]", "^", "`", "{", "|", "}", "~"]
    , punctuation_color = rgb 152 175 180
    , other_colors = []
    }
