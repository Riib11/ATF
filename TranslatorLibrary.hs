module TranslatorLibrary
( get_translator
, trans_md_to_html
, trans_hypaper_to_html
) where

import Utilities
import Debug
import Translator

get_translator :: String -> Translator
get_translator trans_name = case trans_name of
    "md to html\n" -> trans_md_to_html
    "hypaper to html\n" -> trans_hypaper_to_html
    "hypaper to latex\n" -> trans_hypaper_to_latex
    _ -> error $ "translator not found: " ++ trans_name

-- -----------------------------------------------------------------------------

make_block :: String -> ([TargetCode] -> TargetCode) -> Bool -> Block
make_block title block_format does_nest = Block title [] block_format does_nest

-- -----------------------------------------------------------------------------
-- MD to HTML
-- -----------------------------------------------------------------------------

trans_md_to_html = Translator
    ( -- scopes
        [ make_block "--"       join_comment                         False
        , make_block "verbatim" join                                 False
        
        , make_block "head"     (join_tag "head")                    True

        , make_block "js-src-async" join_js_src_async                False
        , make_block "js-src"   join_js_src                          False
        , make_block "js"       join_js                              False
        , make_block "css-src"  join_css_src                         False
        , make_block "css"      join_css                             False
        , make_block "title"    (join_tag "title")                   False

        , make_block "body"     join_html_body                       True
        
        , make_block "#####"    (join_div  "header-5")               True
        , make_block "####"     (join_div  "header-4")               True
        , make_block "###"      (join_div  "header-3")               True
        , make_block "##"       (join_div  "header-2")               True
        , make_block "#"        (join_div  "header-1")               True
        , make_block "p"        (join_div "paragraph")               True
        , make_block "$$"       (join_block_math)                    False
        , make_block "$"        (join_inline_math)                   False
        , make_block "```"      (join_tag_class "pre" "block-code")  False
        , make_block "`"        (join_span "inline-code")            False
        , make_block "img"      join_image                           False
        , make_block ">"        (join_div "block-quote")             True
        , make_block "ul"       (join_tag_class "ul" "ul")           True
        , make_block "-"        (join_tag_class "li" "li")           True
        , make_block "bulleted" (join_bulleted_list)                 True
        , make_block "numbered" (join_numbered_list)                 True
        , make_block "*"        (join_span "inline-bold")            True
        , make_block "_"        (join_span "inline-italic")          True
        , make_block "~"        (join_span "inline-strike")          True

        , make_block "table"    (join_tag_class "table" "table")     True
        , make_block "hr"       join_hrow                            True
        , make_block "tr"       join_row                             True
        ] )
    ( -- root scope
        make_block "root" (join_tag_attr "!DOCTYPE html><html" "html") True )
    ( -- convert filepath
        \fp -> fp ++ ".html" )

-- special joins

join_comment _ = ""

join_span classname = join_container
    ("<span class=\"" ++ classname ++ "\">") ("</span>")

join_div classname = join_container
    ("<div class=\"" ++ classname ++ "\">") ("</div>\n")

join_tag_class tag classname = join_container
    ("<" ++ tag ++ " class=\"" ++ classname ++ "\">") ("</" ++ tag ++ ">")

join_tag_attr tag_begin tag_end = join_container
    ("<" ++ tag_begin ++ ">") ("</" ++ tag_end ++ ">")

join_tag tag = join_container ("<" ++ tag ++ ">") ("</" ++ tag ++ ">")

join_js [x] = "<script type=\"text/javascript\">" ++ x ++ "</script>" 

join_js_src [x] = "<script type=\"text/javascript\" src=\""
                  ++ x ++ "\"></script>"

join_js_src_async [x] = "<script type=\"text/javascript\" src=\""
                  ++ x ++ "\" async></script>"

join_css [x] = "<style type=\"text/css\">" ++ x ++ "</style>" 

join_css_src [x] = "<link rel=\"stylesheet\" type=\"text/css\" href=\""
                   ++ x ++ "\">"

join_html_body ls =
    let helper :: [TargetCode] -> TargetCode
        helper [] = ""
        helper (x:xs) = x ++ helper xs
    in "<body>\n" ++ (helper ls) ++ "\n</body>"


join_inline_math [x] = "$<<" ++ x ++ ">>$"

join_block_math [x] = join_div "block-math"
    $ ["$$<<" ++ x ++ ">>$$"]

join_row tgtcode =
    let helper :: [TargetCode] -> TargetCode
        helper ls = case ls of
            [] -> ""
            (x:xs) -> "<td>" ++ x ++ "</td>" ++ helper xs
    in "<tr class=\"row\">" ++ helper tgtcode ++ "</tr>"

join_hrow tgtcode =
    let helper :: [TargetCode] -> TargetCode
        helper ls = case ls of
            [] -> ""
            (x:xs) -> "<th>" ++ x ++ "</th>" ++ helper xs
    in "<tr class=\"hrow\">" ++ helper tgtcode ++ "</tr>"

join_image [src,alt] = "<img src=\"" ++ src ++ "\" alt=\"" ++ alt ++ "\">"
join_image [src] = "<img src=\"" ++ src ++ "\">"

join_bulleted_list ls =
    let helper :: [TargetCode] -> TargetCode
        helper [] = ""
        helper (x:xs) = "<li>" ++ x ++ "</li>" ++ helper xs
    in "<ul class=\"bulleted-list\">" ++ helper ls ++ "</ul>"

join_numbered_list ls =
    let helper :: [TargetCode] -> TargetCode
        helper [] = ""
        helper (x:xs) = "<li>" ++ x ++ "</li>" ++ helper xs
    in "<ol class=\"numbered-list\">" ++ helper ls ++ "</ol>"

-- -----------------------------------------------------------------------------
-- MD to HYPAPER
-- -----------------------------------------------------------------------------

trans_hypaper_to_html =
    let -- root
        join_root xs =
            "<!DOCTYPE html><html>"
            ++ join xs
            ++ "\n<head>"
            ++ "<link rel=\"stylesheet\" type=\"text/css\" href=\"hypaper.css\">"
            ++ "<script type=\"text/javascript\" src=\"hypaper.js\"></script>"
            ++ "<script type=\"text/javascript\" src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/latest.js\" async></script>"
            ++ "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({ extensions: [\"tex2jax.js\"], jax: [\"input/TeX\", \"output/HTML-CSS\"], tex2jax: { inlineMath: [ ['$<<','>>$'] ], displayMath: [ ['$$<<','>>$$'] ], processEscapes: false }, \"HTML-CSS\": { fonts: [\"TeX\"] } });</script>"
            ++ "\n</head>"
            ++ "\n</html>"
        -- escape
        join_verbatim [x] = x
        -- head
        join_head      = join_div "head"
        join_title [x] = "<div class=\"title\" id=\"title\">"
            ++ x ++ "</div>"
        join_subtitle  = join_div "subtitle"
        join_author    = join_div "author"
        join_date      = join_div "date"
        join_abstract  = join_div "abstract"
        -- body
        join_body = join_div "body"
        join_subsubsection = join_div "subsubsection"
        join_subsection = join_div "subsection"
        join_section = join_div "section"
        join_paragraph = join_div "paragraph"
        join_inline_math [x] = join_span "inline-math"
            $ ["$<<" ++ x ++ ">>$"]
        join_block_math  [x] = join_div "block-math"
            $ ["$$<<" ++ x ++ ">>$$"]
        join_block_code  = join_tag_class "pre" "block-code"
        join_inline_code = join_span "inline-code"
        join_image [src] = join_tag_class "center" "image"
            ["<img src=\"" ++ src ++ "\">"]
        join_image [src, alt] = join_tag_class "center" "image"
            ["<img src=\"" ++ src ++ "\" alt=\"" ++ alt ++ "\">"]
        join_figure [src, caption] = join_tag_class "center" "figure"
            ["<img src=\"" ++ src ++ "\">", "<div>" ++ caption ++ "</div>"]
        join_inline_quote = join_span "inline-quote"
        join_block_quote  = join_div "block-quote"
        join_bulleted xs  = "<ul class=\"bulleted\">"
            ++ (foldl (\x y -> x ++ "<li>" ++ y ++ "</li>") "" xs) ++ "</ul>"
        join_numbered xs  = "<ol class=\"bulleted\">"
            ++ (foldl (\x y -> x ++ "<li>" ++ y ++ "</li>") "" xs) ++ "</ol>"
        join_link [href]  = "<a class=\"link\" href=\""
            ++ href ++ "\">" ++ href ++ "</a>"
        join_link [href, text] = "<a class=\"link\" href=\""
            ++ href ++ "\">" ++ text ++ "</a>"
        join_bold      = join_span "bold"
        join_italic    = join_span "italic"
        join_smallcaps = join_span "smallcaps"
        join_strike    = join_span "strike"
        join_table     = join_tag_class "table" "table"
        join_hrow xs   = join_tag_class "tr" "table-hrow"
            $ map (\x -> join_tag "th" [x]) xs
        join_row xs   = join_tag_class "tr" "table-row"
            $ map (\x -> join_tag "tr" [x]) xs
        -- foot
        join_foot = join_div "foot"
        join_bibliography xs = join_div "bibliography"
            $ (join_div "bib-title" ["Bibliography"] ) : xs
        join_bibitem = join_div "bib-item"
    in Translator
        ( -- scopes
            --------------------
            -- escape
            [ make_block "--"       join_comment  False
            , make_block "verbatim" join_verbatim False
            --------------------
            -- head
            , make_block "head"     join_head     True
            , make_block "title"    join_title    True
            , make_block "subtitle" join_subtitle True
            , make_block "author"   join_author   True
            , make_block "date"     join_date     True
            , make_block "abstract" join_abstract True
            --------------------
            -- body
            , make_block "body" join_body True
            -- structures
            , make_block "###" join_subsubsection   True
            , make_block "##"  join_subsection      True
            , make_block "#"   join_section         True
            , make_block "p"   join_paragraph True
            -- math
            , make_block "$$" join_block_math  False
            , make_block "$"  join_inline_math False
            -- code
            , make_block "```" join_block_code  False
            , make_block "`"   join_inline_code False
            -- image
            , make_block "image"  join_image  True
            , make_block "figure" join_figure True
            -- quote
            , make_block "''" join_inline_quote True
            , make_block ">"  join_block_quote  True
            -- list
            , make_block "bulleted" join_bulleted True
            , make_block "numbered" join_numbered True
            -- link
            , make_block "@" join_link False
            -- font styles
            , make_block "*" join_bold           True
            , make_block "_" join_italic         True
            , make_block "^" join_smallcaps      True
            , make_block "~" join_strike         True
            -- table
            , make_block "table"    join_table True
            , make_block "hr"       join_hrow  True
            , make_block "tr"       join_row   True
            --------------------
            -- foot
            , make_block "foot" join_foot True
            , make_block "bibliography" join_bibliography True
            , make_block "bib-item" join_bibitem True
            ] )
        ( -- root scope
            make_block "root" (join_root) True )
        ( -- convert filepath
            \fp -> fp ++ ".html" )


-- -----------------------------------------------------------------------------


trans_hypaper_to_latex =
    let -- root
        join_env env xs = "\n\n\\begin{" ++ env ++ "}\n"
            ++ (join xs) ++ "\n\n\\end{" ++ env ++ "}"
        join_cmd cmd xs = "\\" ++ cmd ++ "{" ++ join xs ++ "}"
        join_incmd cmd xs = "{\\" ++ cmd ++ " " ++ join xs ++ "}"
        join_root xs =
            "\\documentclass{article}\n"
            ++ "\n\\usepackage{color}"
            ++ "\n\\usepackage{caption}"
            ++ "\n\\usepackage{hyperref}"
            ++ "\n\\usepackage{csquotes}"
            ++ "\n\\usepackage{amsmath}"
            ++ "\n\\usepackage{soul}"
            ++ "\n\\usepackage{graphicx}"
            ++ "\n\\graphicspath{ {./} }"
            ++ "\n\\usepackage{listings}"
            ++ "\n\\lstset{aboveskip=3mm, belowskip=3mm, showstringspaces=false, columns=flexible, basicstyle={\\small\\ttfamily}, numbers=none, breaklines=true, breakatwhitespace=true, tabsize=3}"
            ++ "\n\\begin{document}"
            ++ join xs
            ++ "\n\\end{document}"
        -- escape
        join_verbatim [x] = x
        -- head
        join_head = join_env "center"
        join_title [x] = (join_cmd "huge" [x]) ++ "\\\\[0.4cm]"
        join_subtitle [x] = (join_cmd "large" [x]) ++ "\\\\[0.75cm]"
        join_author [x] = (join_cmd "large" [x]) ++ "\\\\[0.5cm]"
        join_date  [x] = (join_cmd "large" [x]) ++ "\\\\[1.0cm]"
        join_abstract = join_env "abstract"
        -- body
        join_body = join
        join_subsubsection = join_cmd "subsubsection"
        join_subsection = join_cmd "subsection"
        join_section = join_cmd "section"
        join_paragraph = join_container "\n\n" "\n\n"
        join_inline_math = join_container "$" "$"
        join_block_math = join_container "\\[" "\\]"
        join_block_code = join_env "lstlisting"
        join_inline_code = join_cmd "texttt"
        -- \begin{figure}[t]
        -- \includegraphics[width=8cm]{Plot}
        -- \centering
        -- \end{figure}
        join_image [src] = "\\begin{figure}[h] \\includegraphics{"
            ++ src ++ "} \\centering \\end{figure}"
        join_image [src, alt] = "\\begin{figure}[h] \\includegraphics{"
            ++ src ++ "} \\centering \\end{figure}"
        join_figure [src, caption] = "\\begin{figure}[h] \\includegraphics{"
            ++ src ++ "} \n\\captionsetup{labelformat=empty} \n\\caption{" ++ caption ++ "} \\centering \\end{figure}"
        join_inline_quote = join_cmd "textit"
        join_block_quote = join_env "displayquote"
        join_bulleted xs = join_env "itemize"
            $ map (\x -> "\\item " ++ x ++ "\n") xs
        join_numbered xs = join_env "enumerate"
            $ map (\x -> "\\item " ++ x ++ "\n") xs
        join_link [href] = join_cmd "url" [href]
        join_link [href, text] = (join_cmd "href" [href])
                              ++ (join_cmd "" [text])
        join_bold = join_cmd "textbf"
        join_italic = join_cmd "textit"
        join_smallcaps = join_incmd "sc"
        join_strike = join_cmd "st"
        join_table = error "unimplemented"
        join_hrow xs = error "unimplemented"
        join_row xs = error "unimplemented"
        -- foot
        join_foot = join
        join_bibliography xs = "\n\\section*{Bibliography}" ++ join xs
        join_bibitem = join_container "\n\n\\noindent " "\n\n"
    in Translator
        ( -- scopes
            --------------------
            -- escape
            [ make_block "--"       join_comment  False
            , make_block "verbatim" join_verbatim False
            --------------------
            -- head
            , make_block "head"     join_head     True
            , make_block "title"    join_title    True
            , make_block "subtitle" join_subtitle True
            , make_block "author"   join_author   True
            , make_block "date"     join_date     True
            , make_block "abstract" join_abstract True
            --------------------
            -- body
            , make_block "body" join_body True
            -- structures
            , make_block "###" join_subsubsection   True
            , make_block "##"  join_subsection      True
            , make_block "#"   join_section         True
            , make_block "p"   join_paragraph True
            -- math
            , make_block "$$" join_block_math  False
            , make_block "$"  join_inline_math False
            -- code
            , make_block "```" join_block_code  False
            , make_block "`"   join_inline_code False
            -- image
            , make_block "image"  join_image  True
            , make_block "figure" join_figure True
            -- quote
            , make_block "''" join_inline_quote True
            , make_block ">"  join_block_quote  True
            -- list
            , make_block "bulleted" join_bulleted True
            , make_block "numbered" join_numbered True
            -- link
            , make_block "@" join_link False
            -- font styles
            , make_block "*" join_bold           True
            , make_block "_" join_italic         True
            , make_block "^" join_smallcaps      True
            , make_block "~" join_strike         True
            -- table
            , make_block "table"    join_table True
            , make_block "hr"       join_hrow  True
            , make_block "tr"       join_row   True
            --------------------
            -- foot
            , make_block "foot" join_foot True
            , make_block "bibliography" join_bibliography True
            , make_block "bib-item" join_bibitem True
            ] )
        ( -- root scope
            make_block "root" (join_root) True )
        ( -- convert filepath
            \fp -> fp ++ ".tex" )
