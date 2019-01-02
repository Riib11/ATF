\documentclass{article}
%-------------------------------------------------------------------------------
% SETUP
%-------------------------------------------------------------------------------

\usepackage{verbatim}
\newenvironment{code}{\verbatim}{\endverbatim\normalsize}
\usepackage[margin=1in]{geometry}

\newcommand{\tc}[1]{\texttt{#1}}

%-------------------------------------------------------------------------------
\begin{document}
%-------------------------------------------------------------------------------

\begin{center}
\Huge{ATL Translator}
\\[0.75cm]
\end{center}

%///////////////////////////////////////////////
\begin{code}

module Translator
( SourceCode, TransCode, TargetCode
, Translator(Translator)
    , trans_blocks
    , trans_root_block
    , trans_convert_filepath
, Block(Block)
    , block_title
    , block_children
    , block_format
    , block_does_nest
, BlockChild(ChildCode, ChildBlock)
) where

type SourceCode = String
type TransCode  = String
type TargetCode = String

data Translator = Translator
    { trans_blocks           :: [Block]
    , trans_root_block       :: Block
    , trans_convert_filepath :: FilePath -> FilePath }

data Block = Block
    { block_title     :: String
    , block_children  :: [BlockChild]
    , block_format    :: [TargetCode] -> TargetCode
    , block_does_nest :: Bool }

data BlockChild
    = ChildCode  SourceCode
    | ChildBlock Block

instance Show Block where
    show block
        = "{ " ++ (block_title block) ++ " : "
        ++ (show $ reverse $ block_children block) ++ " }"

instance Show BlockChild where
    show child = case child of
        ChildCode x -> "\"" ++ x ++ "\""
        ChildBlock x -> show x

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

%-------------------------------------------------------------------------------
\end{document}
%-------------------------------------------------------------------------------
