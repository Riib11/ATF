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
\Huge{Translator Interpreter}
\\[0.75cm]
\end{center}

%///////////////////////////////////////////////
\begin{code}

module TranslatorInterpreter
( interpret_translator
) where

import Translator

interpret_translator :: TransCode -> IO Translator
interpret_translator transcode = -- TODO: implementation
    return $ get_translator transcode

\end{code}
%\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

%-------------------------------------------------------------------------------
\end{document}
%-------------------------------------------------------------------------------
