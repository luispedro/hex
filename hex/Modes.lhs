\section{Modes}
\begin{code}
module Modes where

import qualified Environment as E

import Tokens
import Macros
import Measures
import Boxes
import Linebreak
\end{code}

Some commands are v-mode commands, other h-mode commands. We begin with a
series of definitions to distinguish the two.

\begin{code}
isVCommand "vspace" = True
isVCommand _ = False
isHCommand = not . isVCommand
\end{code}

The two modes are intertwined. Switching to a different mode is simply a tail
call to the other mode.

\begin{code}
vMode env [] = []
vMode env cs@((PrimitiveCommand seq):_)
    | isVCommand seq = vMode1 env cs
vMode env cs = hMode env cs
\end{code}

\begin{code}
vMode1 env ((PrimitiveCommand "vspace"):cs) = vMode env cs
\end{code}

\code{hMode'} is \emph{restricted horizontal mode}: it produces a list of
HBoxes from a list of commands that are guaranteed to be h-commands.

\begin{code}
hMode' :: [Command] -> [HElement]
hMode' = concatenatewords . map toHElement
    where
        toHElement (CharCommand ' ') = EGlue spaceGlue
        toHElement (CharCommand c) = EBox $ charBox c
\end{code}

We begin by breaking a sequence of commands into paragraphs. \code{paragraph}
gets a single paragraph.

\begin{code}
paragraph :: [Command] -> ([HElement],[Command])
paragraph [] = ([],[])
paragraph cs = (par',rest')
    where
        (par, rest) = break isParagraphBreak cs
        par' = hMode' par
        rest' = case rest of
                    ((PrimitiveCommand "par"):rs) -> rs
                    _ -> rest
        isParagraphBreak (PrimitiveCommand "par") = True
        isParagraphBreak (PrimitiveCommand seq) = isVCommand seq
        isParagraphBreak _ = False
\end{code}

\begin{code}
hMode :: E.Environment -> [Command] -> [VElement]
hMode env [] = []
hMode env cs = (breakintolines linewidth firstParagraph) ++ (vMode env rest)
    where
        (firstParagraph, rest) = paragraph cs
        Just (E.HexDimen linewidth) = E.lookup "textwidth" env
\end{code}
