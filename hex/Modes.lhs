\section{Modes}
\begin{code}
module Modes where

import qualified Environment as E
import qualified Fonts as F
import Chars
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
hMode' :: E.Environment -> [Command] -> [HElement]
hMode' e = concatenatewords . map toHElement
    where
        Just (E.HexFontInfo fnt) = E.currentFont e
        (F.SpaceInfo spS spSt spShr) = F.spaceInfo fnt
        f2d = dimenFromPoints . round . F.fixToFloat
        toHElement (CharCommand (TypedChar _ Space)) = EGlue $ Glue H (f2d spS) (f2d spSt) (f2d spShr)
        toHElement (CharCommand (TypedChar c cat)) = EBox $ Box
                                { boxType=H
                                , width=(f2d w)
                                , height=(f2d h)
                                , depth=(f2d d)
                                , boxContents=typesetChar c
                                } where (w,h,d) = F.widthHeightDepth fnt c
\end{code}

We begin by breaking a sequence of commands into paragraphs. \code{paragraph}
gets a single paragraph.

\begin{code}
paragraph :: E.Environment -> [Command] -> ([HElement],[Command])
paragraph _ [] = ([],[])
paragraph e cs = (par',rest')
    where
        (par, rest) = break isParagraphBreak cs
        par' = hMode' e par
        rest' = case rest of
                    ((PrimitiveCommand "par"):rs) -> rs
                    _ -> rest
        isParagraphBreak (PrimitiveCommand "par") = True
        isParagraphBreak (PrimitiveCommand seq) = isVCommand seq
        isParagraphBreak _ = False
\end{code}

\begin{code}
hMode :: E.Environment -> [Command] -> [VBox]
hMode env [] = []
hMode env cs = (breakintolines linewidth firstParagraph) ++ (vMode env rest)
    where
        (firstParagraph, rest) = paragraph env cs
        Just (E.HexDimen linewidth) = E.lookup "textwidth" env
\end{code}
