\section{Modes}
\begin{code}
module Modes
    ( hMode
    , vMode
    , paragraph
    ) where

import qualified Environment as E
import qualified Fonts as F
import Chars
import Macros
import Measures
import Boxes
import Linebreak
\end{code}

Some commands are v-mode commands, other h-mode commands. We begin with a
series of definitions to distinguish the two.

\begin{code}
isVCommand :: String -> Bool
isVCommand "\\vspace" = True
isVCommand _ = False
\end{code}

The two modes are intertwined. Switching to a different mode is simply a tail
call to the other mode.

\begin{code}
vMode :: E.Environment String E.HexType -> [Command] -> [VBox]
vMode _ [] = []
vMode env cs@((PrimitiveCommand csname):_)
    | isVCommand csname = vMode1 env cs
vMode env cs = hMode env cs
\end{code}

\code{vMode1} handles one vertical mode command.

\begin{code}
vMode1 :: E.Environment String E.HexType -> [Command] -> [VBox]
vMode1 env ((PrimitiveCommand "\\vspace"):cs) = vMode env cs
vMode1 _ _ = error "hex.Modes.vMode1: Can only handle PrimitiveCommand"
\end{code}

\code{hMode'} is \emph{restricted horizontal mode}: it produces a list of
HBoxes from a list of commands that are guaranteed to be h-commands.

\begin{code}
hMode' :: E.Environment String E.HexType -> [Command] -> [HElement]
hMode' e = concatenatewords . map toHElement
    where
        Just (E.HexFontInfo fnt) = E.currentFont e
        (F.SpaceInfo spS spSt spShr) = F.spaceInfo fnt
        f2d = dimenFromFloatingPoints . F.fixToFloat
        toHElement (CharCommand (TypedChar _ Space)) = EGlue $ Glue H (f2d spS) (f2d spSt) (f2d spShr) 0
        toHElement (CharCommand (TypedChar c _)) = EBox $ Box
                                { boxType=H
                                , width=(f2d w)
                                , height=(f2d h)
                                , depth=(f2d d)
                                , boxContents=typesetChar c
                                } where (w,h,d) = F.widthHeightDepth fnt c
        toHElement _ = error "hex.Modes.hmode'.toHElement: can only handle CharCommand"
\end{code}

We begin by breaking a sequence of commands into paragraphs. \code{paragraph}
gets a single paragraph.

\begin{code}
paragraph :: E.Environment String E.HexType -> [Command] -> ([HElement],[Command])
paragraph _ [] = ([],[])
paragraph e cs = (par',rest')
    where
        (par, rest) = break isParagraphBreak cs
        par' = hMode' e par
        rest' = case rest of
                    ((PrimitiveCommand "\\par"):rs) -> rs
                    _ -> rest
        isParagraphBreak (PrimitiveCommand "\\par") = True
        isParagraphBreak (PrimitiveCommand csname) = isVCommand csname
        isParagraphBreak _ = False
\end{code}

\begin{code}
spreadlines :: Scaled -> [VBox] -> [VBox]
spreadlines _ [] = []
spreadlines baselineskip (v:vs) = (v:k:spreadlines baselineskip vs)
    where
        k = Box { boxType=V, height=ht, depth=zeroDimen, width=zeroDimen, boxContents=(Kern ht) }
        ht = (height v) `dmul` (scaledToRational baselineskip)
\end{code}

\code{hMode} implements horizontal mode, whose output is a series of vertical boxes.

\begin{code}
hMode :: E.Environment String E.HexType -> [Command] -> [VBox]
hMode _ [] = []
hMode env cs = (spreadlines baselineskip $ breakintolines linewidth firstParagraph) ++ (vMode env rest)
    where
        (firstParagraph, rest) = paragraph env cs
        Just (E.HexDimen linewidth) = E.lookup "textwidth" env
        Just (E.HexScaledNumber baselineskip) = E.lookup "baselineskip" env
\end{code}
