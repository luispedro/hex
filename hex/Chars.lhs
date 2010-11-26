Our first module defines the tagged characters used by HeX.

\begin{code}
module Chars where
\end{code}

Each character, after being read by the parser acquires a character code. In
TeX, the character codes are all numeric, we will work internally with names.

Character codes are defined by a table that the user can manipulate. This table
obeys namespace rules. For now, we will just represent the table as a Data.Map:

\begin{code}
import qualified Data.Map as Map
\end{code}

We now define the categories.

\begin{code}
data CharCategory = Escape |
                BeginGroup |
                EndGroup |
                MathShift |
                AlignmentTab |
                EOL |
                Parameter |
                Superscript |
                SubScript |
                Ignored |
                Space |
                Letter |
                Other |
                Active |
                Comment |
                Invalid deriving (Eq, Enum, Bounded)

instance Show CharCategory where
    show Escape = "Escape"
    show BeginGroup = "BeginGroup"
    show EndGroup = "EndGroup"
    show MathShift = "MathShift"
    show AlignmentTab = "AlignmentTan"
    show EOL = "EOL"
    show Parameter = "Parameter"
    show Superscript = "Superscript"
    show SubScript = "SubScript"
    show Ignored= "Ignored"
    show Space = "Space"
    show Letter = "Letter"
    show Other = "Other"
    show Active = "Active"
    show Comment = "Comment"
    show Invalid = "Invalid"
\end{code}

In order to be able to go back and forth between these names and the category
codes, we define a pair of translation functions. Unit tests verify that
\texttt{(codeCategory $ categoryCode i) == i}, for all relevant $i$.

\begin{code}
codeCategory :: CharCategory -> Integer
codeCategory Escape = 0
codeCategory BeginGroup = 1
codeCategory EndGroup = 2
codeCategory MathShift = 3
codeCategory AlignmentTab = 4
codeCategory EOL = 5
codeCategory Parameter = 6
codeCategory Superscript = 7
codeCategory SubScript = 8
codeCategory Ignored= 9
codeCategory Space = 10
codeCategory Letter = 11
codeCategory Other = 12
codeCategory Active = 13
codeCategory Comment = 14
codeCategory Invalid = 15

categoryCode :: Integer -> CharCategory
categoryCode 0 = Escape
categoryCode 1 = BeginGroup
categoryCode 2 = EndGroup
categoryCode 3 = MathShift
categoryCode 4 = AlignmentTab
categoryCode 5 = EOL
categoryCode 6 = Parameter
categoryCode 7 = Superscript
categoryCode 8 = SubScript
categoryCode 9 = Ignored
categoryCode 10 = Space
categoryCode 11 = Letter
categoryCode 12 = Other
categoryCode 13 = Active
categoryCode 14 = Comment
categoryCode 15 = Invalid
\end{code}

Now we define the typed char as consisting of both a character and its
category.

\begin{code}
data TypedChar = TypedChar { value :: Char
                 , category :: CharCategory
                 }
\end{code}

Mostly for debugging, we should be able to visualise these. The default show
(which we'd have obtained by deriving from Show) was too verbose, so we define
our own function.

\begin{code}
instance Show TypedChar where
    show TypedChar{value=c, category=cat} = [c] ++ "(" ++ (show $ codeCategory cat) ++ ")"
\end{code}

We now define the central function of this module, which is a trivial function.

\begin{code}
annotate :: Map.Map Char CharCategory -> Char -> TypedChar
\end{code}

The reason it takes the table as its \emph{first} argument is that it makes it
easier to curry.

\begin{code}
annotate table c = TypedChar{value=c, category=cat}
    where cat = Map.findWithDefault Other c table
\end{code}

The plaintex table needs to be defined in code, at least in part. Some of it
could be defined in HeX code if we have enough to correctly parse \\chardef.

This is a ASCII only implementation (at least in the sense that only the
non-accented 26 English letters are tagged as letters). For fuller unicode
support, this might need to be extended in the future.

\begin{code}
plaintextable = Map.fromList $ [('\\', Escape)
                         ,('{', BeginGroup)
                         ,('}', EndGroup)
                         ,('$', MathShift)
                         ,('&', AlignmentTab)
                         ,('\n', EOL)
                         ,('#', Parameter)
                         ,('^', Superscript)
                         ,('_', SubScript)
                         ,('\0', Ignored)
                         ,(' ', Space)
                         ,('~', Active)
                         ,('%', Comment)
                         ,('\8', Invalid)
                         ] ++ map (\c -> (c, Letter)) "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
\end{code}

