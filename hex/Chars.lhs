Our first module defines the tagged characters used by HeX.

\begin{code}
module Chars
    ( CharCategory(..)
    , codeCategory
    , categoryCode
    , TypedChar(..)
    ) where
\end{code}

Each character, after being read by the parser acquires a character code. In
TeX, the character codes are all numeric, we will work internally with names.

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
                Invalid deriving (Eq, Enum, Bounded, Show)
\end{code}

In order to be able to go back and forth between these names and the category
codes, we define a pair of translation functions. Unit tests verify that
\code{(codeCategory $ categoryCode i) == i}, for all relevant $i$. This is
automatically derived by Haskell, but we give it a better name here:

\begin{code}
codeCategory :: CharCategory -> Integer
codeCategory = toInteger . fromEnum

categoryCode :: Integer -> CharCategory
categoryCode = toEnum . fromInteger
\end{code}

Now we define the typed char as consisting of both a character and its
category.

\begin{code}
data TypedChar = TypedChar
                 { value :: Char
                 , category :: CharCategory
                 } deriving (Eq)
\end{code}

Mostly for debugging, we should be able to visualise these. The default show
(which we'd have obtained by deriving from Show) was too verbose, so we define
our own function.

\begin{code}
instance Show TypedChar where
    show TypedChar{value=c, category=cat} = [c] ++ "(" ++ (show $ codeCategory cat) ++ ")"
\end{code}
