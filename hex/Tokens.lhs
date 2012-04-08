\section{Tokens}
\begin{code}
module Tokens
    ( Token(..)
    , tokenCategory
    , TokenStream(..)
    , newTokenStream
    , gettoken
    , tokenliststream
    , emptyTokenStream
    , updateCharStream
    , toksToStr
    , TkS
    , emptyTkS
    , gettokenM
    , maybetokenM
    , skiptokenM
    , peektokenM
    , maybepeektokenM
    , gettokentilM
    , streampushM
    , streamenqueueM
    , updateCharStreamM
    , maybespaceM
    , maybeeqM
    , readNumberM
    , readCharM
    , readStrM
    ) where

import Chars
import CharStream

import Control.Monad
import Control.Monad.State (State,get,put,modify)
\end{code}

Tokens are the next level after annotated characters. A Token is either a
control sequence or an annotated character.

\begin{code}
data Token =
    ControlSequence String
    | CharToken TypedChar
    deriving (Eq)

instance Show Token where
    show (ControlSequence name) = "[" ++ name ++ "]"
    show (CharToken tc) = show tc
\end{code}

Sometimes, just the category of a character is important.
\begin{code}
tokenCategory (CharToken tc) = category tc
tokenCategory _ = Invalid -- It doesn't matter what
\end{code}

We implement the state system described in pp. 46--47 of the Texbook. The 3
states, N, S, and M, are mapped to 3 functions sN, sM, and sS. The
implementation is a bit simpler that what is described in the book. In
particular, there is no concept of ``lines'' separate from the separation by
newline characters. Whether this will matter, only time will tell.

We start with \code{skiptoeol} which is used for comments. It skips everything,
up to, and including, a newline.

\begin{code}
skiptoeol :: TypedCharStream -> TypedCharStream
skiptoeol st
    | emptyStream st = st
    | (category c) == EOL = st'
    | otherwise = skiptoeol st'
    where (c,st') = getchar st
\end{code}

In order to have the state functions return themselves, we need to define them
as a datatype. Unfortunately, we also need to define the ugly
\code{applyStateFunction} helper.

\begin{code}
newtype StateFunction = StateFunction ( TypedCharStream -> ([Token], TypedCharStream, StateFunction) )
applyStateFunction (StateFunction s) = s
\end{code}

Now, the implementation of the two easy states: S \& N.

\begin{code}
sN = StateFunction sN' where
    sN' st
        | emptyStream st = ([],st,sN)
        | (category c) == EOL = ([ControlSequence "\\par"], rest, sN)
        | (category c) == Space = sN' rest
        | otherwise = applyStateFunction sM st
        where (c,rest) = getchar st
sS = StateFunction sS' where
    sS' st
        | emptyStream st = ([], st, sS)
        | (category c) == EOL = applyStateFunction sN rest
        | (category c) == Space = applyStateFunction sS rest
        | otherwise = applyStateFunction sM st
        where (c,rest) = getchar st
\end{code}

\code{sM} does most of the real work of transforming \code{TypedChar}s into
\code{Token}s.

Almost everything is a \code{CharToken}, except single non-letters or a word
following an \code{Escape}. A word is a sequence of \code{Letter}s followed by
non-\code{Letter}. Therefore, getting a csname is done in two steps: if it is
single character, just stop there; otherwise, invoke \code{breakup'}.

The rule of what the next state is are a bit convoluted, but they're taken from
the Texbook.

\begin{code}

sM = StateFunction sM' where
    sM' st
        | emptyStream st = ([], st, sM)
                                -- Maybe below should be (TypedChar '\n' Space) ?
        | (category c) == EOL = ([CharToken (TypedChar ' ' Space)], rest, sN)
        | (category c) == Space = ([CharToken c], rest, sS)
        | (category c) == Comment = applyStateFunction sN $ skiptoeol rest
        | (category c) == Active = ([ControlSequence [value c]], rest, sM)
        | (category c) /= Escape = ([CharToken c], rest, sM)
        where (c,rest) = getchar st
    sM' st = ([ControlSequence ('\\':name)], rest', nextstate)
        where
            (_,rest) = getchar st
            (name, rest', nextstate) = breakup rest
            breakup st'
                | emptyStream st' = ([], st', sN)
                | (category c) == Space = ([value c], brest, sS)
                | (category c) /= Letter = ([value c], brest, sM)
                | otherwise = breakup' [] st'
                where (c,brest) = getchar st'
            breakup' acc st'
                | emptyStream st' = (acc, st', sS)
                | (category c) == Space = (acc, brest, sS)
                | (category c) /= Letter = (acc, st', sM)
                | otherwise = breakup' (acc ++ [value c]) brest
                where
                    (c,brest) = getchar st'
\end{code}

Similar to \code{TypedCharStream}, we define a \code{TokenStream} which
transforms the typed chars into \code{Token}s. The biggest difference is the
\code{queue} functionality, which allows one to push back tokens into the
queue.

\begin{code}
data TokenStream = TokenStream
                { charsource :: TypedCharStream
                , state :: StateFunction
                , queue :: [Token]
                }

instance Eq TokenStream where
    _ == _ = False

newTokenStream :: TypedCharStream -> TokenStream
newTokenStream cs = TokenStream cs sN []
\end{code}

The main function of this module is \code{gettoken}, which returns a pair
\code{(Token, TokenStream)}. This could easily be modified into a Monadic
interface.

\begin{code}
gettoken st | emptyTokenStream st = error "hex.Tokens.gettoken: empty stream"
gettoken tSt@TokenStream{queue=(t:ts)} = (t,tSt{queue=ts})
gettoken tSt@TokenStream{charsource=st, state=s, queue=[]} =
    gettoken tSt{charsource=st',state=s',queue=q}
        where (q,st',s') = applyStateFunction s st

\end{code}


Sometimes we want to have a simple stream that only spits out the tokens we
initialise it with. This is achieved by \code{tokenliststream}:

\begin{code}
tokenliststream toks = TokenStream (TypedCharStream [] []) sN toks
\end{code}

\code{emptyTokenStream} is surprisingly tricky. In fact, we need to look ahead
to see if there is anything left to read. For example if the input consists of
a few lines of comments, there are still many chars left, but the token stream
will be empty. If the state functions were defined slightly differently (to
always read as much as they could), this function could be simpler, but the
state functions would be potentially more complex.

\begin{code}
emptyTokenStream TokenStream{queue=(_:_)} = False
emptyTokenStream TokenStream{charsource=st, state=s, queue=[]}
    | emptyStream st = True
    | otherwise = (null q && emptyStream st')
        where (q,st',_) = applyStateFunction s st
\end{code}

\begin{code}
toksToStr toks = map charof toks
    where
        charof (CharToken (TypedChar c _)) = c
        charof _ = error "hex.Tokens.toksToStr.charof: Unexpected token"
\end{code}


We also add a function to manipulate the underlying character stream.

\begin{code}
updateCharStream t@TokenStream{charsource=s} f = t{charsource=(f s)}
\end{code}

To make code simpler, we define a \code{TokenStream} monad, abbreviated TkS:

\begin{code}
type TkS e a = State (e,TokenStream) a
bTkS :: (TokenStream -> (a,TokenStream)) -> TkS e a
bTkS f = do
    (e,st) <- get
    let (r,st') = f st
    put (e,st')
    return r
\end{code}

We add several helper function to check the status of the stream and get tokens
(while watching out for eof conditions)

\begin{code}
emptyTkS :: TkS e Bool
emptyTkS = do
    tks <- snd `liftM` get
    return (emptyTokenStream tks) 

gettokenM :: TkS e Token
gettokenM = bTkS gettoken
maybetokenM = do
    t <- maybepeektokenM
    case t of
        Nothing -> return Nothing
        Just _ -> gettokenM >> return t

skiptokenM = void gettokenM
peektokenM = bTkS (\tks -> let (t,_) = gettoken tks in (t,tks))
maybepeektokenM = do
    e <- emptyTkS
    if e then return Nothing
    else Just `liftM` peektokenM
\end{code}

To get a few tokens in a row, we can call \code{gettokentilM}, which returns
all token until a certain condition is fulfilled. The matching token is left in
the queue.
\begin{code}
gettokentilM cond = do
    mc <- maybepeektokenM
    case mc of
        Just tk | not (cond tk) -> do
            void gettokenM
            rest <- gettokentilM cond
            return (tk:rest)
        _ -> return []
\end{code}

Updating the char stream can also be done in the monad:
\begin{code}
updateCharStreamM f = modify (\(e,st) -> (e,updateCharStream st f))
\end{code}

We can add tokens to the start of the queue, either one (\code{streampushM}) or
several (\code{streamenqueueM}).
\begin{code}
streampushM t = streamenqueueM [t]
streamenqueueM nts = modify (\(e,st@TokenStream{queue=ts}) -> (e,st{queue=(nts ++ ts)}))
\end{code}

An often needed operation is to skip an optional space or additional equal
signs. We first implement a generic \code{maybetokM} function, which takes a
condition and skips a token if it matches that condition.
\begin{code}
maybetokM :: (Token -> Bool) -> TkS e ()
maybetokM cond = do
    mt <- maybepeektokenM
    case mt of
        Just t | cond t -> void gettokenM
        _ -> return ()
\end{code}

Now, \code{maybespaceM} and \code{maybeeqM} are simply defined as calls to
\code{maybetokM}:
\begin{code}
maybespaceM = maybetokM ((== Space) . tokenCategory)
maybeeqM = maybetokM (== (CharToken (TypedChar '=' Other)))
\end{code}

Finally, a simple function to read an integer from tokens:
\begin{code}
readNumberM :: TkS e Integer
readNumberM = do
        tk <- peektokenM
        case tk of
            CharToken (TypedChar '"' _) -> gettokenM >> readNumber ("0x"++) hexdigits
            CharToken (TypedChar '\'' _) -> gettokenM >> readNumber ("0o"++) octdigits
            CharToken (TypedChar c _) | c `elem` decdigits -> readNumber id decdigits
            t -> error $ concat ["hex.Tokens.readNumberM: expected number (got ", show t, ")"]
    where
        readNumber prefix cond = (read . prefix) `liftM` (digits cond)
        digits :: [Char] -> TkS e [Char]
        digits accepted = do
            tok <- maybepeektokenM
            case tok of
                Just (CharToken tc) | ((`elem` accepted) . value) tc -> do
                    skiptokenM
                    rest <- digits accepted
                    return (value tc:rest)
                _ -> return []
        octdigits = "01234567"
        decdigits = octdigits ++ "89"
        hexdigits = decdigits ++ "ABCDEF"
\end{code}

A few primitives (e.g., \tex{\\catcode}) need to read characters:
\begin{code}
readCharM :: TkS e Char
readCharM = do
    skiptokenM
    tk <- gettokenM
    return (case tk of
        CharToken (TypedChar c _) -> c
        ControlSequence ['\\',c] -> c
        _ -> '\0')
\end{code}

Others need to read a string:
\begin{code}
readStrM = do
    t <- maybepeektokenM
    case t of
        Just (CharToken (TypedChar c cat))
            | cat == Space -> return []
            | otherwise -> do
                skiptokenM
                cs <- readStrM
                return (c:cs)
        _ -> return []
\end{code}
