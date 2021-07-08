%include Formatting.fmt
%include lhs2TeX.fmt

%format getChar = "\id{getChar}"
%format putChar = "\id{putChar}"
%format twochars = "\id{twoChars}"
%format echo = "\id{echo}"
%format bad = "\id{bad}"
%format good = "\id{good}"
%format readNumbers = "\id{readNumbers}"
%format readLn = "\id{readLn}"
%format sequence = "\id{sequence}"
%format replicateM = "\id{replicateM}"


We have not yet studied how a Haskell program communicates with the outside
world (other processes, the file system, the user, |...|). This chapter shows
the exceptional way in which Haskell deals with such I/O tasks.
% Fortunately, Haskell has developed an elegant solution that prominently
% features the |Monad| concept. 

%===============================================================================
\section{Problems with I/O in Haskell}

At first blush it seems easy to perform I/O. We simply adopt the approach of 
other programming languages. This means that we build a number of primitive
functions into the language that perform their I/O task through system
calls to the operating system.
For instance, to read a character from the standard input stream we provide a
function |getChar :: Char| that performs its I/O through system calls and that
returns a character which we can use in the program.

While there seems to be no cloud in the sky there are nevertheless two
serious issues with this naive approach to I/O. The first---and foremost---issue is a matter
of principles and the second of a more pragmatic nature.

%-------------------------------------------------------------------------------
\subsection{Reasoning about I/O}

One of the important principles that Haskell has adopted from the $\lambda$ calculus
is that it should be easy to reason about programs. Two properties of the language
make this possible. 
\begin{itemize}
\item \textbf{Evaluation preserves equality}
With the help of a number of calculation rules, in the form of equalities, we can
easily derive equivalent representations of the same program. If we systematically
apply this approach we can establish that a function call |f x| is equivalent
to its result |v|. After all, we obtain the result by rewriting the call in a number
of equivalence-preserving steps.
\item \textbf{The Leibniz Principle}
The Leibniz Principle states that expressions |e_1| and |e_2| are equivalent
if they cannot be distinguished in any context |g|. We can capture this in the following
formula:
\[ \forall |e_1|, |e_2| . |e_1 == e_2| \rightarrow \forall |g|. |g e_1 == g e_2| \]
\end{itemize}
The combination of these two properties is known as \emph{referential transparency}:
the result of a function |f| only depends on its parameter |x|, and not on any
other factors like its context or how often |f| has already been called.

An important application of referential transparency is \emph{common subexpression elimination} (CSE):
if the same subexpression occurs more than once inside a larger expression, then we can
eliminate the duplicates. The following code illustrates this situation:
\sethscode{otherhscode}

< (f x, f x)

where the subexpression |f x| occurs twice. With CSE we can rewrite this expression into the
equivalent form
\sethscode{otherhscode}

< let y = f x
< in (y, y)

You can check that CSE indeed preserves equivalence for all expressions we have covered so
far. There is only one exception: the naive |getChar| function that we have just introduced.
Suppose that the standard input consists of the sequence of two characters |"ab"|. Then
we likely expect the following result:
\sethscode{interactivehscode}

< ?> (getChar, getChar)
< ('a','b')

However, if we apply CSE, then we can rewrite the expression to:
\sethscode{interactivehscode}

< ?> let y = getChar in (y, y)
< ('a', 'a')

Obviously we do not expect that this expression reads more than one character!
We clearly have a problem: the two expressions only make sense if they are not
equivalent, but if we hold on to our principles they have to be equivalent.
Obviously, we are not willing to give in on either account.

%-------------------------------------------------------------------------------
\subsection{Predictability of I/O}

Many functional programming languages give up on their principles when it comes
to I/O and opt for the pragmatic |getChar :: Char| approach. Yet, even if we
would also prefer a pragmatic solution to a principled one for Haskell, we
would nevertheless not be satisfied with the |getChar :: Char| approach. The
reason is Haskell's unusual \emph{lazy evaluation} mechanism. Lazy evaluation is discussed 
extensively in Chapter~\ref{ch:lazy_evaluation}. Here we will just say that it evaluates just enough
of a computation to produce its result, ignoring any unnecessary subexpressions that do not contribute towards
the result.

Assume again that the input consists of the sequence |"ab"|, and that we define
the following function.
\sethscode{customhscode}

< twochars :: (Char, Char)
< twochars = (getChar, getChar)

If we evaluate this function, we get the result:
\sethscode{interactivehscode}

< ?> twochars
< ('a','b')

However, because of lazy evaluation, it may be the case that the tuple is not 
always fully evaluated. For instance, in this case
\sethscode{interactivehscode}

< ?> fst twochars
< 'a'

the second component is not needed and hence not evaluated. Similarly, in this
case the first component is not evaluated:
\sethscode{interactivehscode}

< ?> snd twochars
< 'a'

But now the result is |'a'| instead of the expected |'b'|. How can this be?
Well, the first component is not evaluated and hence does not consume the
first character on the standard input. So, when we read a character during the
evaluation of the second component, we obviously get the first character in line: |'a'|.

A similarly unexpected situation arises when we flip the components of the tuple.
\sethscode{interactivehscode}

< ?> (snd twochars, fst twochars)
< ('a', 'b')

We do not get |('b','a')| because the components of the result are evaluated in the opposite
order.

The observations we have made on these small hypothetical examples can be
generalized. The native approach to I/O in Haskell is not practical in
combination with lazy evaluation.  In general it is impossible for the
programmer to predict when an I/O action is performed. Moreover, the order does
not depend on the I/O code itself, but on its context. This is of course
unmanageable.

%===============================================================================
\section{The |IO| Type}

As we have just seen, the execution of I/O actions in Haskell is problematic, both
on a principled and on a pragmatic level. Haskell's ingenious solution to the problem
is to simply ban the problematic execution of I/O actions from the language.

%-------------------------------------------------------------------------------
\subsection{Plans of I/O Actions}

Instead of executing I/O actions itself, Haskell leaves the dirty work to 
a ``third party''. A Haskell program does not execute any I/O actions, but
computes a \emph{plan of I/O actions} to be executed. It is up to a third party
to execute this plan. This third party is typically the Haskell runtime
environment implemented in C or assembler.

The important conceptual change of perspective is that within a Haskell program
we build plans of I/O actions instead of executing them. In light of
this new perspective the signature |getChar :: Char| is unsuitable because
it does not designate |getChar| as the plan for an I/O action that yields
a character. Indeed, the type |Char| is the type of a character and not the
type of a plan to obtain a character.

Instead, Haskell uses the following actual signature:
\sethscode{customhscode}

< getChar :: IO Char

where the type constructor |IO| indicates a plan for an I/O action,
and not the actual execution of said action.

For the sake of brevity we omit ``plan for'' from now on and simply say IO action when we are
talking about a value of type |IO a|.

%-------------------------------------------------------------------------------
\subsection{Building I/O Actions}

The Haskell language specification stipulates that the |IO| type is \emph{abstract}.
This means that the programmer has no access to its definition. Hence, it is for instance
not possible to pattern match on values of the |IO| type.

\paragraph{Primitive Functions}
The only way to create values of type |IO a| is through primitive functions
of this type.
The function |getChar :: IO Char| is an example of such a function.
Another primitive function is |putChar :: Char -> IO ()|,
that sends a character to the standard output (e.g., the terminal).
Note that the type |()| is used here. It is used to indicate that the I/O action
does not yield a noteworthy result; the only reason to execute it is for its side effect.

Another trivial I/O action is created with |return :: a -> IO a|. Given a value
of type |a|, it builds a plan to produce that value. The execution of that plan
is meant to simply return the given value without any actual I/O.

\paragraph{Composing I/O Actions}
The programmer can obtain more complex I/O actions by composing the primitive functions.
% Composition is possible because the Haskell language specification states that |IO| is
% a monad. Hence, we can represent pure values as I/O actions with |return :: a -> IO a|. 
For this, there is the primitive combinator |(>>=) :: IO a -> (a -> IO b) -> IO
b|. For example,
\sethscode{customhscode}

< echo :: IO ()
< echo = getChar >>= (\c -> putChar c)

builds an I/O action that fetches a character and then outputs it. The |(>>=)| 
operator covers the ``and then`` part of the previous sentence.

The |return| primitive is the neutral element of composition. For example,
|return 'a' >>= (\c -> putChar c)| is the same as |putChar 'a'|. Also,
|getChar >>= (\c -> return c)| is the same as |getChar|.

Having such a neutral element is often convenient as a base case. For instance, 
|putStr| outputs a |String|, which is just a list of characters.
\sethscode{customhscode}

< putStr :: String -> IO ()
< putStr []      = return ()
< putStr (c:cs)  = putChar c >>= (\ _ -> putStr cs)

In the case of the empty string, there is nothing to be output. This ``do nothing`` is captured by
|return ()|. When composed with outputting the other characters, what happens is just outputting
the other characters.

%-------------------------------------------------------------------------------
\subsection{The |do| Notation}

Using the |>>=| operator gets unwieldy when chaining more than two actions.
Because it is a common pattern, Haskell provides special syntactic sugar,
called the |do| notation, to facilitate it.

Here is an example using this |do| notation.
\sethscode{customhscode}

> echo2 :: IO ()
> echo2 = do  c1 <- getChar
>             c2 <- getChar
>             putChar c1
>             putChar c2

The |do| keyword signals the start of a |do|-block where every
line contains an I/O action that follows the previous one. 
The result of an I/O action can be given a name with the |<-| arrow.
Subsequent lines can refer to names bound in previous lines.

More useful examples are:
\sethscode{customhscode}

< echo :: IO ()
< echo = do  c <- getChar
<            putChar c
<
< putStr :: String -> IO ()
< putStr []      = return ()
< putStr (c:cs)  = do  putChar c
<                      putStr cs
<
< putStrLn :: String -> IO ()
< putStrLn str  = do  putStr str
<                     putChar '\n' 

%-------------------------------------------------------------------------------
\subsection{Reasoning about Plans of I/O Actions}

Simply talking about plans of I/O actions does not result in a conceptual
problem.

Two plans are equivalent if they denote the same action. When we have two
equivalent plans, we can share the plan (CSE):
\begin{eqnarray*}
|(getChar,getChar)| & |==| & |let y = getChar in (y, y)|
\end{eqnarray*}
Both expressions have the type |(IO Char, IO Char)|,
a tuple of plans.

%-------------------------------------------------------------------------------
\subsection{Executing I/O Actions}

Conceptually the execution of I/O actions happens outside the Haskell program.
The program itself is only responsible for delivering the plan. Hence, a Haskell
program is expected to have a function with the following signature:
\sethscode{customhscode}

< main :: IO ()

The runtime system invokes this function to obtain the plan and then executes it.

An elementary Haskell program looks like:
\sethscode{customhscode}

> main :: IO ()
> main = putStrLn "Hello, World!"

You can compile this program with the GHC compiler:
\begin{Verbatim}
    $ ghc --make Hello.hs
    [1 of 1] Compiling Main		(Hello.hs, Hello.o)
    Linking Hello ...
\end{Verbatim}
and then execute it:
\begin{Verbatim}
    $ ./Hello
    Hello, World!
\end{Verbatim}

You can also try out I/O actions in the GHCi interpreter. In contrast
to other types of values, GHCi does not print values of type |IO a|.
Instead it executes them.
\sethscode{interactivehscode}

< ?> putStrLn "Hello, World!"
< Hello, World!

%-------------------------------------------------------------------------------
\subsection{The Shell/Core Metaphor}

As we have seen, I/O actions are only executed once they have been computed
by the program. Conceptually we say that a Haskell program consists of
an outer |IO| shell, the |IO| plan that is returned, and a core that consists
of pure computations used by the |IO| plan.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Thin Shell, Fat Core}

To stick as closely as possible to Haskell's pure principles, it is good style to
keep the |IO| shell of your program as thin as possible, and to allocate as much
of the functionality as possible in its pure core. Moreover, tight coupling between both
parts of the program should be avoided.

Here is an example of bad style:
\sethscode{customhscode}

> bad :: Int -> IO ()
> bad n = go n 0
>   where  go :: Int -> Int -> IO ()
>          go 0 acc  = print acc
>          go n acc  = do  x <- readLn
>                          go (n-1) (acc+x)

This is better style:
\sethscode{customhscode}

> good :: Int -> IO ()
> good n = do  xs <- readNumbers n
>              print (sum xs)

> readNumbers :: Int -> IO [Int]
> readNumbers 0  = return []
> readNumbers n  = do  x   <- readLn
>                      xs  <- readNumbers (n-1)
>                      return (x:xs) 

As you can see, this program cleanly separates the program logic, i.e., summing
a sequence of numbers, from reading and writing numbers. This approach is not
only easier to read, but also promotes reuse. We can now use the pure library
function |sum| to add up the numbers and we can later reuse |readNumbers| in
other applications that do not necessarily add up the numbers they read.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Playing with |IO| Values}

As long as |IO| values have not been embedded in the program shell, they are 
not executed by the runtime system. This means we can freely manipulate them, for
example, in the process of building complex |IO| values.

For instance, we can split |readNumbers| into a number of more primitive functions,
that are more widely reusable:
\begin{enumerate}
\item The central action is |readLn :: IO Int| which reads a number.
\item We need this action $n$ times:
\sethscode{customhscode}

< replicate :: Int -> a -> [a]
< replicate 0 _  = []
< replicate n x  = x : replicate (n-1) x

Note that |replicate| has a polymorphic type that is not |IO|-specific. Hence it is highly 
reusable.

\item Finally, we need to sequence a list of |IO| actions and collect their results in a list.
\sethscode{customhscode}

< sequence :: [IO a] -> IO [a]
< sequence []      = return []
< sequence (m:ms)  = do  x   <- m
<                        xs  <- sequence ms
<                        return (x:xs)

% Note that |sequence| works for all monads, and not just the |IO| monad. Hence, it is
% also highly reusable.

\end{enumerate}

The above three functions are all Haskell library functions, just like the common composition of |replicate| and |sequence|.
\sethscode{customhscode}

< replicateM :: Int -> IO a -> IO [a]
< replicateM n m  = sequence (replicate n m)

By using these library functions, we can write |readNumbers| much more compactly:
\sethscode{customhscode}

< readNumbers :: Int -> IO [Int]
< readNumbers n  = replicateM n readLn

In the Haskell library, |sequence| and |replicateM| have more general types. The library 
uses with the |Monad| type class to support all types, not just |IO|, that have
|return| and |>>=| methods. This abstraction is discussed more extensively in Chapter~\ref{ch:monads}.
