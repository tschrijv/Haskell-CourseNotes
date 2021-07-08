%include Formatting.fmt
%include lhs2TeX.fmt

%if False

> import Data.Char
> import System.Random

%endif

This chapter introduces a powerful functional programming abstraction for
dealing with side effects.

%===============================================================================
\section{Kinds and Type Constructors}

We use types to classify values. The values |True| and |False| are of type |Bool|,
the value |()| is of type |()|, |'a'| and |'b'| are of type |Char|, and so on.

Yet some types, like |Maybe|, are not inhabited by values. To be clear,
there are of course values of type |Maybe ()|, namely
\begin{equation*}
\{ |Nothing|, |Just ()| \}
\end{equation*} 
or of type |Maybe Bool|, namely
\begin{equation*} 
\{ |Nothing|, |Just True|, |Just False| \}
\end{equation*} 
but there are not values of type |Maybe|. The type has to be completed with 
another type in order to be inhabited by some values.

%format k = "\kappa"
%format k_1
%format k_2

With \emph{kinds} we can classify ``proper'' types, who denote a set of values,
and improper or incomplete types who do not represent any values.
Kinds |k| have an inductive structure that is much simpler than that of types. There
are only two syntactic forms:
\begin{itemize}
\item
Kind |*| is the base case. It is the kind of all proper types. We say for instance that
|Bool| has kind |*| and write this as |Bool :: *|.
\item 
The inductive case is kind |k_1 -> k_2|, where |k_1| are |k_2| themselves kinds.
This is the kind of improper types. These types are also called \emph{type constructors},
in analogy with data constructors, because they need to be applied to other types
to obtain a proper type.
A type constructor of kind |k_1 -> k_2| takes a parameter of kind |k_1| and yields
a type of kind |k_2|.  

In the simplest case |k_1| and |k_2| are just the kind |*|. An example of this situation
is |Maybe :: * -> *|. When we apply |Maybe| to |Bool| of kind |*| we obtain |Maybe Bool|
of kind |*|. Another example of a type constructor is that of lists |[] :: * -> *|.
\end{itemize}

A type constructor with a more complex kind is the predefined type |Either|, which has
two type parameters. Recall:

< data Either a b = Left a | Right b

The kind of |Either| is |* -> (* -> *)| or simply |* -> * -> *|.

%===============================================================================
\section{Functor}

We have used type classes to provide a common interface to proper
types that support a similar set of operations. Now we can do the same
for type constructors.

An important class of type constructors with similar operations
are generic ``collections'' of kind |* -> *|. These are data structures
like lists and trees that collect values of a given type. 
Because not all data structures support all possible kinds of 
operations on collections, we use different type classes for 
different operations.

%-------------------------------------------------------------------------------
\subsection{Generalized |map|}

We focus here on a well-studied operation, that is both very abstract and
very concrete: the |map| function of lists. Collections that support this
operation are called \emph{functors} and in general we call the operation |fmap|:

< class Functor f where
<   fmap :: (a -> b) -> (f a -> f b)

The best known instance is that for lists:

< instance Functor [] where
<   fmap f l = map f l

%-------------------------------------------------------------------------------
\subsection{The |Functor| Laws}

Not any implementation of |fmap| will do for other types of collections |f|.
Indeed, we expect that implementations of |fmap| respect our intuition and properly
generalize the |map| function for lists. This expectation is captured in two \emph{laws}:
\begin{enumerate}
\item
Firstly, we expect that |fmap| preserves the shape of the collection
and only modifies the elements individually with the given function |f|.
This property is partially captured in the \emph{identity law}:
\begin{equation*}
  |fmap id == id|
\end{equation*}
This law expresses that if we do not modify the individual elements (|f == id|), 
then |fmap id| does not modify the collection as a whole.
\item
The second law is the \emph{fusion law}:
\begin{equation*}
  |fmap f . fmap g == fmap (f . g)|
\end{equation*}
The law expresses that two successive applications of |fmap| can be merged (or fused)
into a single one. This is a form of optimisation in which two loops are replaced
by a single one.
\end{enumerate}

\paragraph{Lawfulness of Lists} Let us check whether the list instance is lawful and
abides by the two laws. As a reminder, here is the definition of |map|:

< map :: (a -> b) -> [a] -> [b]
< map f []      = []
< map f (x:xs)  = f x : map f xs

We prove the first |Functor| law by means of structural induction on the list.
\begin{description}
\sethscode{otherhscode}
\item[Base case:] |l == []|

\begin{spec}
      map id []
  == {- \textrm{def. of |map|} -}
      []
  == {- \textrm{def. of |id|} -}
      id []
\end{spec}

\item[Induction case:] |l == (x:xs)|

<     map id (x:xs)
< == {- \textrm{def. of |map|} -}
<     id x : map id xs
< == {- \textrm{induction hypothesis} -}
<     id x : id xs
< == {- \textrm{def. of |id|} -}
<     x : xs
< == {- \textrm{def. of |id|} -}
<     id (x:xs)

\end{description}

We prove the second law in a similar way:
\begin{description}
\sethscode{otherhscode}
\item[Base case:] |l == []|

\begin{spec}
      map f (map g [])
  == {- \textrm{def. of |map|} -}
      map f []
  == {- \textrm{def. of |map|} -}
      []
  == {- \textrm{def. of |map|} -}
      map (f . g) []
\end{spec}

\item[Induction case:] |l == (x:xs)|

<     map f (map g (x:xs))
< == {- \textrm{def. of |map|} -}
<     map f (g x : map g xs)
< == {- \textrm{def. of |map|} -}
<     f (g x) : map f (map g xs)
< == {- \textrm{def. of |(.)|} -}
<     (f . g) x : map f (map g xs)
< == {- \textrm{induction hypothesis} -}
<     (f . g) x : map (f . g) xs
< == {- \textrm{def. of |map|} -}
<     map (f . g) (x:xs)

\end{description}

In conclusion, the list instance is indeed lawful. Seen from another perspective,
the two |Functor| laws capture two essential properties of lists.

%-------------------------------------------------------------------------------
\subsection{Example Instances of |Functor|}\label{s:monads:functor:ex}

Just like lists, trees also have a lawful |Functor| instance:
\sethscode{customhscode}

> data Tree a = Leaf a | Fork (Tree a) (Tree a)
> 
> instance Functor Tree where
>   fmap f (Leaf x)    = Leaf (f x)
>   fmap f (Fork l r)  = Fork (fmap f l) (fmap f r)

Another instance of |Functor| is that of |Maybe|, which can be seen as a tiny
collection with either 0 or 1 element:
\sethscode{customhscode}

< instance Functor Maybe where
<   fmap f Nothing   = Nothing
<   fmap f (Just x)  = Just (f x)

Two more esoteric minimal |Functor| instances are that for the singleton collection |Id|
and the empty collection |Empty|.
\sethscode{customhscode}

< data Id a     = Id a
< data Empty a  = Empty
< 
< instance Functor Id where
<   fmap f (Id x) = Id (f x)
<
< instance Functor Empty where
<   fmap f Empty = Empty

\begin{exercise}
Prove that the above four instances are lawful.
\end{exercise}

%===============================================================================
\section{Monads as Collections}

The |Monad| concept and corresponding Haskell type class build on the notion
of a |Functor|.

There are two ways to think concretely about the abstract notion of a monad.
The first one is the most simple. It views monads as collections that support two
operations in addition to that of |Functor|. Unfortunately, because this is not
the main view on monads in Haskell, it is not reflected in the standard |Monad|
type class.  Hence, for the sake of studying the collections view, we define
our own type class |Monad'| and come back to Haskell's |Monad| type class when
we consider the second view on monads.

So, without further ado, here is the |Monad'| type class:
\sethscode{customhscode}

> class Functor c => Monad' c where
>   unit  ::  a -> c a
>   join  ::  c (c a) -> c a  

Note that every |Monad'| instance must also have a |Functor| instance with an
accompanying implementation for |fmap|. This is captured in the |Functor| super
class constraint on the |Monad'| declaration. In addition to the |fmap|
implementation, |Monad'| instances supply implementations for two more methods.
\begin{enumerate}
\item The |unit :: a -> c a| method creates a singleton collection of type |c a|
      from the given element of type |a|.
\item The |join :: c (c a) -> c a| method collapses the two levels of a nested collection of type |c (c a)|
      to obtain a flat collection of type |c a|.
\end{enumerate}

To make the above abstract descriptions more concrete
let us consider the instance for the list type:
\sethscode{customhscode}

> instance Monad' [] where
>   unit x    =  [x]
>   join xss  =  concat xss  

where the |Prelude| function |concat| is predefined as
\sethscode{customhscode}

< concat :: [[a]] -> [a]
< concat []        =  []
< concat (xs:xss)  =  xs ++ concat xss

As we can see, |unit x| creates the singleton list |[x]|. Also |join [[1,2],[3],[],[4,5,6]]|
collapses the nested list into the flat list |[1,2,3,4,5,6]|.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Example Instances}

Here are a few more example instances for the types introduced in Section~\ref{s:monads:functor:ex}.
\sethscode{customhscode}

> instance Monad' Tree where
>   unit x  =  Leaf x
>
>   join (Leaf t)    =  t
>   join (Fork l r)  =  Fork (join l) (join r)
>
> instance Monad' Maybe where
>   unit x  =  Just x
>
>   join Nothing   =  Nothing
>   join (Just m)  =  m

Verify that these instances implement the two methods appropriately.

\begin{exercise}
Provide instances for the esoteric types |Id| and |Empty| of Section~\ref{s:monads:functor:ex}.
\end{exercise}

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Lawful Instances}
The |Monad'| type class comes with four laws that all instances must satisfy.

\begin{enumerate}
\item The first law regulates the interaction between |join| and |unit|.
\begin{equation*}
  |join . unit == id|
\end{equation*}
In point-free style this law can be rather cryptic. To give more insight in what the law
expresses, we present it in a different form known as a \emph{commuting diagram}. 
\[ \xymatrix{
     |C A| \ar[d]_-{|unit|} \ar[dr]^-{|id|}&  \\
     |C (C A)| \ar[r]_-{|join|} & |C A|
   }
\]
Every node in the diagram is a type and every arrow denotes a function from one
type to another type.  Hence, a path in the diagram represents the function
that is the composition of the functions denoted by the initial arrows.

We say that the diagram commutes because all paths with the same start and end
points are equivalent. In this case |join . unit| and |id| are equivalent, as
already expressed by the textual law. What makes the commuting diagram more
informative (and more attractive) than the textual law is the fact that it
makes the involved types explicit.

In this case the diagram makes it more apparent that if we first wrap a
singleton collection around an existing collection with |unit| and then flatten
the resulting nested collection with |join| we end up with the original
collection.

In order to illustrate the law, we show a special form of the commuting diagram
where we replace the types with example values.
\[ \xymatrix{
     |[1,2,3] :: [Int]| \ar[d]_-{|unit|} \ar[dr]^-{|id|}&  \\
     |[[1,2,3]] :: [[Int]]| \ar[r]_-{|join|} & |[1,2,3] :: [Int]|
   }
\]

\item The second law also considers a different interaction between |join| and |unit|.
\begin{equation*}
  |join . fmap unit == id|
\end{equation*}
The corresponding commuting diagram is:
\[ \xymatrix{
     |C A| \ar[d]_-{|fmap unit|} \ar[dr]^-{|id|}&  \\
     |C (C A)| \ar[r]_-{|join|} & |C A|
   }
\]
This law is very similar to the first one. The essential difference is that we now do not
create a singleton layer around the original collection with |unit|, but around the individual elements
inside the collection with |fmap unit|. Flattening with |join| again yields the original collection.

Here is a concrete example.
\[ \xymatrix{
     |[1,2,3] :: [Int]| \ar[d]_-{|fmap unit|} \ar[dr]^-{|id|}&  \\
     |[[1],[2],[3]] :: [[Int]]| \ar[r]_-{|join|} & |[1,2,3] :: [Int]|
   }
\]

\item The third law considers the interaction of two |join|s.
\begin{equation*}
  |join . join == join . fmap join|
\end{equation*}
The corresponding commuting diagram is:
\[ \xymatrix{
     |C (C (C A))| \ar[d]_-{|join|} \ar[r]^-{|fmap join|}& |C (C A)| \ar[d]^-{|join|} \\
     |C (C A)| \ar[r]_-{|join|} & |C A|
   }
\]
Both paths in the diagram turn a collection with 3 levels of nesting into a
flat collection by successively collapsing 2 levels. The path on the left first
collapses the two outer levels, while the path on the right first collapses the
two inner levels. The law states that it does not matter which one
you pick.

Here is an example.
\[ \xymatrix{
     |[[[1,2],[3]],[[4]]] :: [[[Int]]]| \ar[d]_-{|join |} \ar[r]^-{|fmap join|} & |[[1,2,3],[4]] :: [[Int]]| \ar[d]^-{|join|}  \\
     |[[1,2],[3],[4]] :: [[Int]]| \ar[r]_-{|join|} & |[1,2,3,4] :: [Int]|
   }
\]

\item The final law governs the interaction between |fmap| and |unit|.
\begin{equation*}
  |fmap f . unit == unit . f|
\end{equation*}
where |f :: A -> B| can be any function.
The corresponding commuting diagram is:
\[ \xymatrix{
     |A| \ar[d]_-{|unit|} \ar[r]^-{|f|}& |B| \ar[d]^-{|unit|} \\
     |C A| \ar[r]_-{|fmap f|} & |C B|
   }
\]
The law states that we can choose whether we first transform a value with |f|
and then wrap it in a singleton collection, or first wrap the value in a singleton
collection and the transform it with |fmap f|.

This is an example.
\[ \xymatrix{
     |1 :: Int| \ar[d]_-{|unit|} \ar[r]^-{|odd|}& |True :: Bool| \ar[d]^-{|unit|} \\
     |[1] :: [Int]| \ar[r]_-{|fmap odd|} & |[True] :: [Bool]|
   }
\]
\end{enumerate}

\begin{exercise}
Prove that the |Maybe| instance is lawful.
\end{exercise}

\begin{exercise}
Prove that the list instance is lawful.
\end{exercise}

\begin{exercise}
Prove that the |Tree| instance is lawful.
\end{exercise}

\begin{exercise}
Draw the commuting diagrams for the two |Functor| laws.
\end{exercise}

%===============================================================================
\section{Monads for Computations with Side Effects}

The main use of monads in Haskell is for modelling computations with
side-effects.

%-------------------------------------------------------------------------------
\subsection{Motivating Example}

Consider a datatype |Exp| for representing simple arithmetic expressions.
\sethscode{customhscode}

> data Exp  =  Lit Int | Add Exp Exp

The |Lit| constructor creates simple integer literals (or constants), and the
|Add| constructor denotes the addition of two subexpressions. For instance,
|Add (Lit 1) (Lit 2)| denotes the expression |1 + 2|.

Of course |Exp| only captures the syntax of arithmetic expressions. We capture
their semantics, i.e., the evaluation of an expression tree, in a function:
\sethscode{customhscode}

> eval :: Exp -> Int
> eval (Lit n)      =  n
> eval (Add e1 e2)  =  eval e1 + eval e2

Note that |eval| is what we call a \emph{pure} function: it is a function in
the mathematical sense that maps every input expression tree onto an integer.
\sethscode{interactivehscode}

< ?> eval (Add (Lit 1) (Lit 2))
< 3

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Expressions with Division}

To spice things up, we extend the expression syntax with a constructor |Div|
for division.
\sethscode{customhscode}

> data Exp2  =  Lit2 Int | Add2 Exp2 Exp2 | Div2 Exp2 Exp2

This obviously requires an extended evaluation function.
\sethscode{customhscode}

> eval2 :: Exp2 -> Int
> eval2 (Lit2 n)      =  n
> eval2 (Add2 e1 e2)  =  eval2 e1 + eval2 e2
> eval2 (Div2 e1 e2)  =  eval2 e1 `div` eval2 e2

Unfortunately, this definition is no longer a pure function.
\sethscode{interactivehscode}

< ?> eval2 (Div2 (Lit2 1) (Lit2 0))
< *** Exception: divide by zero

It raises an exception upon division by zero and hence does not return a result
for every possible expression. This makes |eval| a \emph{partial} function: its
result is not defined for every value in its domain.

While this exception mechanism is common to many programming languages, it is
considered a violation of the purely functional programming philosophy where we
want to stay as close as possible to the mathematical ideal of a total function
that is defined for all values of its domain.

\paragraph{Explicit Partiality}

Fortunately, instead of relying on the impure exception mechanism, we can model
the partiality of |eval| in a purely functional way. The key is to be explicit about
the partiality in the return type.
For this purpose we use the |Maybe| type constructor.
\sethscode{customhscode}

> eval3 :: Exp2 -> Maybe Int

It signals that |eval| returns either |Just| a value or |Nothing|. Now |eval|
can be defined as a total function that explicitly returns |Nothing| when there
is no sensible result.
\sethscode{customhscode}

> eval3 (Lit2 n)        = Just n
> eval3 (Add2 e_1 e_2)  = case eval3 e_1 of
>                           Just n_1  -> case eval3 e_2 of 
>                                          Just n_2  -> Just (n_1 + n_2)
>                                          Nothing  -> Nothing
>                           Nothing  -> Nothing
> eval3 (Div2 e_1 e_2)  = case eval3 e_1 of
>                           Just n_1  -> case eval3 e_2 of 
>                                          Just n_2  -> if (n_2 == 0)
>                                                         then Nothing
>                                                         else Just (n_1 `div` n_2)
>                                          Nothing  -> Nothing
>                           Nothing  -> Nothing

Observe in the above definition how the |Maybe| result type
forces us to explicitly anticipate the potential failure at all
(recursive) invocations of |eval3|. This is a good thing because it
forces us to pause and think about how we want to handle failure.

The downside is that the explicit handling of partiality makes the definition
of |eval3| highly verbose. Luckily we can remedy this issue with a healthy dose
of abstraction. Observe that the following pattern occurs four times in the above code.
\sethscode{otherhscode}

<    case ... of
<      Just x   ->  ... x ...
<      Nothing  ->  Nothing

Let us capture this pattern in a function and call it |bind|.
\sethscode{customhscode}

> bind :: Maybe a -> (a -> Maybe b) -> Maybe b
> bind m f  =  case m of
>                Just x   -> f x
>                Nothing  ->  Nothing

This function takes two partial computations. The first, |m|, maybe returns a
result of type |a|. The second, |f|, depends on the result of the first
computation and maybe returns a result of type |b|. The |bind| function
composes or \emph{glues} these two computations together: if |m| produces a
result, it is passed to |f|; if |m| fails, the composition fails.

The definition of |eval4| in terms of |bind| is much more concise.
\sethscode{customhscode}

> eval4 (Lit2 n)        =  Just n
> eval4 (Add2 e_1 e_2)  =  eval4 e_1 `bind` \n_1  -> 
>                          eval4 e_2 `bind` \n_2  ->
>                          Just (n_1 + n_2)
> eval4 (Div2 e_1 e_2)  =  eval4 e_1 `bind` \n_1 ->
>                          eval4 e_2 `bind` \n_2 ->
>                          if (n_2 == 0)
>                            then Nothing
>                            else Just (n_1 `div` n_2)

Note that the partiality of recursive |eval4| calls is no longer handled explicitly;
the |bind| function fully encapsulates this. This means that the code is not only shorter,
but also expresses only the interesting aspect of the computation.

%-------------------------------------------------------------------------------
\subsection{Generalisation}

We can generalise the use of |Maybe| to model partiality in a purely functional
fashion to using other monads for modelling other kinds of side-effects. Every
time the idea is that the type |m a| models a computation with results of type
|a| that can have additional, impure behavior modelled by the monad |m|.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Bind-Centrism}
The function |bind| plays a central role in this approach. It allows us to
compose two computations with side-effects |m a| and |a -> m b| where the
latter depends on the former. In fact, |bind m f| acts as function application
for computations with side-effects where |m| is the argument and |f| is the
function. In contrast with regular pure function application, which is written
with a space in Haskell, both the argument and the function can produce side
effects. The order of the arguments of |binds| indicates in which order the
side effects happen: first those of the argument and then those of the
function.
 
In order to express |bind|, the |Monad'| type class we have introduced earlier
is enough:
\sethscode{customhscode}

> bind2 :: Monad' m => m a -> (a -> m b) -> m b
> bind2 m f = join (fmap f m)

Verify that this boils down to the earlier definition when |m| is instantiated
to |Maybe|.

However, given the central role of |bind| for modelling side effects, Haskell
does not use the |Monad'| type class. Instead it uses the following |bind|-centric 
interface:
\sethscode{customhscode}

< class Monad m where
<   return :: a -> m a
<   (>>=) :: m a -> (a -> m b) -> m b

Here |bind| is actually written with the infix operator |(>>=)|, but still
pronounced \emph{bind}.  Also our earlier |unit| function has been renamed to
|return|, which is more apt when talking about computations that immediately
produce a result without generating any side effect.

Note that the |Monad| type class does not have |Functor| as an explicit super class.\footnote{
In recent versions of Haskell, this situation has been remedied. In fact, the remedy also
involves a type class |Applicative| that provides a more expressive interface than |Functor| but less expressive than |Monad|.}
Even so, every |Monad| is necessarily a |Functor| and we can express |fmap| for any |Monad| in terms of |(>>=)| and |return| thus:
\sethscode{customhscode}

> fmapM :: Monad m => (a -> b) -> (m a -> m b)
> fmapM f m  =  m >>= \x -> return (f x)

\begin{exercise}
Verify that this definition coincides with the |fmap| implementation given earlier for |Maybe|.
\end{exercise}

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Dynamism}
We can also express |join| in terms of |(>>=)|.
\sethscode{customhscode}

> join2 :: Monad m => m (m a) -> m a
> join2 m  =  m >>= \n -> n

This definition highlights the dynamic character of monadic computations. A
computation of type |m (m a)| is one that produces another computation as a
result. With |join| we can run this dynamically produced computation
immediately after it has been produced. Hence, the plan for a dynamic
computation does not have to be known in advance, it can be made up along the
way.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{The |Monad| Laws}

We have already expressed two |Functor| laws and four |Monad'| laws. We can reformulate
these in terms of three concise |Monad| laws.

\begin{enumerate}
\item  The \emph{left unit} law states that |return| is the unit of |(>>=)| on the left.
\begin{equation*}
  |return x >>= f  ==  f x|
\end{equation*}
This is obvious since no side effects happen in |return|.
\item  For similar reasons the \emph{right unit} law states that |return| is also the unit of |(>>=)| on the right.
\begin{equation*}
  |m >>= return  ==  m|
\end{equation*}
\item  The \emph{associativity} law states that |(>>=)| is associative.
\begin{equation*}
  |(m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)|
\end{equation*}
Since the nesting does not matter, we often omit the parentheses and write simply |m >>= f >>= g|.
\end{enumerate}

\begin{exercise}
Prove the |Monad| laws in terms of the |Monad'| and |Functor| laws, and the definition of |(>>=)|
in terms of |fmap| and |join|. 
\end{exercise}

\begin{exercise}
Prove the |Functor| and |Monad'| laws in terms of the |Monad| laws, and the definitions of |fmap|
and |join| in terms of |return| and |(>>=)|.
\end{exercise}

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{The |do| Notation}

In order to facilitate the use of monadic computations, Haskell provides syntactic sugar that makes
the use of |(>>=)| less cumbersome. This syntactic sugar allows writing a pipeline of |(>>=)| applications
in the form of a |do| block: 
\sethscode{otherhscode}
\begin{eqnarray*}
\begin{minipage}{9cm}
< m >>= \x1 -> f1 x1 >>= \x2 -> ... >>= \x_n -> f_n x1 ... x_n
\end{minipage}
& |==| & 
\begin{minipage}{3cm}

< do  x1 <- m 
<     x2 <- f1 x1
<     ...
<     x_n <- ...
<     f_n x1 ... x_n
\end{minipage}
\end{eqnarray*}
Every step of the pipeline is written on its own line. If a step produces a result that
is used in later lines, it is given a name with the left arrow notation |x <-|.

To illustrate the |do| notation, we rewrite the definition of the monadic |eval| function 
in terms of it.
\sethscode{customhscode}

> eval5 :: Exp2 -> Maybe Int
> eval5 (Lit2 n)        =  return n
> eval5 (Add2 e_1 e_2)  =  do  n_1 <- eval5 e_1
>                              n_2 <- eval5 e_2
>                              return (n_1 + n_2)
> eval5 (Div2 e_1 e_2)  =  do  n_1 <- eval5 e_1
>                              n_2 <- eval5 e_2
>                              if (n_2 == 0)
>                                then Nothing
>                                else return (n_1 `div` n_2)

Here are two important caveats for novice users of the |do| notation:
\begin{enumerate}
\item
  The syntax is sensitive to indentation: all rules of a |do| block
  have to be neatly aligned and start in the same column. If a line is indented further
  than its predecessor, the compiler generates a syntax error of the form
  \texttt{parse error on input `<-'}.

\item
  The last line in a |do| block can never be of the form |x <- m| because there
  are no further lines that could refer to |x|. If you make a mistake against this rule,
  the compiler treats you to the error message \texttt{The last statement in a 'do' block
  must be an expression}.
\end{enumerate}

From now on we often use the |do| notation when writing monadic code.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{The |(>>)| Operator}

The predefined |(>>)| operator is a special form of |(>>=)|.
\sethscode{customhscode}

< (>>) :: m a -> m b -> m b
< ma >> mb  = ma >>= \ _ -> mb 

The monadic computation |m >> n| sequentially composes |m| and |n| without passing
the result of |m| to |n|. Instead the intermediate result is simply discarded. Hence
the only reason to execute |m| is for its side effects.

A good use of |(>>)| is in combination with the |guard| function. 
\sethscode{customhscode}
\begin{spec}
guard :: Bool -> Maybe ()
guard b = 
  if b  then return ()
        else Nothing
\end{spec}
This function returns an uninteresting result of type |()|, but has the interesting
side effect of failing when the given boolean is |False|.

We can use this as follows in our running example.
\sethscode{customhscode}

< eval (Div e_1 e_2)  = do  n_1 <- eval e_1
<                           n_2 <- eval e_2
<                           guard (n_2 /= 0) >> return (n_1 `div` n_2)

Moreover, we do not have to mention the |(>>)| operator explicitly. 
It is used implicitly by the |do| notation whenever we do not name the result
of a step.
\sethscode{customhscode}

< eval (Div e_1 e_2)  = do  n_1 <- eval e_1
<                           n_2 <- eval e_2
<                           guard (n_2 /= 0) 
<                           return (n_1 `div` n_2)

%-------------------------------------------------------------------------------
\subsection{The |State| Monad}

We now present the |State| monad which models the \emph{mutable state} side effect.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{List of Random Numbers}
Consider the problem of generating a list of random numbers in Haskell. For this purpose
we can use the |RandomGen| type class for random number generators.
\sethscode{customhscode}

< class RandomGen g where
<   next :: g -> (Int, g)
<   -- other methods omitted

In fact, this type class provides an interface for pseudo-random number
generators, as actual randomness cannot be obtained in a purely functional
language or even a fully deterministic computer. The idea is to approximate
true randomness by pseudo-randomness, which uses a sufficiently complex
deterministic algorithm that only seems to generate random results.

The idea is that the generator has a state, in the above interface represented
by a value of type |g|. The |next| function produces, based on some algorithm,
from that state a pseudo-random number and a new state. Given the same initial
state, the |next| function always generates the same number and next state;
there is no actual randomness involved, only purely functional behavior. The
way to obtain multiple (typically different) pseudo-random numbers, is to call
|next| on the second state, third state, and so on.

We capture this approach in the |randomList| function that produces a list of
|n| random numbers.
\sethscode{customhscode}

> randomList :: RandomGen g => Int -> g -> ([Int], g)
> randomList n g0
>   | n > 0 
>   =  let (x,   g1)  =  next g0
>          (xs,  g2)  =  randomList (n - 1) g1
>      in  (x:xs, g2)
>   | otherwise
>   =  ([], g0)

Observe that the explicit threading of the generator state is rather clumsy. It
not only clutters the definition of |randomList|, but also makes it easy to
introduce logical errors.  For instance, if we mistakenly write the recursive
call to |randomList| as |randomList (n-1) g0|, i.e., mentioning the initial
generator state, we always obtain a list of |n| identical numbers.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Threading State with Bind}

We can clean up the verbose threading of state by encapsulating it in a function.
As a starting point, we introduce a type synonym for a state-passing computation.
\sethscode{customhscode}

> type SP s a = s -> (a, s)

In words, a state-passing computation is one that takes an input state of type
|s| and produces a output value of type |a| alongside an output state of type
|s|. The |bindSP| function glues together two state-passing computations, whether
the latter depends on the result of the former.
\sethscode{customhscode}

> bindSP :: SP s a -> (a -> SP s b) -> SP s b
> bindSP m f  =  \s0  ->  let  (x, s1) = m s0
>                         in   f x s1

Observe how |bindSP| threads the state through both computations.

We can also represent a pure computation (i.e., one that is independent
of the state) as a state-passing computation.
\sethscode{customhscode}

> pureSP :: a -> SP s a
> pureSP x  =  \s -> (x, s)

The two building blocks for state-passing computations nicely unclutter
our definition of |randomList|.
\sethscode{customhscode}

> randomList2 :: RandomGen g => Int -> SP g [Int]
> randomList2 n
>   | n > 0 
>   =  next                 `bindSP` \x -> 
>      randomList2 (n - 1)  `bindSP` \xs ->
>      pureSP (x:xs)
>   | otherwise
>   =  pureSP []

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{The |State| Monad}

Clearly, state passing is a computational effect that forms a monad. In order
to make it a proper instance of the monad type class we do need a type
constructor |m| of kind |* -> *| that can be applied to the result type |a :: *| of
the computation.  The type |s -> (a, s)| obviously cannot be split up into |m a| for some
|m|.  Also the type synonym |SP s a| cannot be used, because\footnote{for
technical reasons related to how type checking works} the Haskell language does not
allow type synonyms to be split; |SP s| is not a valid type in Haskell.

The solution to this conundrum is to define a new type in Haskell.
\sethscode{customhscode}

> data State s a = StateC (SP s a)

Because this is a new type with the |a| parameter conveniently in the last position,
it is perfectly valid to mention the type constructor |State s :: * -> *| separately.
At last we have a suitable type constructor to instantiate the |Monad| type class.

Note that |State| is just a simple wrapper around |s -> (a, s)|. Hence our instance
basically replicates the earlier definitions, but adds a little bit of noise converting
between the |State| type and the underlying representation.
\sethscode{customhscode}

> instance Monad (State s) where
>   return x  =  StateC (pureSP x)
>   m >>= f   =  StateC (bindSP (runState m) (runState . f))
>
> runState :: State s a -> SP s a
> runState (StateC m)  =  m

This slightly affects the definition of |randomList|.
\sethscode{customhscode}

> randomList3 :: RandomGen g => Int -> State g [Int]
> randomList3 n
>   | n > 0 
>   =  do  x   <-  StateC next              
>          xs  <-  randomList3 (n - 1)
>          return (x:xs)
>   | otherwise
>   = return []

\begin{exercise}
Prove that the |Monad| instance is lawful.
\end{exercise}

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Getting and Putting}

The acts of reading and writing the state are usually encapsulated in two functions.
\sethscode{customhscode}

> get :: State s s
> get  =  StateC (\s -> (s, s)) 
>
> put :: s -> State s ()
> put s'  =  StateC (\s -> ((), s'))

These allow us to avoid explicitly mentioning the |StateC| constructor in our running example. 
\sethscode{customhscode}

> randomList4 :: RandomGen g => Int -> State g [Int]
> randomList4 n
>   | n > 0 
>   =  do  x   <-  nextS              
>          xs  <-  randomList4 (n - 1)
>          return (x:xs)
>   | otherwise
>   = return []
>   where
>     nextS :: RandomGen g => State g Int
>     nextS =
>       do g <- get
>          let (x, g') = next g
>          put g'
>          return x

Avoiding the |StateC| constructor is a good thing because it avoids reliance on
the actual representation of the state monad. However, it is also bad because
the definition of |nextS| is rather verbose. For this reason, a third primitive is
often introduced to modify the current state.
\sethscode{customhscode}

> modify :: (s -> (a, s)) -> State s a
> modify f  =  StateC f

This leads us to our final definition of |randomList|.
\sethscode{customhscode}

> randomList5 :: RandomGen g => Int -> State g [Int]
> randomList5 n
>   | n > 0 
>   =  do  x   <-  modify next              
>          xs  <-  randomList5 (n - 1)
>          return (x:xs)
>   | otherwise
>   = return []

%-------------------------------------------------------------------------------
% \subsection{De Type Class}
% 
% De type class voor monads introduceert twee nieuwe operaties:
% 
% < class Monad m where
% <   return  :: a -> m a
% <   (>>=)   :: m a -> (a -> m b) -> m b
% 
% De |return| operatie (let niet op de naam) geeft je een manier om een
% singleton-collectie te maken. De |>>=| operatie (spreek uit: bind) is
% een variante op |fmap|. Het verband wordt duidelijker als we de parameters
% herschikken.
% 
% < (=<<) :: (a -> m b) -> (m a -> m b)
% < f =<< m = m >>= f 
% 
% en de signatuur naast die van |fmap| zetten:
% 
% < fmap :: (a -> b) -> (m a -> m b)
% 
% Het verschil zit hem in de functieparameter. Bij |fmap| wordt elk element van
% type |a| afgebeeld op juist \'e\'en element van type |b|. Bij |>>=| daarentegen
% wordt elk element van type |a| afgebeeld op een collectie |m b|, en |>>=| voegt
% vervolgens al deze collecties samen. 
% 
% %-------------------------------------------------------------------------------
% \subsection{Lijsten}
% 
% Hier is de voorbeeldimplementatie voor lijsten:
% 
% < instance Monad [] where
% <   return x  = [x]
% <   m >>= f   = concat (map f m)
% 
% Deze implementatie volgt netjes de informele beschrijving: De |return|-methode
% bouwt een singleton-lijst, terwijl |>>=| elk element vervangt door een nieuwe lijst en
% deze element-lijsten concateneert.
% 
% Hier zijn enkele voorbeelden van het gebruik van deze |Monad| instantie.
% 
% < > return 5
% < [5]
% <
% < > [1,2,3] >>= (\n -> [n-1,n+1])
% < [0,2,1,3,2,4]
% <
% < > [1,2] >>= (\n -> [3,4] >>= (\m -> return (n * m)))
% < [3,4,6,8]
% 
% %-------------------------------------------------------------------------------
% \subsection{De |Monad|-wetten}
% 
% Er zijn drie wetten die ervoor zorgen dat alle instanties van |Monad| voldoen
% aan de intu\"itie.
% 
% De eerste twee wetten drukken uit dat |return| een ``neutraal element'' is ten
% opzichte van |>>=|.
% \begin{eqnarray*}
%   |return x >>= f| & |==| & |f x| \\
%   |m >>= return |  & |==| & |m|
% \end{eqnarray*}
% Deze twee wetten drukken uit dat het aanmaken van singleton collecties
% overbodig is in combinatie met |>>=|.
% 
% De derde wet druk uit dat |>>=| \emph{associatief} is:
% \begin{eqnarray*}
%   |(m >>= f) >>= g| & |==| & |m >>= (\x -> f x >>= g)|
% \end{eqnarray*}
% 
% \paragraph{Controle voor Lijsten}
% We bewijzen dat de lijstinstantie voldoet aan de drie |Monad|-wetten.
% 
% We zullen gebruik maken van enkele hulpstellingen:
% \begin{eqnarray}
%   |[] >>= f| & |==|   & |[]| \\
%   | map f . concat |  & |==| & |concat . map (map f)| \\
%   | concat . concat | & |==| & |concat . map concat | 
% \end{eqnarray}
% 
% <      [] >>= f
% < == {- def. van |(>>=)| -}
% <      concat (map f [])
% < == {- def. van |map| -}
% <      concat []
% < == {- def. van |concat| -}
% <      []
% 
% De eerste wet bewijzen we met pure equational reasoning.
% 
% <      return x >>= f
% < == {- def. van |return| -}
% <      [x] >>= f
% < == {- def. van |(>>=)| -}
% <      concat (map f [x])
% < == {- def. van |map| -}
% <      concat [f x]
% < == {- def. van |concat| -}
% <      f x
% 
% De tweede wet bewijzen we met structurele inductie.
% \begin{description}
% \item[Basisgeval:] |m = []|
% 
% <      [] >>= return
% < == {- hulpstelling -}
% <      []
% 
% \item[Inductief geval:] |m = (x:xs)|
% <      (x:xs) >>= return
% < == {- def. van (>>=) -}
% <      concat (map return (x:xs))
% < == {- def. van |map| -}
% <      concat (return x : map return xs)
% < == {- def. van |concat| -}
% <      return x ++ concat (map return xs)
% < == {- inductiehypothese -}
% <      return x ++ xs
% < == {- def. van |return| -}
% <      [x] ++ xs
% < == {- def. van |(++)| -}
% <      x:xs
% \end{description}
% 
% De derde wet bewijzen we ook met pure equational reasoning.
% 
% <      (m >>= f) >>= g
% < == {- def. van |(>>=)| -}
% <      concat (map f m) >>= g
% < == {- def. van |(>>=)| -}
% <      concat (map g (concat (map f m))
% < == {- hulpstelling -}
% <      concat (concat (map (map g) (map f m)))
% < == {- fusiewet van |map| -}
% <      concat (concat (map (map g . f) m))
% < == {- hulpstelling -}
% <      concat (map concat (map (map g . f) m))
% < == {- fusiewet van |map| -}
% <      concat (map (concat . map g . f) m)
% < == {- def. van |(>>=)| -}
% <       concat (map (\x -> f x >>= g) m)
% < == {- def. van |(>>=)| -}
% <      m >>= (\x -> f x >>= g) 
% 
% %-------------------------------------------------------------------------------
% \subsection{Voorbeeldinstanties}
% 
% Hier zijn twee voorbeelden van |Monad|-instanties. Ga zelf na dat aan de wetten voldaan is.
% 
% \paragraph{Bomen}
% Net zoals voor lijsten, kunnen we voor bomen een instantie maken: 
% 
% > instance Monad Tree where
% >   return x           = Leaf x
% >   (Leaf x)    >>= f  = f x
% >   (Fork l r)  >>= f  = Fork (l >>= f) (r >>= f)
% 
% Merk op dat de definitie van |>>=| hier makkelijker is omdat het concateneren van bomen
% makkelijk gaat met de |Fork| constructor.
% 
% \paragraph{|Maybe|}
% Ook de kleine collectie |Maybe| is een |Monad|:
% 
% < instance Monad Maybe where
% <   return x        = Just x
% <   (Just x) >>= f  = f x
% <   Nothing  >>= f  = Nothing 
% 
% %-------------------------------------------------------------------------------
% \subsection{|Monad|s als berekeningen}
% 
% Het concept |Monad| is (net zoals |Functor|) een vrij abstract begrip. Als je
% voor een typeconstructor de nodige operaties kan implementeren die aan de
% wetten voldoen, dan is het een |Monad|. Een instantie moet dus niet per se
% overeenkomen met de intu\"itie van een collectie.
% 
% In het bijzonder is er een (gedeeltelijk overlappende) klasse van instanties
% die voldoet aan een andere interpretatie, deze van \emph{berekeningen}.  In die
% interpretatie stelt een expressie van type |m a| een berekening voor van soort
% |m| die een resultaat berekent van type |a|. De berekeningsoort |m| geeft aan
% dat het niet gaat om een pure berekening maar een berekening waarin
% (neven)-\emph{effecten} kunnen optreden.
% 
% \paragraph{|Maybe| Opnieuw Bekeken}
% Het |Maybe| type is hier een voorbeeld van. Een expressie van type |Maybe Int|
% kan je in plaats van als collectie ook interpreteren als een berekening die een
% |Int| oplevert, maar potentieel als neveneffect ook kan falen. Daarbij laat
% |return| toe om een pure berekening te bekijken als een potentieel falende
% berekening (die natuurlijk niet faalt).  Met |>>=| maken we een sequenti\"ele
% compositie van potentieel falende berekeningen. Daarbij dient de uitvoer van de ene
% berekening als invoer van de volgende. Als een van de berekeningen faalt, dan faalt
% ook de sequenti\"ele compositie.
% 
% We schetsen een kleine toepassing hiervan op basis van een interpreter voor een 
% kleine taal van aritmetische expressies.
% 
% > data Exp = Lit Int | Add Exp Exp | Div Exp Exp
% >
% > eval :: Exp -> Maybe Int
% > eval (Lit n)      = Just n
% > eval (Add e_1 e_2)  = case eval e_1 of
% >                       Just n_1  -> case eval e_2 of 
% >                                    Just n_2  -> Just (n_1 + n_2)
% >                                    Nothing  -> Nothing
% >                       Nothing  -> Nothing
% > eval (Div e_1 e_2)  = case eval e_1 of
% >                       Just n_1  -> case eval e_2 of 
% >                                    Just n_2  -> if (n_2 == 0)
% >                                                  then Nothing
% >                                                  else Just (n_1 `div` n_2)
% >                                    Nothing  -> Nothing
% >                       Nothing  -> Nothing
% 
% De interpreter geeft |Maybe Int| terug om dat de uitvoering mogelijks kan falen. 
% Faling treedt op bij deling door nul.
% 
% De bovenstaande interpreter is zeer verboos door de vele pattern matching op
% |Maybe| waarden. Met behulp van de |Monad| instantie van |Maybe| kunnen we de
% interpreter veel compacter herschrijven.
% 
% > eval_2 :: Exp -> Maybe Int
% > eval_2 (Lit n)      = return n
% > eval_2 (Add e_1 e_2)  = eval_2 e_1 >>= \n_1 ->
% >                         eval_2 e_2 >>= \n_2 ->
% > 			  return (n_1 + n_2)
% > eval_2 (Div e_1 e_2)  = eval_2 e_1 >>= \n_1 ->
% >                         eval_2 e_2 >>= \n_2 ->
% >                         if (n_2 == 0)
% >                           then Nothing
% >                           else return (n_1 `div` n_2)
% 
% In deze monadische stijl is het mogelijk falen veel minder expliciet aanwezig. Het wordt
% ge\"encapsuleerd door de |Monad|.
% 
% %-------------------------------------------------------------------------------
% \subsection{De |do| notatie}
% 
% Omdat Haskell monadische berekeningen zo belangrijk vindt, biedt het speciaal
% daarvoor syntactische suiker aan. Deze syntactische suiker maakt het schrijven
% van monadische berekeningen minder verboos en benadrukt tegelijk het
% sequenti\"ele, bijna imperatieve karakter van deze berekeningen.
% 
% De syntactisch suiker laat toe om een compositie van opeenvolgende applicaties van |>>=|
% te noteren als een |do|-blok. De centrale \emph{suiker}-notatie is:
% \begin{eqnarray*}
% |m >>= \x -> f x| & |==| & 
% \begin{minipage}{3cm}
% 
% < do  x <- m 
% <     f x
% 
% \end{minipage}
% \end{eqnarray*}
% en meerdere |>>=| worden vertikaal binnen eenzelfde |do|-blok opgenomen.
% 
% Als voorbeeld herschrijven we de interpreter hierboven met de |do|-notatie:
% 
% > eval_3 :: Exp -> Maybe Int
% > eval_3 (Lit n)      = return n
% > eval_3 (Add e_1 e_2)  = do  n_1 <- eval_3 e_1
% >                             n_2 <- eval_3 e_2
% > 			      return (n_1 + n_2)
% > eval_3 (Div e_1 e_2)  = do  n_1 <- eval_3 e_1
% >                             n_2 <- eval_3 e_2
% >                             if (n_2 == 0)
% >                                 then Nothing
% >                                 else return (n_1 `div` n_2)
% 
% Vanaf nu gebruiken we zoveel mogelijk de |do|-notatie.
% 
% \paragraph{Let op!} Dit zijn twee frequente syntaxfouten die gemaakt worden
% door beginnende gebruikers van de |do|-notatie:
% \begin{enumerate}
% \item
%   De syntax is indentatiegevoelig: alle regels in een |do|-blok moeten netjes
%   in dezelfde kolom beginnen. Als een regel wat meer inspringt dan de vorige, dan
%   krijg je een syntaxfout van de vorm \texttt{parse error on input `<-'}.
% 
% \item
%   De laatste regel in een |do|-blok kan niet van de vorm |x <- m| zijn, want
%   dergelijke regel wordt vertaald naar de onvolledige Haskell-expressie |m >>= \x -> |.
%   Je krijgt als foutboodschap \texttt{The last statement in a 'do' block must be an expression}.
%   
%   
% \end{enumerate}
% 
% %-------------------------------------------------------------------------------
% \subsection{De |>>|-operator}
% 
% De voorgedefinieerde |>>|-operator is een specialisatie van |>>=|:
% \begin{spec}
% (>>) :: m a -> m b -> m b
% ma >> mb  = ma >>= \_ -> mb 
% \end{spec}
% 
% De monadische berekening |ma >> mb| is een sequenti\"ele compositie van |ma| en
% |mb| waarbij het resultaat van |ma| niet wordt doorgegeven aan |mb|. Het wordt
% gewoon genegeerd. De enige reden om |ma| uit te voeren is dus zijn neveneffect.
% 
% Een voorbeeld hiervan is de functie |guard|:
% \begin{spec}
% guard :: Bool -> Maybe ()
% guard b = 
%   if b  then return ()
%         else Nothing
% \end{spec}
% Deze functie geeft een oninteressante resultaat van type |()|, maar kan als
% interessant neveneffect falen.
% 
% In ons voorbeeld kunnen we deze functie als volgt gebruiken:
% 
% < eval_3 (Div e_1 e_2)  = do  n_1 <- eval_3 e_1
% <                             n_2 <- eval_3 e_2
% <                             guard (n_2 /= 0) >> return (n_1 `div` n_2)
% 
% Bovendien wordt impliciet een |>>|-operator geplaatst tussen opeenvolgende
% regels in een |do|-blok waar de |>>=|-operator niet van toepassing is.
% 
% < eval_3 (Div e_1 e_2)  = do  n_1 <- eval_3 e_1
% <                             n_2 <- eval_3 e_2
% <                             guard (n_2 /= 0) 
% <                             return (n_1 `div` n_2)
% 
% %-------------------------------------------------------------------------------
% \subsection{|Monad| $\subset$ |Functor|}
% 
% A |Monad| is special case of a |Functor|. Unfortunately, the two Haskell type
% classes do not exploit this relation. Even worse, the |Monad| type class
% unnecessarily replicates (and naturally also extends) the functionality of
% |Functor|. Indeed, we can expose this replication by implementing |fmap| in
% terms of |return| and |(>>=)|. This shows that |Functor| functionality is 
% fully provided by the |Monad| instances.
% 
% \begin{exercise}
% Provide this implementation of |fmap| and prove that it is lawful using the |Monad| laws.
% \end{exercise}
% 
% %===============================================================================
% \section{Case Study: Parser Monad}
% 
% Het systeem van parsers die we eerder zagen vormt ook een |Monad|. Je kan een
% berekening van type |Parser a| beschouwen als een berekening met als bijkomende
% effecten 1) de consumptie van een deel van de invoer, en 2) meerdere mogelijke
% resultaten.
% 
% We definieerden |Parser| eerder als een type-synoniem. Om echter een instantie van
% |Monad| te maken is het beter om een unieke type te maken. Dit is makkelijk op basis
% van een |newtype|-declaratie:
% 
% > newtype Parser a = P { runP :: String -> [(a, String)] }
% 
% Dit vereist kleine wijzigingen aan de definities van onze combinatoren:
% 
% > item :: Parser Char
% > item = P $ \str -> case str of
% >               []     -> []
% >               (c:cs) -> [(c,cs)]
% >
% > (\/) :: Parser a -> Parser a -> Parser a
% > p_1 \/ p_2 = P $ \str -> runP p_1 str ++ runP p_2 str
% >
% > abort :: Parser a
% > abort = P $ \str -> []
% >
% > ret :: a -> Parser a
% > ret x = P $ \str -> [(x,str)]
% >
% > andThen :: Parser a -> (a -> Parser b) -> Parser b
% > p_1 `andThen` fp_2 = P $ \str_1 -> [(y,str_3) |  (x,str_2) <- runP p_1 str_1,
% >                                                  (y,str_3) <- runP (fp_2 x) str_2]
% 
% Het is duidelijk op basis van de signaturen en de suggestieve namen dat we als volgt een
% |Monad|-instantie maken voor |Parser|:
% 
% > instance Monad Parser where
% >   return x  = ret x
% >   m >>= f   = m `andThen` f
% 
% Nu kunnen we afgeleide parsers als volgt schrijven:
% 
% > digit :: Parser Int
% > digit = do c <- item
% >            if isDigit c
% >              then return (ord c - ord '0')
% >              else abort
% > 
% > number :: Parser Int 
% > number = go 0
% >   where go acc =  (do  d <- digit
% >                        go (acc * 10 + d))
% >                   \/ return acc 
% 
