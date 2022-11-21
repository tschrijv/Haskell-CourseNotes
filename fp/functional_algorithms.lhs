%include Formatting.fmt
%include lhs2TeX.fmt

%if False

%endif

In this chapter, we explore an approach to designing algorithms that leverages
general functional programming techniques (abstraction, higher-order functions,
and the adaptation of algebraic concepts to programming) and Haskell language
features like type classes.

%===============================================================================
\section{Introduction: Left vs. Right}

Haskell offers two structural recursion schemes over lists, the left and the
right fold.

< foldl :: (b -> a -> b) -> b -> [a] -> b
< foldl cl nl []      =  nl
< foldl cn nl (x:xs)  =  foldl cn (cn nl x) xs

< foldr :: (a -> b -> b) -> b -> [a] -> b
< foldr cr nr []      =  nr
< foldr cr nr (x:xs)  =  cr x (foldr cr nr xs)

In the case where the types |a| and |b| are the same, we can compare their
behavior for the same |c| and |n|---the pair of |c| and |n| is called
the \emph{algebra}.\footnote{The mathematical field of \emph{Algebra} studies algebras, and folding
is a computer science application of this.}

< foldl c n [x1,x2,x3,x4]   =  c (c (c (c n x1) x2) x3) x4
< foldr c n [x1,x2,x3,x4]   =  c x1 (c x2 (c x3 (c x4 n)))

\begin{figure}
\begin{center}
\begin{forest}
 [,phantom
  [|c|,tier=one
   [|c|,tier=two
    [|c|,tier=three
     [|c|,tier=four
      [|n|,tier=five]
      [|x1|,tier=five]
     ]
     [|x2|,tier=four]
    ]
    [|x3|,tier=three]
   ]
   [|x4|,tier=two]
  ]
  [$~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~$]
  [|c|,tier=one
   [|x1|,tier=two]
   [|c|,tier=two
    [|x2|,tier=three]
    [|c|,tier=three
     [|x3|,tier=four]
     [|c|,tier=four
      [|x4|,tier=five]
      [|n|,tier=five]
     ]
    ]
   ]
  ]
 ]
\end{forest}
\end{center}
\caption{Evaluation trees of |foldl c n [x1,x2,x3,x4]| (left) and  |foldr c n [x1,x2,x3,x4]| (right).}\label{fig:leftvsright}
\end{figure}

The first accumulates values from the left and the second from the right (see Figure~\ref{fig:leftvsright}). This
can yield very different results. For example,

< > foldl (-) 0 [1,2,3,4]
< -10
< > foldr (-) 0 [1,2,3,4]
< 2

However, there are algebras where accumulating form the left or the right yields the same output 
for any list. For example,

< > foldl (+) 0 [1,2,3,4]
< 10
< > foldr (+) 0 [1,2,3,4]
< 10 

or

< > foldl (++) [] [[1,2],[],[3],[4]]
< [1,2,3,4]
< > foldr (++) [] [[1,2],[],[3],[4]]
< [1,2,3,4]

\paragraph{Challenge}
Can you think of other algebras where accumulating from the left and the right
yields the same result?

%===============================================================================
\section{Semigroups and Monoids}

There are two key properties of the algebra that are responsible for making it
impervious to the side of accumulation.

%-------------------------------------------------------------------------------
\subsection{Associativity}

The first key property is \emph{associativity}. An operator |(<>) :: A -> A -> A| is
associative, when changing the bracketing of an expression has no impact on the result:
\begin{equation*}
|x <> (y <> z) ==  (x <> y) <> z| 
\end{equation*}
Because the bracketing does not matter for an associative operator, we often
drop it and just write |x <> y <> z|.

As we can see in the above examples, |(+)| and |(++)| are both associative operators 
while |(-)| is not.

In the field of Abstract Algebra,\footnote{Abstract Algebra is a subfield of Algebra that
classifies algebras based on their properties.} a set |A| with an associative operator |(<>)| is known as a
\emph{semigroup}. Haskell has adopted this concept in its semigroup type class.

< class Semigroup a where
<   (<>) :: a -> a -> a

Any instance of this type class is of course required to provide an associative
implementation for the ``diamond'' operator |(<>)|.

For example,

< instance Semigroup [a] where
<   (<>) = (++)

\paragraph{Challenge}
How many associative |Int| operators can you think of? Try to get to 10.

%-------------------------------------------------------------------------------
\subsection{Aside: Newtype Wrappers}

Two obvious associative operators for |Int| are |(+)| and |(*)|.  This poses a
problem, because Haskell does not allow two instances of the same type class
for the same type. Hence, we can't create two |Semigroup| instances for |Int|
with these two operators. That makes sense of course, as Haskell could then not know
whether |(+)| or |(*)| is intended when it sees the diamond operator |(<>)| at type
|Int|. It is to avoid this kind of ambiguity that Haskell does not allow multiple
instances for the same type.

How then should we deal with this situation when there are multiple sensible
algebras? The common approach in Haskell is to create distinct types, that are
copies of the original type and that each have their own instance.

We can create a copy of |Int| by means of a new datatype with one constructor
that has one field:

< data IntCopy = MkIntCopy Int

In fact, Haskell has a separate keyword |newtype| for this special case of
datatype.\footnote{|newtype| and |data| behave slightly differently under laziness, but that is beyond this course.}
Types created with |newtype| are called ``newtype wrappers''.

< newtype IntCopy = MkIntCopy Int

Typically, the new types are named after their algebra:

< newtype Sum     = Sum Int
< newtype Product = Product Int

Because they are separate types, they can each have their own |Semigroup| instance:

< instance Semigroup Sum where
<   Sum x <> Sum y = Sum (x + y)
<
< instance Semigroup Product where
<   Product x <> Product y = Product (x * y)

In fact, because |(+)| and |(*)| are meant to be associative for all instances of |Num|, the
module |Data.Semigroup| readily provides more general definitions:

< newtype Sum a     = Sum a
< newtype Product a = Product a
<
< instance Num a => Semigroup (Sum a) where
<   Sum x <> Sum y = Sum (x + y)
<
< instance Num a => Semigroup (Product a) where
<   Product x <> Product y = Product (x * y)

It is worth browsing the documentation of |Data.Semigroup| to learn about other available 
semigroup instances.

%-------------------------------------------------------------------------------
\subsection{Neutral Element}

Associativity on its own is not enough. For example, |(++)| is associative, but:
< > foldl (++) [0] [[1,2],[],[3],[4]]
< [0,1,2,3,4]
< > foldr (++) [0] [[1,2],[],[3],[4]]
< [1,2,3,4,0]

Hence, also the initial value |n| of the accumulator matters. Indeed, it must be
the \emph{neutral element} of the operator, both on the left and the right.
\begin{equation*}
| n <> x == x == x <> n|
\end{equation*}

In abstract algebra, a semigroup with a neutral element is known as a \emph{monoid}.
Hence, Haskell features the |Monoid| subclass of |Semigroup|.

< class Semigroup a => Monoid a where
<   mempty  :: a

For example,

< instance Monoid [a] where
<   mempty = []

\paragraph{Challenges}
\begin{itemize}
\item
What are the neutral elements of other associative operations?
\item
Not all associative operators have a neutral element; can you think of some without
a neutral element?
\end{itemize}

%-------------------------------------------------------------------------------
\subsection{Beyond left vs. right}

As we have seen, by using a monoid we get the same behavior from
|foldr| and |foldl|. However, it goes further than that. Indeed, 
bracketing all the way to the left and all the way to the right
are just the two extremes. We can sprinkle neutral elements anywhere
over our expression and choose a corresponding bracketing.

A prominent strategy is \emph{divide-\&-conquer } where we recursively split
the list in two halves that we process independently and then combine their
results. When the processing of the parts happens in parallel, the time
complexity even goes from linear to logarithmic!\footnote{Provided the splitting does not take
linear time, which it does for Haskell lists.}

> foldDC :: Monoid m => [m] -> m
> foldDC []      = mempty
> foldDC [x]     = x
> foldDC l       = foldDC l1 <> foldDC l2
>   where (l1,l2) = splitAt (length l `div` 2) l  

The computation tree of |foldDC| is binary:

< foldDC [x1,x2,x3,x4,x5,x6,x7,x8]
< =
< ((x1 <> x2) <> (x3 <> x4)) <> ((x5 <> x6) <> (x7 <> x8))

\begin{center}
\begin{forest}
  [|<>|
   [|<>|,tier=two
    [|<>|,tier=three
     [|x1|,tier=four]
     [|x2|,tier=four]
    ]
    [|<>|,tier=three
     [|x3|,tier=four]
     [|x4|,tier=four]
    ]
   ] 
   [|<>|,tier=two
    [|<>|,tier=three
     [|x5|,tier=four]
     [|x6|,tier=four]
    ]
    [|<>|,tier=three
     [|x7|,tier=four]
     [|x8|,tier=four]
    ]
   ]
  ]
\end{forest}
\end{center}

Here the independent parts of the tree can be processed in parallel. 
In other words, if we can express an algorithm as a fold with a monoid,
it can be parallellized!

%===============================================================================
\section{Worked Example: Sortedness}

Consider the function |isSorted| that checks whether a list is sorted.

> isSorted :: Ord a => [a] -> Bool
> isSorted []     =  True
> isSorted [x]    =  True
> isSorted (x:y:xs) = x <= y && isSorted (y:xs)

We would like to obtain a parallellizable version of this function. This
basically means that we should come up with a monoid that underpins a
divide-\&-conquer approach to |isSorted|.

At first blush, we might take |Bool| as the type of the monoid, |&&| as its
associative operator with neutral element |True|. However, after a little thought,
it should be obvious that this won't do. Indeed, from knowing whether two 
adjacent sublists |xs| and |ys| are sorted, we can't always tell whether |xs ++ ys|
is sorted. For example, |[1,2]| and |[3,4]| are sorted and so is |[1,2] ++ [3,4]|, yet
|[3,4] ++ [1,2]| is not.

Hence, we need a more crafty type for the monoid, that allows us to compare two
adjacent sorted sublists. After exercising our little gray cells some more, we
realise that the minimal information we need for that are the first (i.e.,
smallest) and last (i.e., largest) element of the sorted sublist.

< data Sortedness a = NotSorted | Sorted a a
<
< instance Ord a => Semigroup (Sortedness a) where
<   NotSorted     <>  _            =  NotSorted
<   _             <>  NotSorted    =  NotSorted
<   Sorted l1 u1  <>  Sorted l2 u2
<                    | u1 <= l2    =  Sorted l1 u2
<                    | otherwise   =  NotSorted

While |Sortedness a| forms a semigroup, it lacks a neutral element. (Mind that
|NotSorted| is an \emph{absorbing} element, quite the opposite of a neutral
element.) So we add one for good measure, which also denotes an empty list,
which obviously has no (smallest and largest) elements.

> data Sortedness a = EmptySorted | NotSorted | Sorted a a
>
> instance Ord a => Semigroup (Sortedness a) where
>   EmptySorted   <>  s            =  s
>   s             <>  EmptySorted  =  s
>   NotSorted     <>  _            =  NotSorted
>   _             <>  NotSorted    =  NotSorted
>   Sorted l1 u1  <>  Sorted l2 u2
>                    | u1 <= l2    =  Sorted l1 u2
>                    | otherwise   =  NotSorted
>
> instance Ord a => Monoid (Sortedness a) where
>   mempty = EmptySorted

Finally, we can rewrite |isSorted| with a parallellizable fold.

> isSorted' :: Ord a => [a] -> Bool
> isSorted'= toBool . foldDC . map singleSorted

This approach consists of three steps: 
\begin{enumerate}
\item
We turn every element (every singleton list, if you will)
into a |Sortedness| value.

> singleSorted :: Sortedness a
> singleSorted x = Sorted x x

\item
We accumulate (also called \emph{crush}) all the monoid values
with |foldDC|, which can be done in parallel.
\item
We extra the boolean from the resulting monoid value.

> toBool :: Sortedness a -> Bool
> toBool EmptySorted   =  True
> toBool NotSorted     =  False
> toBool (Sorted _ _)  =  True

\end{enumerate}

That is all there is to it.


%===============================================================================
% \section{Foldable Datastructures}
% 
% type Foldable :: (* -> *) -> Constraint
% class Foldable t where
%   Data.Foldable.fold :: Monoid m => t m -> m
%   foldMap :: Monoid m => (a -> m) -> t a -> m
%   Data.Foldable.foldMap' :: Monoid m => (a -> m) -> t a -> m
%   foldr :: (a -> b -> b) -> b -> t a -> b
%   Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
%   foldl :: (b -> a -> b) -> b -> t a -> b
%   Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
%   foldr1 :: (a -> a -> a) -> t a -> a
%   foldl1 :: (a -> a -> a) -> t a -> a
%   Data.Foldable.toList :: t a -> [a]
%   null :: t a -> Bool
%   length :: t a -> Int
%   elem :: Eq a => a -> t a -> Bool
%   maximum :: Ord a => t a -> a
%   minimum :: Ord a => t a -> a
%   sum :: Num a => t a -> a
%   product :: Num a => t a -> a
%   {-# MINIMAL foldMap | foldr #-}
%   	-- Defined in ‘Data.Foldable’
