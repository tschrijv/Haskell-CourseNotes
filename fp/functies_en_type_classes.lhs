%include Formatting.fmt
%include lhs2TeX.fmt

%if False

> import Data.Char

> data Light = Red | Orange | Green

%endif

This chapter shows that functions are first-class citizens in Haskell.
We call them that because they are the equal of any other type of value in
the language. This means that they can appear anywhere where other
values do: as parameters to functions, as the result of a function, as value
stored in a variable, in a datastructure, \ldots

The most prominent of these first-class rights is the possibility to 
pass around functions as parameters.

%-------------------------------------------------------------------------------
\section{Function Parameters}

Here are the definitions of two functions |sum| and |prod| that compute
the sum and product of a list of integers respectively.
\sethscode{customhscode}

< sum :: [Int] -> Int
< sum []      = 0
< sum (x:xs)  = x + sum xs
< 
< prod :: [Int] -> Int
< prod []      = 1
< prod (x:xs)  = x * prod xs

What immediately jumps out is how similar the definitions of these two functions
are. They clearly have the same inductive pattern: there is a base
case (|0| or |1|) for the empty list, and for the non-empty list
the recursive result (|sum xs| or |prod xs|) is combined with the
first element |x| to obtain the overall result by means of a function 
(|+| or |*|).

We can capture this common pattern in a new function |foldr|:
\sethscode{customhscode}

< foldr f z []      = z
< foldr f z (x:xs)  = f x (foldr f z xs)

We have introduced two parameters to deal with the points
where |sum| and |prod| differ:
|z| for the base case and |f| to combine the recursive result
with the first element.

Now we can define |sum| and |prod| in terms of this common pattern.
\sethscode{customhscode}

> sum l   = foldr (+) 0 l
> prod l  = foldr (*) 1 l

These two definitions are very compact because the inductive logic
has been isolated in the |foldr| function. It is instrumental
to consider the type signature of this function:
\sethscode{customhscode}

< foldr :: (e -> a -> a) -> a -> [e] -> a

This signature reflects that |foldr| has three parameters, which are
in reverse order of appearance:
\begin{itemize}
\item the list of type |[e]|, where the type of the elements |e| is unconstrained;
\item the result |z| for the base case of any type |a|, which is also the result
      type of |foldr| as a whole; and
\item the function |f| with function type |e -> a -> a|, because it is applied
      to an element in the list (|e|) and a recursive result (|a|) to yield a new
      result (|a|).
\end{itemize}
Because of the first function parameter |f| we call |foldr| a \emph{higher-order function},
and a first-order function in particular.

The order of a function is defined (inductively, how else?) as follows:
\begin{itemize}
\item a \textbf{0th-order function} has only non-function parameters; and
\item an \textbf{(n+1)th-order function} has one or more function parameters,
      the highest order of which is $n$.
\end{itemize}
In practice we most often encounter 0th-order and 1st-order functions, while
2nd- and higher-order functions are rather rare. That is why the particular order
is usually not relevant and we typically only speak of higher-order functions when
we want to indicate that there is a function parameter.


\paragraph{The Behavior of |foldr|}

The function name |foldr| is short for \emph{fold right}. In other functional
languages this function is also called \emph{reduce}, because it reduces a list datastructure
to a single value (e.g., a sum). The recursion scheme that is captured in this function is not
restricted to lists, but can be easily generalized to other inductive datatypes.

We can diagrammatically clarify how this recursion scheme affects a list as follows:
\begin{Verbatim}[xleftmargin=1cm,framesep=1cm]
	    a  :  (b  :  (c  :  []))
	
	==> (foldr f z)
	
	    a `f` (b `f` (c `f` z ))
\end{Verbatim}
As we can see the impact of |foldr| is to replace the list constructors
|(:)| and |[]| with the custom values |f| and |z|. Using terminology 
from \emph{category theory} we call the combination of |f| and |z| an
\emph{algebra} for lists. By means of |foldr| the algebra assigns 
a (new) meaning to the list, which on its own is a meaningless or uninterpreted
datastructure.

Let us apply this principle, to replace the constructors by parameters, to the previously
presented type of trees:
\sethscode{customhscode}

> data Tree a = Empty | Branch a (Tree a) (Tree a)
> 
> foldTree :: (e -> a -> a -> a) -> a -> Tree e -> a
> foldTree f z Empty           = z
> foldTree f z (Branch x l r)  = f x  (foldTree f z l) 
>                                     (foldTree f z r)

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Function Definitions in Terms of |foldr|} 

Here we provide a recipe for defining functions in terms of a call to |foldr|,
\begin{spec}
h l = foldr f z l
\end{spec}
where the parameters |f| and |z| have to be instantiated with the appropriate expressions.

This recipe is known as \emph{the universal property of |foldr|}. Formally:
\begin{equation*}
\boxed{
|h l = foldr f z l| 
\qquad \Leftrightarrow \qquad
\left\{
\begin{array}{rcl}
|h []|     &  |=| & |z| \\
|h (x:xs)| & |=| & |f x (h xs)| 
\end{array}
\right.
}
\end{equation*}
If we know what |h| is, we can attempt to solve the system of two equations on
the right for |z| and |f|.

To make the recipe concrete, we illustrate it on the function 
|h == subsequences|.
The function
|subsequences :: [a] -> [[a]]| 
returns all ordered sublists of the given list. The order in which the
sublists are returned is not important.
Here is an example:
\begin{spec}
> subsequences [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
\end{spec}

We anticipate the following shape for the function definition:
\sethscode{customhscode}
\begin{spec}
subsequences :: [a] -> [[a]]
subsequences l = foldr f z l
\end{spec}

We determine the two parameters |f| and |z| thus:
\begin{itemize}
\item
The |z| parameter is the easiest to determine. It is obtained from the first of
the two equations, with |h == subsequences|:
\begin{equation*}
|subsequences [] = z|
\end{equation*}
In other words, |z| is the expected result for the empty list.
Clearly an empty list has only one sublist: the empty list itself. Hence we expect:
\begin{spec}
> subsequences []
[[]]
\end{spec}
and we partially instantiate the definition accordingly:
\sethscode{customhscode}
\begin{spec}
subsequences :: [a] -> [[a]]
subsequences l = foldr f [[]] l
\end{spec}

\item
To determine the parameter |f|, we solve the second equation.
\begin{equation}\label{eq:1}
|subsequences (x:xs) = f x (subsequences xs)|
\end{equation}
This equation is concerned with how the function behaves itself
with respect to a list of the form |x:xs|. In particular we have to characterize
the behavior in terms of |x| and the result |r| that we obtain from recursively
applying the function to the tail |xs| (i.e., |r == subsequences xs|). The way
|x| and |r| are combined determines the function |f|.

In our example we have to figure out how we obtain the subsequences of the list |x:xs|
given |x| and |subsequences xs|. This follows from the following two insights:
  \begin{itemize}
  \item 
  Every subsequence of |xs| is also a subsequence of |x:xs|.
  \item
  If we stick |x| in front of a subsequence of |xs|, we obtain a subsequence of |x:xs|.
  \end{itemize}
Hence, we can say that:
\begin{equation*}\label{eq:2}
| subsequences (x:xs) = [ x : ys || ys <- subsequences xs] ++ subsequences xs |
\end{equation*}

If we fill in this insight in Equation~\ref{eq:1}, we get:
\begin{equation*}\label{eq:3}
| [ x : ys || ys <- subsequences xs] ++ subsequences xs = f x (subsequences xs) |
\end{equation*}
By abbreviating, the recursive result |subsequences xs| to |r|, we get:
\begin{equation*}\label{eq:4}
|[ x : ys || ys <- r] ++ r = f x r|
\end{equation*}

If we turn this equation around, we get a valid definition for |f|:
\sethscode{customhscode}
\begin{spec}
f :: a -> [[a]] -> [[a]]
f x r  =  [x : ys | ys <- r] ++ r
\end{spec}
\end{itemize}

Lastly, we would like to point out that not every function can be written
as a call to |foldr|.

%-------------------------------------------------------------------------------
\subsection{Map} A second frequent recursion scheme is that of |map|:
\sethscode{customhscode}

< map :: (a -> b) -> [a] -> [b]
< map f []      = []
< map f (x:xs)  = f x : map f xs

This higher-order function transforms every element in a list by means
of the given function |f|.
For instance, |capitalize| replaces every letter in a |String|
by the corresponding capital using the predefined function
|toUpper :: Char -> Char|.
\sethscode{customhscode}

> capitalize :: String -> String
> capitalize s  = map toUpper s 

\subsection{Filter} A third useful higher-order function is |filter|:
\sethscode{customhscode}

< filter :: (a -> Bool) -> [a] -> [a]
< filter p []      = []
< filter p (x:xs)
<   | p x          = x : filter p xs
<   | otherwise    = filter p xs  

This higher-order function removes all elements from a given list 
that do not satisfy the given \emph{predicate} |p|.

%-------------------------------------------------------------------------------
\section{Support for Higher-Order Functions}

Because Haskell wants to encourage the use of higher-order functions,
it provides special support that facilitates their use. This support allows
to quickly write new functions (typically function parameters) in terms of
existing functions, and is based on ideas from the $\lambda$-calculus.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Local Definitions}

Suppose that we want to square all elements in a list. We immediately identify
the |map| recursion scheme as appropriate for this task. The function to apply
to every element is the squaring function. We write the auxiliary
function |square| to capture this.
\sethscode{customhscode}

> squarelist :: [Int] -> [Int]
> squarelist l  = map square l
> 
> square :: Int -> Int
> square x  = x * x

Now |square| is a toplevel definition that can also be used elsewhere.  Often
this is not intended. When writing small auxiliary definitions---e.g., to serve as a parameter of a higher-order function---we only intend
them to be used locally and do not wish to create any additional dependencies
further away that, e.g., would make it more onerous to refactor the program.
For this purpose Haskell provides local definitions that have a limited
scope. In fact, Haskell provides two different syntactic forms: 
|where| blocks and |let ... in ...| expressions. This is what the program
looks like with both styles of local definitions:

< squarelist :: [Int] -> [Int]
< squarelist l  = map square l
<   where
<     square x  = x * x

and 

< squarelist :: [Int] -> [Int]
< squarelist l  = 
<   let  square x  = x * x
<   in   map square l

There is no recommended form; both are equally `good'. There are some small
differences in their use. The |where| block is local to another definition,
in this case that of |squarelist|, is visible everywhere in that definition.
The |let ... in ...| expression can be used anywhere an expression can be
written. The definitions it creates are visible everywhere in the |let ... in
...|.

Both support multiple definitions, simply by writing them on subsequent lines.For example, 

< squarelist :: [Int] -> [Int]
< squarelist l  = result
<   where
<     square x  = x * x
<     result    = map square l

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Anonymous Functions}

The function |square| is actually rather trivial, and it makes little sense to
define and name a special-purpose function for the job. For this reason and
inspired by the $\lambda$-calculus, Haskell allows to compactly write anonymous
functions for specifying function parameters.
\sethscode{customhscode}

< squarelist :: [Int] -> [Int]
< squarelist l  = map (\x -> x * x) l

The notation |\x -> x * x| approximates the symbolic $\lambda$-calculus
notation $\lambda x.x*x$. The parameters of the anonymous function are listed
between the backslash and the arrow, while the body of the function is given
after the arrow.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Currying and Partial Application}

As first-class citizens functions can be returned from other functions just like any other type of
value. Here is an example.
\sethscode{customhscode}

> add :: Int -> (Int -> Int)
> add x = \y -> y + x

The function |add| takes a number |x| and returns an (anonymous) function
of type |Int -> Int| that adds |x| to the function parameter |y|.

This way we obtain for instance the increment function:
\sethscode{customhscode}

< inc :: Int -> Int
< inc = add 1

\sethscode{interactivehscode}
 
< ?> inc 5
< 6

Compare the above higher-order definition of |add| to the following
similar function |add'|:
\sethscode{customhscode}

> add' :: Int -> Int -> Int
> add' x y = y + x

The function |add'| is seemingly a $0$-order function with two parameters that is fundamentally different from |add|.
Yet in fact there is no difference between both functions in Haskell! We call this concept \textit{currying}: every function with
$n$ parameters is actually simulated by an $n-1$th-order function that takes the first parameter and returns an $n-2$th-order function
that processes the remaining parameters.

Haskell conveniently allows us to write $n$-parameter functions in $0$th-order style. Yet it is very useful to be aware 
of the actual underlying higher-order representation as we can exploit it to compactly derive \emph{new} functions
from existing ones.

We can for instance define |inc| in terms of |add'|:
\sethscode{customhscode}

< inc = add' 1

This technique of applying a function to fewer than all of its parameters is 
called \emph{partial application}.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Operator Sections} 

The partial application of operators is possible too.
We can for instance define |inc| directly in terms of the operator |+|.
\sethscode{customhscode}

< inc = (+) 1

A function that checks whether a number is strictly positive cannot 
be defined easily in terms of partial application of |>|, because
we want to supply the second argument but not the first one.
Fortunately, there is special syntax for just that, called \emph{operator section}.
\sethscode{customhscode}

> positive :: Int -> Bool
> positive = (> 0)

This is a right operator section. A left operator section is possible too: 
|(0 >)|.

Here are a few examples:
\sethscode{interactivehscode}

< ?> map (>0) [-1,0,1]
< [False,False,True]
<
< ?> map (0>) [-1,0,1]
< [True,False,False]
<
< ?> map (*2) [1..5]
< [2,4,6,8,10]

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Eta-reduction} 

A final convenient feature is $\eta$-reduction, which Haskell has copied
from the $\lambda$-calculus.
With $\eta$-reduction we can rewrite this:
\sethscode{interactivehscode}

< ?> map (\x -> signum x) [-10,0,20]
< [-1,0,1]

into the more compact form:
\sethscode{interactivehscode}

< ?> map signum [-10,0,20]
< [-1,0,1]

because |\x -> signum x| and |signum| are equivalent.

We can also apply $\eta$-reduction to conventional function definitions. Instead of:
\sethscode{customhscode}

< sum l = foldr (+) 0 l

we can simply write:
\sethscode{customhscode}

< sum = foldr (+) 0

%-------------------------------------------------------------------------------
\section{Stylish Higher-Order Code}

A very compact, but very readable\footnote{after some familiarisation}
programming style aims to write function definitions without mentioning function
parameters explicitly. This style is called \emph{point-free} because 
it does not mention the so-called \emph{points} or values.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Function Composition}

Curiously the point-free style makes heavy use of ``points'':
\sethscode{customhscode}

< (.) :: (b -> c) -> (a -> b) -> (a -> c)
< f . g = \x -> f (g x)

This dot-operator implements function composition\footnote{well-known in
mathematics where it is written as $\circ$} in Haskell.

We illustrate the point-free style with an example: counting the number of words in a text. 
First we write the traditional \emph{point-wise} solution:
\sethscode{customhscode}

> wc :: String -> Int
> wc str = length (words str)

where the function |words :: String -> [String]| splits a string
into a list of words and the function |length :: [a] -> Int| yields the length of a list.

In point-free style we obtain a more compact definition by not mentioning the parameter |str|:
\sethscode{customhscode}

> wc' :: String -> Int
> wc' = length . words

We say that the function |wc'| is defined as the composition of |length| and |words|.
Obviously the order of composition matters: the function |length|
is applied to the result of |words|.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Pipelines}

Of course more than two functions can be composed in this way. The result
is called a \emph{pipeline}. The data flows from right to left through
such a pipeline. Here is small example:
\sethscode{customhscode}

> squareSumEvens :: [Int] -> Int
> squareSumEvens = sum . map (\x -> x * x) . filter even

It may take some getting used to this style, but once you have mastered
it, you will appreciate how easy it is to read this kind of definition.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Point-wise Pipelines}

The point-wise counterpart of function composition |(.)| is
function application. In Haskell function application is denoted with a space,
like the space in |f x|. If you want to apply multiple functions in succession,
you have to use parentheses to group the function calls as in |f (g x)| because
|f g x| is equivalent to |(f g) x| which has an entirely different meaning. 

However, Haskell offers also an explicit operator |($)| for function application:
\sethscode{customhscode}

> ($) :: (a -> b) -> a -> b
> f $ x  = f x

This operator is very convenient because it has a very low priority and
associates to the right. This means that |f $ g x| denotes |f (g x)|, but
uses 1 fewer character to do so.

Hence, we can write |squareSumEvens| in an analogous point-wise style:
\sethscode{customhscode}

< squareSumEvens :: [Int] -> Int
< squareSumEvens l = 
<   sum $ map (\x -> x * x) $ filter even l

As you can see, it is easy to switch between point-wise and point-free
style with |($)| and |(.)|.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\subsection{Dataflow Inversion}

Finally, we mention a variant of |($)| that is popular in the functional language F\#:
\sethscode{customhscode}

> (|>) :: a -> (a -> b) -> b
> x |> f  = f x 

This operator associates to the left with low priority and swaps the parameters
of |($)|. This yields a more conventional left-to-right dataflow, similar to 
bash pipelines.
\sethscode{customhscode}

< squareSumEvens :: [Int] -> Int
< squareSumEvens l = 
<   l |> filter even |> map (\x -> x * x) |> sum

In the same way you can define an operator for function composition where the
data flows from left to right. In the area of category theory this operator is
sometimes denoted as |;| such that $|f ; g| \equiv |g . f|$. Unfortunately, |;|
is a reserved symbol in Haskell that cannot be used as an operator.  Hence, you
need to come up with a different operator name for the job.

%=============================================================================
\chapter{Type Classes}

Type classes are one of the most remarkable and famous features of Haskell.

%-------------------------------------------------------------------------------
\section{Adhoc Overloading}

Many programming languages overload operators like |+| and |*|.
For example in Java |+| can be used for adding both integers and
floating-point numbers. We say that the operator |+| is \emph{overloaded}
because it provides different implementations for different types of parameters.
We say that the overloading is \emph{adhoc} because the behavior of the operator
is not uniformly defined for every type, as compared to parametric polymorphism, 
but is individually defined for every type. 

Overloaded operators are the cause of much envy in programmers if the language
does not permit them to use |+| for the addition of user-defined
datatypes (e.g., |Matrix|). Then the developers of the language are privileged,
and they have decided once and for all which operators are overloaded and which
types they support. Even though it would be convenient to use |+| for custom types,
most languages simply do not permit this.

There are however languages that do support user-defined overloading, and C++ is
perhaps the best-known example. Yet, the way Haskell supports user-defined adhoc overloading
is particularly remarkable because of its elegance.

The following examples illustrate the overloading of |==| in Haskell:
\sethscode{interactivehscode}

< ?> 'a' == 'b'
< False
< 
< ?> 1.0 == 1.0
< True
< 
< ?> True == True
< True

We can apply |==| to parameters of different types:
|Char|, |Double|, |Bool|, \ldots but not all types.

%-------------------------------------------------------------------------------
\section{Type Classes}

To indicate that |==| is an overloadable function, it is declared 
in a type class.
\sethscode{customhscode}

< class Eq a where
<   (==) :: a -> a -> Bool

This declaration defines the type class with name |Eq|. The type class has one
type parameter |a| and one function |(==)|. Type class functions are called
\emph{methods}. Only the type signature of the method |(==)| is given.
Every concrete type |T| can supply a different implementation of the method |(==)|;
the only requirement is that this implementation has type |T -> T -> Bool|, which
we obtain by substituting the type |a| with |T| in the original signature.

\subsection{Instances}

To indicate that a particular type implements the methods of a type class, we
provide an \emph{instance} of that type class for that type. Here we see the
instance of |Eq| for the type |Bool|:
\sethscode{customhscode}

< instance Eq Bool where
<   True  == True   = True
<   False == False  = True
<   _     == _      = False

The type class |Eq| is already predefined in Haskell, and instances for most
predefined types are provided as well. However, when defining a new ADT, 
a corresponding |Eq| instance does not come into existence automatically.
Consider the |Light| ADT; if we want to compare values of this type, GHCi
points out that there is no applicable implementation of |(==)|:
\sethscode{interactivehscode}

< ?> Green == Green
< 
< <interactive>:1:7:
<     No instance for (Eq Light)
<       arising from a use of == 
<     Possible fix: add an instance declaration for (Eq Light)
<     In the expression: Green == Green
<     In an equation for `it': it = Green == Green 
 
At the same time we get advice for how to solve the problem: simply 
declare an |Eq Light| instance. Let us do so:
\sethscode{customhscode}

> instance Eq Light where
>   Red     == Red     = True
>   Green   == Green   = True
>   Orange  == Orange  = True
>   _       == _       = False
 
Observe that we have a lot of freedom in how we want to define
this instance. The above example implements structural equality. However, this is
not required; we can opt for a more semantic notion of equality instead.
For example, we can define evaluation for a datatype of expressions:
\sethscode{customhscode}

> data Exp = Lit Int | Add Exp Exp
> 
> eval :: Exp -> Int
> eval (Lit n)      = n
> eval (Add e1 e2)  = eval e1 + eval e2 
  
Now we can state that two expressions are equal if they evaluate to the same result:
\sethscode{customhscode}

> instance Eq Exp where
>   e1 == e2  = eval e1 == eval e2

Observe that in the above declaration the first occurrence of |==|
denotes equality of expressions, while the second occurrence denotes
equality of integers.

\subsection{Laws}
Do we have complete freedom in how we implement a particular type class
instance? No, not every well-typed definition of |==| makes sense as a notion
of equality. We expect to have only implementations that actually capture
equivalence relations.
The fact that |==| should be an equivalence relation, can be
captured in three \emph{laws}:
\begin{IEEEeqnarray*}{rCl"s}
| x == x | & \equiv  & | True |                               & \textsc{(Reflexivity)} \\
| x == y | & \equiv & | y == x |   & \textsc{(Symmetry)} \\
| x == y && y == z | & \equiv & | x == z && y == z |  & \textsc{(Transitivity)}
\end{IEEEeqnarray*}
This is the minimal sensible specification of |(==)|.
Unfortunately, these laws cannot be expressed in the Haskell language itself,
nor can they be enforced by the Haskell compiler. Just like in other languages we have to include this supplementary specification in comments and code documentation. Nevertheless, these laws are deemed highly important and it is expected that the implementer of an instance verifies that his implementation satisfies them.

\subsection{Multiple Methods}

A type class is not limited to a single method. Typically multiple
related methods are bundled in the same type class. The |Eq| class
is an example of this practice. It not only contains the equality test
|(==)| we have shown above, but also the inequality test |(/=)|.
\sethscode{customhscode}

< class Eq a where
<   (==) :: a -> a -> Bool
<   (/=) :: a -> a -> Bool

This method comes of course with an additional law, which expresses
the connection between the two methods:
\begin{IEEEeqnarray*}{rCl"s}
| x == y | & \equiv  & | not (x /= y)|                               & \textsc{(Consistency)} \\
\end{IEEEeqnarray*}

\subsection{Default Implementations}

To lighten the burden of implementing type class instances, Haskell makes
it possible to provide so-called \emph{default implementations} of methods
in the type class declaration. This is mainly useful if the type class
contains multiple methods that can be defined in terms of each other. For example, the full declaration of the |Eq| type class makes use of default
implementations to define |(==)| and |(/=)| in terms of each other
following the \textsc{(Consistency)} law.
\sethscode{customhscode}

< class Eq a where
<   (==) :: a -> a -> Bool
<   x == y  = not (x /= y)
<   (/=) :: a -> a -> Bool
<   x /= y  = not (x == y)

When a new instance is defined, it need not implement both methods.
It is sufficient to implement only one of the two; the other comes
for free thanks to the default implementation. Of course it is still
possible to implement both methods; this is useful when there exists
a more efficient implementation for the type at hand than the default
implementation.
If we forget to implement any method, the instance assumes both default
implementations. This is of course problematic because their mutual
definition leads to an infinite loop at runtime.

To mitigate this problem it is common to state in the documentation
of the type class what the minimal sets of methods are that need to be
implemented. For |Eq| there are two such (singleton) sets: $\{|(==)|\}$
and $\{|(/=)|\}$. Implementing more methods is allowed, but not less.

\subsection{Constrained Polymorphism}

Type class methods are of course to be used within other function definitions.
An example is the function |nub|, which removes duplicates from a list.

> nub []     = []
> nub (x:xs) = x : nub (filter (/= x) xs)

What is the most general type signature of this function?
In particular, for what types of list elements does this function make sense?
If we consider the implementation of |nub|, then the only restriction
on the type |a| of elements is that it should provide an implementation
of the inequality test |(/=)|. Multiple types do so (e.g., |Int| and |Bool|),
but certainly not all types (e.g., |Int -> Int|).

Hence, the restriction on the type |a| is precisely expressed by
saying that ``there has to be an instance of type class |Eq| for |a|''.
In Haskell we express this restriction with the notation |Eq a|.\footnote{
Here the name of the type class, such as |Eq|, plays the role of a \emph{logical predicate} that expresses: this type has an instance of this type class.}
We add this restriction as follows to the type signature:
\sethscode{customhscode}

> nub :: Eq a => [a] -> [a]

We call the function type a \emph{qualified type} because it is subject
to a qualification (i.e., an additional condition). The term \emph{constrained
polymorphism} is also used to express that the type is not purely polymorphic, but polymorphism with additional constraints on the type parameters. We call |Eq a| in this role a \emph{type constraint}.

\subsection{Propagation of Type Constraints}
We just saw that |nub| has a type constraint in its signature
because it uses the type class method |(/=)|. In turn |nub|
also ``infects'' other functions with its type constraint.
The function |nbDistinct| is defined in terms of |nub|:
\sethscode{customhscode}

> nbDistinct :: Eq a => [a] -> Int
> nbDistinct l = length (nub l)

Because |nbDistinct| calls |nub| on a list |l| of type |[a]|,
the type constraint |Eq a| is also imposed in the signature of |nbDistinct|.

Hence type constraints are contagious, and the type class methods
are the ground zero of the contagion; their stand-alone signatures already 
feature the type constraint. Consider:
\sethscode{customhscode}

< (==) :: Eq a => a -> a -> Bool

The only way to prevent further propagation of a constraint is to fill in a concrete monomorphic type for which a type class instance exists:
\sethscode{customhscode}

> nbDistinctInts :: [Int] -> Int
> nbDistinctInts l = nbDistinct l

Here we call |nbDistinct| on a list of type |[Int]|.
Because there is an |Eq Int| instance, the constraint does not
appear in |nbDistinctInts|.

\subsection{Polymorphic Instances}
We can write instances for polymorphic data types like |[a]| too.
For example, the |Eq| instance for this type is already predefined as:
\sethscode{customhscode}

< instance Eq a => Eq [a] where
<   []     == []      = True
<   (x:xs) == (y:ys)  = x == y && xs == ys
<   _      == _       = False

Note that the instance itself is subject to the type constraint |Eq a|, which appears in the head of the instance.
After all, to compare lists we have to be able to compare their elements.

By combining the instances |Eq Int| and |Eq Light| with this instance
for lists, we can perform the following checks.
\sethscode{interactivehscode}

< ?> [1,2,3] == [1,2,3]
< True
< 
< ?> [Green,Orange,Red] == [Green,Orange]
< False
< 
< ?> [[Green],[Orange],[Red]] == [[Green],[Orange],[Red]]
< True

Another example of a predefined polymorphic instance is that for tuples:
\sethscode{customhscode}

< instance (Eq a, Eq b) => Eq (a,b) where
<   (x1,y1) == (x2,y2)  = x1 == x2 && y1 == y2

Hence two tuples are equal if their components are.
Because this requires the equality tests for the two component types, two
type constraints arise: |Eq a| for the first component, and |Eq b| for the second.  
\sethscode{interactivehscode}

< ?> (1,Red) == (1,Red)
< True
< 
< ?> (Green,2) == (Green,3)
< False


\subsection{Show}
An important and well-known type class is |Show|:
\sethscode{customhscode}

< class Show a where
<   show :: a -> String
<   .. -- additional methods

This class captures the conversion of values of type |a| to a textual |String|
representation. (The omitted methods are for advanced uses and not discussed
here. We can safely ignore them because they have a default implementation in
terms of |show|.)

All predefined types, with the exception of functions, have a |Show| instance.
In fact, we have already made use of these instances without knowing.
Indeed, the interactive GHCi environment uses |show| behind the scenes to obtain
textual representations of results that it can print on the screen.

In this example
\sethscode{interactivehscode}

< ?> True && False
< False

the expression |True && False| is evaluated to the value |False|. 
GHCi obtains the text |"False"| that appears on the screen 
by evaluating |show (True && False)|.
Yet, if we evaluate a trivial value of a new type, GHCi does not
know how to display it:
\sethscode{interactivehscode}

< ?> Red
< 
< <interactive>:1:1:
<     No instance for (Show Light)
<       arising from a use of Red
<     Possible fix: add an instance declaration for (Show Light)
<     In a stmt of an interactive GHCi command: print it


In order to display the value, we have to write a |Show| instance first:

> instance Show Light where
>   show Green   = "green light"
>   show Orange  = "orange light"
>   show Red     = "red light"

Now GHCi can display the result:
\sethscode{interactivehscode}

< ?> Red
< red light

\subsection{Subclasses}

New type classes can extend existing ones. This
is illustrated by the type class |Ord a|, which states that elements of type 
|a| are totally ordered. The class extends |Eq a|. After all, equality
is obviously defined for ordered elements. Hence, the |Ord| class
is defined as a \emph{subclass} of |Eq|.
\sethscode{customhscode}

< class Eq a => Ord a where
<   compare  :: a -> a -> Ordering
<   (<)      :: a -> a -> Bool
<   (>=)     :: a -> a -> Bool
<   (>)      :: a -> a -> Bool
<   (<=)     :: a -> a -> Bool
<   max      :: a -> a -> a
<   min      :: a -> a -> a

with
\sethscode{customhscode}

< data Ordering = LT | EQ | GT

where |LT| is short for ``less-than'', |EQ| for ``equals'',
and |GT| for ``greater than''.

The class contains many methods, but luckily only one
has to be implemented explicitly: either |compare| or |(<=)|. The
others come for free thanks to default implementations.

Because |Ord| is a subclass of |Eq|, it is only possible to 
create a new |Ord| instance for a type, if that type already has an
|Eq| instance. That is the case for |Light|, and so we can write:
\sethscode{customhscode}

> instance Ord Light where
>   compare Green   Green   = EQ
>   compare Red     Red     = EQ  
>   compare Orange  Orange  = EQ
>   compare Green   _       = LT
>   compare _       Green   = GT
>   compare Red     _       = GT
>   compare _       Red     = LT

We do not elaborate the laws for |Ord|, but simply summarize them
by saying that |Ord| should provide a total order.

\subsection{Automatically Derived Instances}

For a limited number of predefined type classes Haskell knows how to automatically
derive new instances following a generic recipe. This is useful when the programmer does not want to waste time writing the instance and the generic recipe of Haskell is exactly what is required. 

You can make use of this feature by ending a |data| declaration with |deriving (...)|, where |...| is a list of type class names whose instances should be generated.

For example, we can write for |Light|:
\sethscode{customhscode}

< data Light = Green | Orange | Red deriving (Eq, Ord, Show)

Observe that automatic derivation of instances only applies to a limited number of predefined type classes, and certainly not for user-defined type classes.\footnote{although there is an advanced feature to teach the compiler how to do so}
