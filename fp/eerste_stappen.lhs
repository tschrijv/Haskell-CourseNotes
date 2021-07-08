%include Formatting.fmt
%include lhs2TeX.fmt

This chapter covers the basic features of Haskell and enables you to write
simple programs.

%===============================================================================
\section{Functions}

The term \emph{functional programming} says it all: functions are the core
unit of code in Haskell. 

%-------------------------------------------------------------------------------
\subsection{My First Function}

Let us look right away at a first example of a Haskell function definition:
\sethscode{customhscode}

> f x = x + 1

This notation is very similar to the mathematical notation for the same function 
definition:
\[ f(x) = x + 1 \]
In both cases we speak of a function (named) |f| with a parameter |x|.
The only difference between the Haskell notation and the mathematical notation
is that Haskell does not require parentheses around the parameter |x|.

Let us also briefly emphasize the role of the |=| symbol. Just like in
mathematics this symbol expresses that |f x| is equivalent to |x + 1|.
Importantly, this equality goes both ways. Of course the evaluation uses the
equality from left to right. However, the equation also works the other way around:
we can replace |x + 1| anywhere in a program by |f x| without changing
the program's meaning.

Now we illustrate how to call this function in GHCi, the interactive environment of GHC.
\sethscode{interactivehscode}

< ?> f 0
< 1

Just like in the function definition we separate the function name 
from the (now actual) parameter with a space. GHCi prints the result
of the function call, in this case |1|, on the next line. Based on the
function definition it should be intuitively clear how GHCi obtains this
result. We return later to the technical details of this process.

Here is another call:
\sethscode{interactivehscode}

< ?> f 1
< 2

We can obtain the same result by applying |f| twice in succession to the value |0|. This
gives us a slightly larger expression:
\sethscode{interactivehscode}

< ?> f (f 0)
< 2

As you can see, we do use parentheses here.  Yet, they are not used to separate
the function name from its parameters. Instead they are used to group a subexpression.
If we had written |f f 0|, then Haskell would not have understood that |0| is a parameter
of the second |f| and |f 0| a parameter of the first |f|. The parentheses group the unit |f 0|
to disambiguate the expression.

Parentheses in Haskell have exactly the same grouping role as parentheses have
in arithmetic expressions like $(1 + 2) \times 3$.

%-------------------------------------------------------------------------------
\subsection{Multiple Parameters}

A function with multiple parameters is defined in much the same way as a function
with a single parameter:
\sethscode{customhscode}

< g x y = x + 2 * y 

We separate the parameters from one another with a space. The same happens in
function calls:
\sethscode{interactivehscode}

< ?> g 1 2
< 5
< 
< ?> f (g 1 2)
< 6
< 
< ?> g 1 (f 2)
< 7

%-------------------------------------------------------------------------------
\subsection{Naming}

There are two kinds of function names in Haskell: 1) conventional alphanumeric names,
and 2) operator names.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Conventional Alphanumeric Names}

Conventional alphanumeric names start with a lower case letter
(|a|-|z|) or an
underscore (|_|). In the latter case more characters have to follow; in the former case this is optional.
Subsequent characters may be lower case letters (|a|-|z|),
upper case letters (|A|-|Z|), 
digits (|0|-|9|), 
underscore (|_|) and
apostrophe ('). 
%TODO: type-en constructornamen beginnen met een hoofdletter

These are examples of valid names:
\sethscode{otherhscode}
\begin{spec}
f
f'
f''
_f
f'_'f
fUNny
\end{spec}

It is good style to use camel case for function names and to limit the
use of digits, underscores and apostrophes.
\sethscode{otherhscode}
\begin{spec}
concatMap
zipWith
\end{spec}
Digits and apostrophes are sometimes used as suffixes of existing function
names to identify related functions. However, this practice is only used for
illustration purposes and is not suitable for production code.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Operator Names}

In contrast to many other programming languages, Haskell allows the programmer
to define custom operators. An operator name consists of one or more of the following
symbols:
\sethscode{otherhscode}
\begin{spec}
!#$%&*+./<=>?@\^|-~:
\end{spec}
Many operators already have a predefined meaning, like:
\sethscode{otherhscode}
\begin{spec}
 $       +       ++     >>=
 .       -       &&     <*>
 <       *       ||     <$>
 >       /       >>
\end{spec}
but there are still many unused combinations.

%TODO: operatornamen die beginnen met : zijn constructoren

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Function Application Syntax}

The notation for the application of a function to a number of parameters depends on the
type of function name. In case of a conventional function name, the function is placed 
in front of its parameters. This is known as \emph{prefix} notation.
\sethscode{customhscode}
\begin{spec}
g x y  = f x y
\end{spec}

If the function name is an operator, the operator is placed between the two parameters. This is
known as \emph{infix} notation.
\sethscode{customhscode}
\begin{spec}
x @@ y  = x + y
\end{spec}

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Conversion}

It is possible to use a conventional function name as if it were an operator and vice versa.
We achieve the former by placing the function name between 
\emph{backquotes} (a.k.a. \emph{backticks}). This allows us to use conventional functions in infix position.
\sethscode{customhscode}
\begin{spec}
x `g` y  = x `f` y
\end{spec}

Conversely parentheses turn an operator into a conventional funtion name that can be used in prefix position.
\sethscode{customhscode}
\begin{spec}
(@@) x y  = (+) x y
\end{spec}

%===============================================================================
\section{Primitive Data}

The $\lambda$-calculus establishes that, in theory, functions are sufficient to
express all computational tasks. That is of course not very practical. Hence, Haskell
offers also various other datatypes besides functions.

%-------------------------------------------------------------------------------
\subsection{\texttt{Int}: Integer Numbers}

The type |Int| is the name for integer numbers in Haskell. We have in fact
already used integer numbers in the above functions |f| and |g|. This fact has
perhaps gone unnoticed because the notation for |Int| values is entirely
standard:
|0|, |1|, |-22|, \ldots

For addition, subtraction and multiplication Haskell provides the usual operators that
can be used in infix position:
\sethscode{interactivehscode}

< ?> 5 + 3
< 8
< 
< ?> 10 - 3
< 7
< 
< ?> 7 * 9
< 63

Less conventional is the use of a regular function |div| for integer division:
\sethscode{interactivehscode}

< ?> div 8 2
< 4
< 
< ?> div 8 3
< 2

There are various other predefined |Int| functions. Here is one more:
\sethscode{interactivehscode}

< ?> mod 8 3
< 2
< 
< ?> (div 8 3) * 3 + (mod 8 3)
< 8


%-------------------------------------------------------------------------------
\subsection{\texttt{Double}: Floating-Point Numbers}

Floating-point numbers are also written in the usual way:
|0.0|, |3.14159|, |-99.99|, \ldots \ By default Haskell uses \emph{double} precision. Hence the type name |Double|. 
We will see later how to obtain single precision numbers of type |Float|.

Again Haskell provides operators 
|+|, |-|, |*| and now also
|/| for division.
\sethscode{interactivehscode}

< ?> 8.0 / 2.0
< 4.0
< 
< ?> 8.0 / 3.0
< 2.6666666666666665


%-------------------------------------------------------------------------------
\subsection{\texttt{Bool}: Boolean Values}

Finally, we mention the Boolean values of type |Bool|:
|True| and |False|.

The operators for conjunction and disjunction are the conventional ones:
\sethscode{interactivehscode}

< ?> True && False
< False
< 
< ?> True || False
< True

The function |not| implements negation:
\sethscode{interactivehscode}

< ?> not True
< False
< 
< ?> not False
< True


You can obtain Boolean values by comparing numbers:
\sethscode{interactivehscode}

< ?> 1 + 1 == 2
< True
< 
< ?> 1 /= 1 
< False
< 
< ?> 2 > 3
< False
< 
< ?> 3.1 < 4.7
< True


Of course you can also define your own functions that return Boolean values:
\sethscode{customhscode}

> passingGrade p = p > 9

and use them at will:
\sethscode{interactivehscode}

< ?> passingGrade 9
< False
< 
< ?> passingGrade 10
< True

%===============================================================================
\section{Conditional Expressions}

Conditional control structures are found in nearly all programming languages.

%-------------------------------------------------------------------------------
\subsection{If-then-else}

The 
\emph{Flafius} bank offers an interest rate that depends on the savings amount.
Their policy is captured in the following function:
\sethscode{customhscode}

> rate a = 
>   if a > 250000
>     then 1.00
>     else 1.40

If the savings amount is more than 
250,000 euro, you get a lower interest rate.
The function |rate| makes use of the built-in conditional expression
|if-then-else|.

You can nest multiple occurrences of this conditional expression form to distinguish
more than two cases. Consider for instance the Flafius \emph{Plus} account that distinguishes
three different cases:
\sethscode{customhscode}

> plusRate a = 
>   if a > 250000
>     then 1.00
>     else if a > 50000
>            then 1.40
>            else 1.90

%-------------------------------------------------------------------------------
\subsection{Guards}

The |if-then-else| notation quickly becomes unwieldy when there are more than two
cases. For this reason Haskell provides an interesting alternative known as \emph{guards}.
With guards we can write the function |plusRate| as follows:
\sethscode{customhscode}

%format plusRate2 = "\Varid{plusRate}"

> plusRate2 a
>   | a > 250000  = 1.00
>   | a >  50000  = 1.40
>   | otherwise   = 1.90

This formulation is more compact and readable. A guard is a Boolean expression
like |a > 250000| that ``guards'' a case of the function. If the test succeeds,
the corresponding result is returned. If the test fails, the function tries the
next guard. The final case is usually a default case guarded by |otherwise|. 
Note that |otherwise| is just a more readable synonym for |True|.

The guard notation has been inspired by the corresponding mathematical notation:
\[
\mathit{plusRate}(a) = 
\left\{\begin{array}{l@@{\hspace{1cm}}l}
1.00 &, 250000 < a            \\
1.40 &, 50000 < a < 250000   \\
1.90 &, \mathit{otherwise}
\end{array}\right. \]

%===============================================================================
\section{Lists and Recursion}

A list is an ordered collection of data that is used in a wide range
of applications. Lists in Haskell deserve special attention because they
are used more frequently, also in situations where other languages would not
use them.

%-------------------------------------------------------------------------------
\subsection{Lists}

A list containing the elements
|1|, |2| and |3| is denoted as
|[1,2,3]|. Lists with other types of elements are written in a similar fashion. 
For instance,
|[True,False,False,True]| is a list of
Boolean values and |[0.0, 0.25, 0.5, 0.75, 1.0]| contains doubles.

Three useful functions to inspect lists are 
|null|, |head| and
|tail|. The first of these checks whether a list is empty.
\sethscode{interactivehscode}

< ?> null [1,2,3]
< False
< 
< ?> null []
< True

The functions |head| and |tail| are only defined for non-empty lists.
In such cases |head| returns the first element and |tail| the list of remaining elements.
\sethscode{interactivehscode}

< ?> head [1,2,3]
< 1
< 
< ?> tail [1,2,3]
< [2,3]

Applying either function on an empty list results in a runtime exception.
\sethscode{interactivehscode}

< ?> head []
< *** Exception: Prelude.head: empty list
< 
< ?> tail []
< *** Exception: Prelude.tail: empty list


%-------------------------------------------------------------------------------
\subsection{Primitive Notation}

The notation 
|[1,2,3]| is actually \emph{syntactic sugar} meant to ``sweeten'' the use of lists. 
This syntactic sugar is internally translated by Haskell into a more general and primitive
notation. This general notation is based on an \emph{inductive} definition of lists.
\begin{itemize}
\item The base case is the empty list |[]|, and
\item the inductive case |x : xs| sticks the element |x| in front of the list |xs|.
\end{itemize}
We call |[]| and |:| the \emph{constructors} of lists; they allow us to build
lists.
\sethscode{interactivehscode}

< ?> 1 : []
< [1]
< 
< ?> 1 : (2 : [])
< [1,2]
< 
< ?> 1 : 2 : []
< [1,2]

As you can see GHCi prints lists we build with the primitive constructors 
using the syntactic sugar notation. 
Observe that we can omit parentheses because the right-most occurrence of |:| has 
the highest priority.

%-------------------------------------------------------------------------------
\subsection{Recursive Functions on Lists}

Now that we have seen how to build and inspect lists, nothing can stop us from
writing various functions on lists. For instance, we can write a simple function
that creates a singleton list from the given element thus:
\sethscode{customhscode}

> singleton x = [x]

However, functions like |singleton| that do all of their work ``at once'' in
the function body are, due to their simplicity, not terribly exciting.

Functions become more fanciful and interesting when they outsource part of
their work to another function. For instance, the function |two| creates
a list with two copies of the given element by delegating part of the work
to |singleton|.
\sethscode{customhscode}

> two x = x : singleton x

Yet, ultimately nothing very deep arises from delegation to simple functions.
After all, we can simply inline the auxiliary functions (i.e., expand the
function definitions) to get the same behavior.
\sethscode{customhscode}

> two' x = x : [x]

Hence, while this kind of delegation affords us valuable \emph{code reuse}, we
do not gain any \emph{expressive power}.

The situation changes dramatically when functions delegate part of their work
to themselves. This outrageously useful concept, whereby a function is defined
in terms of itself, is known as \emph{recursion}.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Recursively Producing Lists} 
As a first example of a recursion, we write the function |fromTo| that
builds the lists of numbers in the given interval.
\sethscode{interactivehscode}

< ?> fromTo 1 10
< [1,2,3,4,5,6,7,8,9,10]
< 
< ?> fromTo 5 8
< [5,6,7,8]
< 
< ?> fromTo 8 5
< []

We define this function compactly as follows:
\sethscode{customhscode}

> fromTo from to
>   | from > to  = []
>   | otherwise  = from : fromTo (from+1) to

We distinguish between two cases:
\begin{itemize}
\item in case the interval is empty we return the empty list, and
\item in case the interval is non-empty, we return the non-empty list
      that starts with the first element of the interval and follows with the remaining interval.
\end{itemize}
We call this function \emph{recursive} because, in the case of a non-empty
interval, it calls itself recursively to create the tail of the list. 

Observe that we could not have written the function |fromTo| without recursion.
Indeed, recursion greatly increases the expressive power and makes the Haskell
language \emph{Turing complete}.\footnote{If you don't know what this means, do
look it up.} This makes recursion a key concept which is used pervasively in
most Haskell programs and is vital to master: it is for functional programming
what loops are for imperative programming.\footnote{Arguably recursion is more
fundamental than loops, as it also arises frequently in mathematics and in art,
e.g., see Matryoshka Dolls and the Droste effect.}

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Termination}

Observe that not all of the work is left to the recursive call: the function
itself adds the first element of the interval. Clearly, if all of the work were
left to the recursive call, nothing would ever be achieved! Indeed, the most direct
way of writing an infinite loop in Haskell is by writing a function that does nothing
more than call itself recursively.
\sethscode{customhscode}

> loop x = loop x

In practice, programmers end up writing more convoluted forms of such
\emph{non-terminating} recursion by accident. Hence, it is essential to make
sure all uses of recursion are terminating. The key idea is to
ensure that 
\begin{enumerate}[(a)]
\item
the amount of work for the function is larger, in a sense to be identified by
the programmer, than that for the recursive call, and that 
\item
starting from any amount of work a base case is reached in a finite number of
steps; a base case is one that is handled directly without recursive calls.
\end{enumerate}

In the case of the |fromTo| function we can show termination by characterising
the amount of work with the size of the interval to be generated, which is a
natural number. 
We can verify that condition
(a) is satisfied as the size of the interval clearly decreases by 1 in the
recursive call. Condition (b) is also met because the interval becomes empty
after a finite number of steps\footnote{the number of steps is equal to the size of the interval} and an empty interval is generated straight away
without further recursion.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Recursively Consuming Lists}
Now we write a function that takes a list apart. This function |sum| 
sums the numbers in the list.
\sethscode{interactivehscode}

< ?> sum []
< 0
< 
< ?> sum [1,2,3,4,5]
< 15
< 
< ?> sum (fromTo 1 10)
< 55

Again we write a recursive function.

< sum list
<   | null list  = 0
<   | otherwise  = head list + sum (tail list)

This function uses guards to distinguish between the empty list, the base case,
and the non-empty list, the recursive case. The recursive call is applied to a
component of the original list, in this case its tail. This is known as
\emph{structural recursion}.

There are two prominent reasons why structural recursion is a very common
pattern for defining functions.  Firstly, it is very natural and easy to define
a function that is guided along by the shape of its input data structure.
Secondly, structural recursion is very well-behaved as it guarantees
termination. Indeed, the recursive call has obviously less work because a
component of the input is obviously smaller than the input itself, and
ultimately the input data structure is so small it has no further components.
 
%-------------------------------------------------------------------------------
\subsection{Pattern Matching}

Haskell simplifies the case distinction for lists by means of 
\emph{pattern matching}. Instead of using the |null| function for case analysis
we can use \emph{patterns}. A pattern is a particular shape of list that we
are interested in. In the case of |sum| we distinguish between the pattern |[]|
and the pattern |(x:xs)|. This results in the following definition:
\sethscode{customhscode}
%format som2 = "\Varid{sum}"

> som2 []      = 0
> som2 (x:xs)  = x + som2 xs

The role of a pattern is two-fold. Firstly, it selects a particular case, just like a guard.
Secondly, it defines a number of variables that can be used within the definition
of the case. In the example above the pattern |(x:xs)| defines the variables
|x| and |xs| that can be used in |x + som2 xs|.

Pattern matching has three important advantages:
\begin{enumerate}
\item 
The combination of the two roles means that we can write the function |sum|
\emph{more compactly}.
\item 
Pattern matching improves the \emph{readability} because the patterns are
written in the same way as the list values that are supplied as input to the function.
Hence we can immediately see which case is applicable to a particular input.
\item
Pattern matching promotes \emph{equational reasoning}: each of the cases
is an independent equation that we can exploit during reasoning about the equality
of two expressions.
\end{enumerate}

We can easily define the predefined functions
|null|, |head| and |tail| ourselves in terms of
pattern matching.
\sethscode{customhscode}

< null []      = True
< null (x:xs)  = False
< 
< head (x:xs)  = x
< 
< tail (x:xs)  = xs

In the case of |head| and |tail| there is no sensible result for the
empty list |[]|. Hence this case is not defined for both functions.
That is why we call them \emph{partial functions}, functions that are not
defined for their entire domain of inputs.

%===============================================================================
\section{Types}

So far we have said very little about \emph{types}, while Haskell is actually
quite renowned for its advanced type system.
Yet, in contrast to many other statically typed programming languages, the programmer
does not have to extensively annotate code with type information (parameter types, return types, 
type of local variables, \ldots). A particular part of a Haskell system, the \emph{typechecker}, 
is able to automatically derive the necessary type information in a process called
\emph{type inference}.\footnote{Type Inference was developed by Hindley and Milner
in the 70s.} Based on this derived type information the typechecker validates
whether all operations in the program make sense. If not, it signals type errors.

%-------------------------------------------------------------------------------
\subsection{Type Signatures}

Even though you do not have to supply type information, there nevertheless are compelling
reasons to do so for your user-defined functions in the form of \emph{type signatures}:
\begin{itemize}
\item They are useful documentation of the code. A signature indicates how the function
      is to be used (what parameters to supply  and what result to expect) and
      also gives an indication of what the function may or may not do.

      For example, the online search engine \emph{Hoogle}\footnote{\url{http://hoogle.haskell.org/}} 
      facilitates finding predefined Haskell functions based on a desired type.

\item The signature provides an (abstract) specification of the function being defined.
      If the specification does not agree with the definition, then the typechecker
      signals this discrepancy. In this way it is possible to detect bugs early in the
      development process.
\end{itemize}

Now consider how to write type signatures. We repeat our first function, now with an explicit
type signature:
\sethscode{customhscode}
%format f2 = "\Varid{f}"

> f2 :: Int -> Int
> f2 x = x + 1

The type signature is written on the line above the function definition. It consists
of the name of the function |f| followed by |::| and then the function type |Int -> Int|.
This type |Int -> Int| is the type of functions that take a value of type |Int|
and map it to a value of type |Int|.

Here are more of our functions, now with a type signature.
\sethscode{customhscode}
%format geslaagd2 = "\Varid{passingGrade}"
%format basisrente2 = "\Varid{rate}"

> g :: Int -> Int -> Int
> g x y = x + 2 * y 
> 
> geslaagd2 :: Int -> Bool
> geslaagd2 p = p > 9
> 
> basisrente2 :: Int -> Double
> basisrente2 b = 
>   if b > 250000
>     then 1.00
>     else 1.40

As you can see the types of the parameters and results can be different. Multiple parameters
are separated by arrows |->|.

%-------------------------------------------------------------------------------
\subsection{Algebraic Datatypes}

Haskell programmers are not limited to the predefined datataypes like |Int|
and |Bool|, but can also create their own new types.
This happens in the form of \emph{algebraic datatypes} (ADTs).\footnote{Do not confuse
these with abstract datatypes, also abbreviated by ADT.}

\paragraph{Alternatives}
A new Haskell ADT is defined by means of a |data|-declaration that enumerates
the alternative constructors. Here is an ADT for the colors of a traffic light:
\sethscode{customhscode}

> data Light = Red | Orange | Green

The name of the type is |Light|. It distinguishes three different values, 
|Red|, |Orange| and |Green|, based on the corresponding constructors.

The following function makes use of the |Light| ADT:
\sethscode{customhscode}

> next :: Light -> Light
> next Red     = Green
> next Orange  = Red
> next Green   = Orange

The function |next| illustrates that pattern matching is also available 
for user-defined datatypes.

We call |Light| a \emph{sum type}: the values of this type
are the ``sum'' (or union) of the values that can be constructed by 
the three different constructors.
\[ |Light| = \{ |Red| \} \cup \{ |Orange| \} \cup \{ |Green| \}\]
In languages like C++ and Java we call the above often
enum-types or enumerations of values.

Observe that the predefined type |Bool| is another example of such a sum type. You
can define it yourself as follows:
\sethscode{customhscode}

< data Bool = True | False


\paragraph{Combinations}
A quite different kind of ADT is the following |Person| ADT that represents
people with their name and age.
\sethscode{customhscode}

> data Person = P String Int

The name of this type is |Person|. It provides a single constructor |P|. This constructor
has two parameters or \emph{fields}. The first field is of the predefined type |String|
and represents the name of the person. The second field is of type |Int| and represents
the person's age. Here is a possible value:
\sethscode{customhscode}

> tom :: Person
> tom = P "Tom" 34

The order of the fields is important and has to conform to the order of the field types
in the |data|-declaration.

Here are a few functions over people:
\sethscode{customhscode}

> mkPerson :: String -> Int -> Person
> mkPerson name age = P name age
> 
> getName :: Person -> String
> getName (P n a) = n
> 
> getAge :: Person -> Int
> getAge (P n a) = a


An ADT like |Person| is also called a \emph{product} type because
there is a value of this type for every element in the (Cartesian) product
of the field types.
\[ |Person| = \{ |P n a| \mid (|n|,|a|) \in |String| \times |Int| \}\]

\paragraph{Sums of Products}
In general we can define an ADT as a sum of products: multiple constructors
can each have a number of fields. For instance, here is an ADT for geometric shapes
that represent circles and rectangles. Circles have a radius and rectangles have
a width and height.
\sethscode{customhscode}

> data Shape
>   = Circle Double
>   | Rectangle Double Double
> 
> area :: Shape -> Double
> area (Circle radius)            = radius * radius * pi
> area (Rectangle width height)   = width * height

The set of all possible values of type |Shape| is:
\[ |Shape| = \{ |Circle r| \mid |r| \in |Double| \} \cup
                    \{ |Rectangle w h| \mid (|w|,|h|) \in |Double| \times |Double| \}\]

\paragraph{Recursively Defined ADTs}
Finally, in the most general form, new ADTs can also be defined in terms of themselves.
Here is an example, an ADT of binary trees.
\sethscode{customhscode}

> data Tree
>   = Empty
>   | Branch Int Tree Tree

A tree is either |Empty| or it is a |Branch| with an element of type |Int| and two subtrees.
These subtrees in turn are either empty or branches\ldots
\ The set of trees of type |Tree| satisfies the recursion equation:
\[ |Tree| = \{ |Empty| \} \cup \{ |Branch e l r| \mid (|e|,|l|,|r|) \in |Int|\times|Tree|\times|Tree| \} \] 

Mathematically speaking this is not a unique definition for the set |Tree|,
because |Tree| appears both on the left- and right-hand side of the equation.
For now we restrict ourselves to the obvious solution of this equation, but later we come back to another solution.

The obvious solution is that trees are \emph{inductively} defined. This means
that every non-empty tree is built from ``smaller'' subtrees that in turn
are defined of yet smaller trees. In other words, every tree is finite.

Recursively defined ADTs naturally give rise to recursive functions over them.
For instance, the recursive function |sumTree| sums the numbers in the given
tree.
\sethscode{customhscode}

> sumTree :: Tree -> Int
> sumTree Empty           = 0
> sumTree (Branch n l r)  = n + sumTree l + sumTree r

More precisely, |sumTree| is defined by structural recursion. Its second
clause, handling a branch, features two recursive calls, one for each of 
the two subtrees of the branch.

%-------------------------------------------------------------------------------
\subsection{Parametric Polymorphism}

Parametric polymorphism is known in the context of object-oriented languages as 
\emph{genericity}. It is an important abstraction mechanism of Haskell.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{The Problem}

Consider the following definition:
\sethscode{customhscode}

< id x = x

This function is the identity function; it simply returns its parameter.

What is the type signature of |id|? Based on what we have seen so far,
many signatures are possible: |Int -> Int|, |Double->Double|,
|Light->Light|, \ldots 
All these signatures are valid because the function does not exploit any
essential property of its parameter.

If we need an identity function for many different types, we would have to
write different versions of the same function, each with a different 
type signature but the same implementation:
\sethscode{customhscode}

> id1 :: Int -> Int
> id1 x = x
> 
> id2 :: Double -> Double
> id2 x = x
> 
> id3 :: Light -> Light
> id3 x = x

This repetition of code is of course sinful. 
Moreover, |id| causes a problem for Haskell's type inference. What type signature
should be inferred automatically? That is ambiguous, and of course it is rather
annoying if type inference were to pick one arbitrarily, if it did not correspond
to what the programmer needs.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{The Solution}

The solution to the above problem is to extend the simple type system we have
seen so far with a new concept that allows us to express that the identity function
works at all types. This is the concept of \emph{type parameters}.
\ We denote these type parameters with lower case letters like |a|, |b|, \ldots
A type parameter is a placeholder for any type. We use them as in the type signature below:
\sethscode{customhscode}

< id :: a -> a
< id x = x

This signature uses one type parameter |a| that occurs twice.
At every call of the function |id| the type parameters are \emph{implicitly}
filled in by the typechecker. For example, at a call |id True| the parameter
|a| is instantiated by |Bool| and at a call |id Green| it is instantiated
by |Light|.

We call a signature that contains type parameters \emph{polymorphic},
in contrast to \emph{monomorphic} signatures that do not contain
type variables.

This polymorphism also solves the ambiguity problem for type inference.
Even though there are several monomorphic signatures for |id|, the polymorphic
one is more general as it covers all the monomorphic ones. We call this
polymorphic signature the \emph{principal} signature as it covers any other
valid signature we can provide for |id|.

Here is an example of a polymorphic function with two type parameters:
\sethscode{customhscode}

< const :: a -> b -> a
< const x y = x


%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Polymorphic ADTs}

Not just functions can be polymorphic, but ADTs too.
A good example is the recursive |Tree| type we saw earlier. The elements of this
type of tree are of type |Int|. What if we also want to have trees with elements
of type |Bool|, |Light| or \ldots? Instead of writing a new |data|-declaration
for every variant, we can just write a single polymorphic one:
\sethscode{customhscode}

%format Tree2 = "\Varid{Tree}"
%format Empty2 = "\Varid{Empty}"
%format Branch2 = "\Varid{Branch}"

> data Tree2 a = Empty2 | Branch2 a (Tree2 a) (Tree2 a)

The type |Tree2| is parameterised in an element type |a| that we can instantiate
in whatever way we want. For example |Tree2 Int| corresponds to our earlier monomorphic
version and |Tree2 Bool| is a tree of |Bool| values.

Observe that |Tree2| itself is not a proper type. We call it a \emph{type constructor}.
It first has to be applied to an element type before we obtain a proper type.

Of course polymorphic ADTs easily lead to polymorphic functions.
Here is an example:
\sethscode{customhscode}

> size :: Tree2 a -> Int
> size Empty2           = 0
> size (Branch2 x l r)  = 1 + size l + size r


%-------------------------------------------------------------------------------
\subsection{Predefined ADTs}

It is useful to be aware of several predefined ADTs in Haskell.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Unit} 
The \emph{unit} type is a type with a single value. Its pseudo-declaration is:
\sethscode{customhscode}

< data () = ()

In other words, the type has the special name |()| and a single value also written |()|.
At first sight, this type is pretty useless because its value is known a priori.
Yet, we will see some useful applications later.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Tuples} 
Haskell provides a series of predefined generic tuple types. The
pseudo-definition of the binary tuple type is:
\sethscode{customhscode}

< data (a,b) = (a,b)

with predefined projection functions:
\sethscode{customhscode}

< fst :: (a,b) -> a
< snd :: (a,b) -> b


Tuples are useful to return multiple results from a function.
An example of this practice is the |quotRem| function that returns
both the quotient and remainder after division.
\sethscode{interactivehscode}

< ?> quotRem 10 3
< (3,1)


Haskell also supports tuples with more than two components. However, no projection
functions are defined for them.
\sethscode{customhscode}

> info :: (String, Int, Double)
> info = ("Tom", 34, 1.94)


%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Generic Sum Type} 
The predefined generic sum type is |Either|.
\sethscode{customhscode}

< data Either a b
<   = Left a
<   | Right b


In contrast to tuples, the type |Either| is not used often.
In practice, new custom ADTs are defined with meaningful constructor names.
Perhaps one reason is that Haskell does not provide special syntax for |Either|.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Lists} The pseudo-definition of the polymorphic list type is:
\sethscode{customhscode}

< data [a]
<   = []
<   | a : [a]

%-------------------------------------------------------------------------------
\subsection{Type Synonyms}

Besides defining \emph{new} ADTs, it is also possible to introduce an
additional name for an existing type. This additional name is known as
\emph{type synonym} or \emph{type alias}. It can be freely exchanged for
the original name of the type. Here is an example:
\sethscode{customhscode}

< type Distance = Int

Now we can write |Distance| wherever |Int| is expected, and vice versa.

Type synonyms are used to indicate that a particular generic type, e.g., |Int|,
is used to denote a specific concept, e.g., distance.
Implicitly and informally this entails that not every operation on this use
of the existing type makes sense. For instance, it does not make sense to
add a non-distance to a distance. Yet, type synonyms do not disallow
such improper operations. To make an explicit distinction between |Distance|
and |Int|, a new ADT has to be defined.
\sethscode{customhscode}

> data Distance = D Int

The disadvantage of this approach is that all useful functions for |Ints|, like
addition, have to be redefined for |Distance|. This is where the advantage of
type synonyms lies: such redefinition is not needed.

Hence there is clearly a trade-off between type synonyms and new ADTs. 
Either one has less programming work, or one gets more support from the 
typechecker. For that reason type synonyms are often used
in the \emph{rapid prototyping} phase of software development, while custom ADTs
are created in the development of robust software.

\paragraph{Strings}
A prominent application of type synonyms in Haskell are |String|s.
Most languages have a separate type for strings to represent text. However,
in Haskell strings are just lists of characters. The |String| type synonym is
simply defined as:
\sethscode{customhscode}

< type String = [Char]

The special notation |"Tom"| is just syntactic sugar for the regular
list notation
|['T','o','m']|.
The advantage of strings as lists is that all polymorphic list functions
also apply to strings.\footnote{
The disadvantage is that strings in Haskell are not represented in memory 
in the same compact way as in other programming languages.
For that reason, Haskell also features alternative libraries with more compact
representations like |Text|.}
\sethscode{interactivehscode}

< ?> head "Tom"
< 'T'
< ?> reverse "Tom"
< "moT"
< ?> length "Tom"
< 3


%===============================================================================
\section{List Comprehensions}

The well-known \emph{comprehension} notation is used in mathematics to 
define sets. For instance, assume $S = \{1,2,3,4,5,6,7,8,9,10 \}$.  Then
we can write the $R$ of squares of elements of $S$ smaller than 5 in this way:
\[ R = \{ n*n \mid n \in S \wedge n < 5\} \]
Of course $R$ is equal to $\{ 1, 4, 9, 16\}$.

In Haskell we can use essentially the same comprehension notation to define
lists.
\sethscode{customhscode}

> s :: [Int]
> s = [1,2,3,4,5,6,7,8,9,10]
> 
> r :: [Int]
> r = [ n * n | n <- s, n < 5]

The syntax |n <- s| in the comprehension is called a \emph{generator} and
the syntax |n < 5| is called a \emph{guard}. The generator generates
candidate values |n| from the list |s| and the guard selects only
the valid ones. These selected values are returned in the resulting list.


A beautiful example of list comprehensions is the following
definition of the well-known quicksort algorithm.
\sethscode{customhscode}

> qs :: [Int] -> [Int]
> qs []          = []
> qs (pivot:xs)  = qs [l | l <- xs, l <= pivot]
>                  ++
>                  [pivot]
>                  ++
>                  qs [h | h <- xs, h > pivot]

A list comprehension can also consist of multiple generators and guards.
Here is an example with two generators that computes the Cartesian product of
two lists.
\sethscode{customhscode}

> cp :: [a] -> [b] -> [(a,b)]
> cp l r = [(a,b) | a <- l, b <- r]

Observe the order in which the pairs are generated:
\sethscode{interactivehscode}

< ?> cp [1,2] ['a','b']
< [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

As you can see, for every element produced by the first generator, all elements 
of the second generator are enumerated. The effect is similar to that of a nested
loop in imperative programming languages.

% \begin{excercise}
% Onderstaande code genereert alle Pythagoriaanse drietallen $(a,b,c)$ van
% onderling verschillende getallen kleiner dan $n$ waarvoor: 
%   \[ a^2 + b^2 = c^2 \]
% 
% > pyth n = [(a,b,c)  | a <- [1..n] 
% >                    , b <- [1..n]
% >                    , c <- [1..n]
% >                    , a < b
% >                    , a^2 + b^2 == c^2
% >                    ]
% 
% Voor elke unieke oplossing $(a,b,c)$ kunnen we op triviale wijze een
% symmetrische oplossing $(b,a,c)$ afleiden.
% De guard |a < b| zorgt ervoor dat we deze symmetrische oplossingen niet genereren; dit heet
% \emph{symmetry breaking}.
% 
% Herschrijf de lijstcomprehensie zodat de berekening sneller gaat.  Doe dit door
% guards en generatoren te herschikken en de intervallen van de generatoren aan
% te passen.
% 
% \end{excercise}
