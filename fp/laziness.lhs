%include Formatting.fmt
%include lhs2TeX.fmt

This chapter discusses the lazy evaluation strategy of Haskell known
as \emph{call by need}.

%===============================================================================
\section{Evaluation Strategies}

Chapter~\ref{ch:inleiding} has presented the $\lambda$-calculus, 
which forms a foundation for Haskell. The three computation rules
provide equivalence-preserving ways of transforming expressions: 
$\alpha$-conversion, $\beta$-reduction and
$\eta$-conversion. 
Of these three results, the one for $\beta$-reduction conforms to our
intuition about evaluating programs. 
A $\beta$-reduction step simplifies a function call.
By repeatedly applying this step until no more function call appears
in the expression, we can obtain a final result.

Here is an example of a non-trivial expression that is turned into
a final result by repeated application of $\beta$-reduction.
\sethscode{otherhscode}
%{
%format . = "."
%format |-> = "\rightarrowtail_\beta"

<     ((\x.x 5) (\y.(\z.y))) 7
< |->
<     ((\y .(\z.y)) 5) 7
< |-> 
<     (\z.5) 7
< |-> 
<     5


%-------------------------------------------------------------------------------
\subsection{Call by Value}

Not every example is as straightforward as the one above.
Stating that every program is evaluated by means of repeated
$\beta$-reduction still leaves a degree of freedom. Namely
there can be multiple reducible function
applications within an expression. Then we can choose which subexpression
to reduce first.

Here is a simple example with two reducible subexpressions. We 
choose to reduce the one on the right first.
\sethscode{otherhscode}

<     (\y.y) ((\z.z) 5)
< |-> 
<     (\y.y) 5
< |-> 
<     5

The above examples illustrates the conventional evaluation strategy, used by most
programming languages. It is known as \emph{call-by-value}, because arguments are first reduced to values before they are passed to a function.

%-------------------------------------------------------------------------------
\subsection{Call by Name}

There is an alternative reduction order in the running example, where the application on the left is reduced first:
\sethscode{otherhscode}

<     (\y.y) ((\z.z) 5)
< |-> 
<     ((\z.z) 5)
< |-> 
<     5

This strategy is known as \emph{call-by-name}, because it passes the arguments
to the function before evaluating them. Hence the arguments are passed
in their original form (i.e, with their original name).

It may seem a coincidence that both call-by-value and call-by-name yield
the same result in the example, but it is not. In general, if both
evaluation strategies yield a value, that value is identical.

If both strategies yield the same result, then why is it important to distinguish between them?

There are two important reasons for this:
\begin{itemize}
\item 
   Firstly, you have to read the ``fine print'': \emph{if they yield a result}, it is the same. It may be the case though that one strategy does not yield a result, but the other one does. Here not yielding a result means that the evaluation does not terminate.

  Let us illustrate this with an example in Haskell notation. Consider the following two function definitions:
\sethscode{customhscode}

> loop = loop
> const x y = x

   With call-by-value the evaluation of the following expression does not terminate:
\sethscode{otherhscode}
%format |--> = "\rightarrowtail_\textit{CBV}"

<      const 42 loop
< |-->
<      const 42 loop
< |-->
<      ...

   With call-by-name the evaluation does terminate:
\sethscode{otherhscode}
%format |---> = "\rightarrowtail_\textit{CBN}"

<      const 42 loop
< |--->
<      42

   We see that call-by-value always evaluates the arguments, even if they are not needed to obtain the final result. In contrast, call-by-name only evaluates the arguments if and when they are actually needed to obtain the final result.

This observation can be generalized: the termination behavior of call-by-name is better than that of call-by-value. Every expression that terminates with call-by-value, also terminates with call-by-name, but not vice versa.

\item 
  Secondly, even if the two strategies obtain the same result, they may not necessarily do so with the same time and space characteristics. We illustrate the difference in runtime with |const 42 (sum [1..1000])|. With call-by-name
the result |42| is obtained after 1 reduction step, while call-by-value first needs about 1000 reduction steps to compute the sum.

  At first sight call-by-name always needs fewer reduction steps than call-by-value, but that is not the case. This perhaps unexpected weakness of call-by-name is mitigated by call-by-need.

\end{itemize}

%-------------------------------------------------------------------------------
\subsection{Call by Need}

Call-by-name has the annoying performance problem of sometimes doubling
the amount of work.
Here we count three reduction steps for call-by-value:
\sethscode{otherhscode}

<      (\x -> x + x) (1 + 2)     
< |-->
<      (\x -> x + x) 3 
< |-->
<      3 + 3
< |-->
<      6

In contrast call-by-name requires four steps:
\sethscode{otherhscode}

<      (\x -> x + x) (1 + 2)     
< |--->
<      (1 + 2) + (1 + 2)
< |--->
<      3 + (1 + 2)
< |--->
<      3 + 3
< |--->
<      6

We can arbitrarily increase the number of steps with the example |(\x -> x + x)
(sum [1..n])|. What is the source of the problem? The function |\x -> x + x|
doubles the work of the argument |x|, because |x| appears twice in the body of
the function. That is why call-by-name copies the argument expression and
subsequently has to reduce both copies.

Call-by-need solves this problem of call-by-value by remembering the connection between the two copies. Instead of duplicating the work, it is \emph{shared}. If one copy is evaluated, the other copy is immediately replaced by the result. This is illustrated thus:
\sethscode{otherhscode}

%format |----> = "\rightarrowtail_\textit{need}"

<      (\x -> x + x) (1 + 2)     
< |---->
<      (1 + 2) + (1 + 2)
< |---->
<      3 + 3
< |---->
<      6

As you can see, call-by-need only needs three reduction steps. What is not shown in this textual representation is that call-by-need remembers that the two occurrences of |1 + 2| are copies of each other. In the second step the left occurrence is evaluated to |3|, and the right occurrence is immediately replaced by the same result.

In summary call-by-need has the same good termination behavior as call-by-name, better than call-by-value. Besides that call-by-need evaluates every expression at most once. In contrast, call-by-value evaluates every expression exactly once, and call-by-name may do this an arbitrary number of times.

%}

%===============================================================================
% \section{Implementatie van Call-by-Need}
%
% \TODO{pointer-gebaseerde implementatie}

%===============================================================================
\section{Lazy Evaluation in Practice}

%-------------------------------------------------------------------------------
\subsection{Control Abstractions}

An interesting aspect of lazy evaluation is that Haskell makes it easy to
define control abstractions, like \texttt{if-then-else}.
\sethscode{customhscode}

> ifthenelse :: Bool -> a -> a -> a
> ifthenelse True   e1 e2 = e1
> ifthenelse False  e1 e2 = e2

This definition behaves in the same way as the built-in |if then else|:
\sethscode{interactivehscode}

< ?> (\x -> ifthenelse (x > 0) (10 / x) 0) 0
< 0

If we would evaluate the same example with call-by-value, |ifthenelse| would
not behave as expected.

%-------------------------------------------------------------------------------
\subsection{Infinite Data Structures}

Infinite data structers are a remarkable application of lazy evaluation.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\paragraph{Infinite Data Structures}

The most important example of infinite data structures are infinite lists.
This example shows an infinite list of natural numbers:
\sethscode{interactivehscode}

< ?> [1..]
< [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
< 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
< 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,
< 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
< 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
< 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, ...

If we had not cut off the output here, GHCi would have gone on forever.

We can implement the above example ourselves:
\sethscode{customhscode}

> from :: Int -> [Int]
> from n = n : from (n + 1)

\sethscode{interactivehscode}

< ?> from 1
< [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
< 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
< 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,
< 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
< 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
< 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, ...

On its own an infinite list looks like an oddity. No practical program can actually process an infinite amount of data. Every terminating program can only consider a finite part of such infinite amount of data. Yet, thanks to lazy evaluation it is practical to do so with an infinite list. For example:
\sethscode{interactivehscode}

< ?> take 10 [1..]
< [1,2,3,4,5,6,7,8,9,10]

We take the first ten elements of the infinite list. Because the remainder of the list is not needed for the final result, it is not evaluated. In this way lazy evaluation makes a seemingly non-terminating program terminate.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\paragraph{Compositionality}
What makes infinite lists interesting is the purity of their definition. For example:
\sethscode{customhscode}

> nats :: [Int]
> nats = [1..]

defines the natural numbers in a very compact way, without considering termination. Of course every practical terminating program can only consider a finite subset of the natural numbers, but |nats| need not be concerned with the logic for selecting a particular subset. This logic can be defined externally to |nats|. In this way we can work in a highly compositional fashion: 1) we can apply various kinds of selection to |nats|, and 2) we can apply the same selection to various kinds of infinite lists.
\sethscode{interactivehscode}

< ?> take 5 nats
< [1,2,3,4,5]
<
< ?> takeWhile (<7) nats
< [1,2,3,4,5,6]

Moreover, we can define new infinite lists in terms of existing ones.

The even numbers are:
\sethscode{customhscode}

> evens :: [Int]
> evens  = map (2*) nats

\sethscode{interactivehscode}

< ?> take 10 evens
< [2,4,6,8,10,12,14,16,18,20]

An alternative definition for the even numbers is:
\sethscode{customhscode}

> evens' :: [Int]
> evens' = zipWith (*) nats (repeat 2)

where
\sethscode{customhscode}

< repeat :: a -> [a]
< repeat x = x : repeat x
<
< zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
< zipWith f  []      _       = []
< zipWith f  _       []      = []
< zipWith f  (x:xs)  (y:ys)  = f x y : zipWith f xs ys

The factorials are:
\sethscode{customhscode}

> facs :: [Int]
> facs = scanl1 (*) nats

where
\sethscode{customhscode}

< scanl1 :: (a -> a -> a) -> [a] -> [a]
< scanl1 f []     = []
< scanl1 f (x:xs) = go xs x
<   where  go []      acc = [acc]
<          go (x:xs)  acc = acc : go xs (f acc x)
 
\sethscode{interactivehscode}

< ?> take 10 facs
< [1,2,6,24,120,720,5040,40320,362880,3628800]


%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Dataflow Dependencies}

An astounding alternative definition of |nats| is: 
\sethscode{customhscode}

> nats' :: [Int]
> nats' = 1 : map (1+) nats'

This infinite list is defined in terms of itself.
This is seemingly paradoxical: to define the natural numbers, we already have
to have a definition of the natural numbers. Fortunately, there is not actually a paradox. Indeed, the definition defines the first number in the list directly with self-dependency. Based on this first number, we can determine the second number, and so on:
\sethscode{otherhscode}

< nats'  ==  1 : map (1+) nats'
<        ==  1 : map (1+) (1 : ???)
<        ==  1 : 1+1 : map (1+) (1+1 : ???)
<        ==  1 : 2 : map (1+) (2 : ???)

In fact every element depends on the previous one, and the first element is given. Hence, there is an evaluation order in which all the elements can be effectively computed, and lazy evaluation dynamically figures out this evaluation order.

The powers of |2| constitute a similar example, where every number is double the previous one:
\sethscode{customhscode}

> pows2 :: [Int]
> pows2 = 1 : map (2*) pows2

One more example are the Fibonacci numbers, where every number is the sum
\sethscode{customhscode}
of the two previous ones:

> fibs :: [Int]
> fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
 
% %- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% \paragraph{In de Knoop}

% %-------------------------------------------------------------------------------
% \subsection{Geheugengebruik}
% 
% \paragraph{Goedaardig}
% 
% \paragraph{Kwaadaardig}
% 
%===============================================================================
\section{Discussion}

Even though call-by-need has multiple interesting applications and properties,
it is not the most widely preferred evaluation strategy. There are actually a number of disadvantages:
\begin{itemize}
\item 
There is more overhead associated with the implementation of call-by-need (the management of so-called \emph{thunks}), than with call-by-value. This means that programs where call-by-value and call-by-need use the same number of evaluation steps, call-by-value is typically faster.
      
The overhead is partially mitigated by optimized compilation: \emph{strictness analysis} determines when call-by-value can be used.

\item 
It is harder for the programmer to reason about evaluation characteristics (space and time) of call-by-need than call-by-value.

\item 
The combination of call-by-need with I/O and other side effects is quite complex, as we will see later.
\end{itemize}


