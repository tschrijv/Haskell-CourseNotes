%include Formatting.fmt
%include lhs2TeX.fmt

The pure functional nature of Haskell enables a very simple 
approach to reasoning about the equivalence of programs.
This technique is called \emph{equational reasoning}.
This chapter illustrates the power of this technique on
a number of examples.

%===============================================================================
\section{Proving Simple Lemmas}

%format E_0
%format E_1
%format E_n
%format E_2
%format E_n1 = "\con{E}_{n-1}"

Equational reasoning is essentially a technique for proving lemmas about the
equality of two expressions.
The technique consists of proving 
|E_0 == E_n| by means of 
$n$ \emph{trivial} steps |E_0 == E_1|, |E_1 == E_2|, \ldots and |E_n1 ==
E_n| that immediately follow from known equalities. Because of the transitivity
of equality, together these consecutive steps form a proof of the target equation.

%===============================================================================
\section{Proof by Structural Induction}

Equational reasoning is very powerful in combination with structural induction.
Structural induction is a proof mechanism for properties defined over 
recursive datatypes, typically involving functions defined by structural induction.

%-------------------------------------------------------------------------------
\subsection{Basic Examples}

We illustrate the technique on a few basic properties of |(++)|, which is
defined by structural recursion on its first argument, which happens to be a
value of the recursive datatype |[a]|.
\sethscode{customhscode}

< (++) :: [a] -> [a] -> [a]
< []      ++ ys  = ys
< (x:xs)  ++ ys  = x : (xs ++ ys)

A first trivial property is that |[]| is the neutral element of |(++)| on the left.
\begin{theorem}
\begin{eqnarray*}
  | [] ++ l| & |==| & | l | 
\end{eqnarray*}
\end{theorem}
This follows immediately from the definition of |(++)|. Indeed, we establish the equation
by considering the first rule in the definition of |(++)| as an axiom.

Less trivial to prove is that |[]| is also the neutral element on the right.
\begin{theorem}
\begin{eqnarray*}
  | l ++ []| & |==| & | l | 
\end{eqnarray*}
\end{theorem}

We prove this theorem by means of structural induction on the list |l|.
This means that we prove the theorem independently for every possible constructor
from which |l| can be built: either |l| is of the form |[]|, or
it is of the form |(x:xs)|.
In the latter case we are allowed to make use of the \emph{induction hypothesis}
|xs ++ [] == xs| which states that the property holds for the recursive subterm |xs|.
\begin{description}
\sethscode{otherhscode}
\item[Base case:] |l = []|

<     [] ++ []
< == {- definition of |(++)| -}
<     []

\item[Inductive case:] |l = (x:xs)|

<      (x:xs) ++ []
< == {- definition of |(++)| -}
<      x : (xs ++ [])
< == {- induction hypothesis -}
<      x : xs

\end{description}

The third property we prove is the associativity of |(++)|.
%format l_1
%format l_2
%format l_3
\begin{theorem}
\begin{eqnarray*}
  | (l_1 ++ l_2) ++ l_3| & |==| & | l_1 ++ (l_2 ++ l_3) | 
\end{eqnarray*}
\end{theorem}
We prove this property by structural induction on |l_1|.
\begin{description}
\sethscode{otherhscode}
\item[Base case:] |l_1 = []|

<     ([] ++ l_2) ++ l_3
< == {- definition of |(++)| -}
<     l_2 ++ l_3
< == {- definition of |(++)| -}
<     [] ++ (l_2 ++ l_3)

\item[Inductive case:] |l_1 = (x:xs)|

<     ((x : xs) ++ l_2) ++ l_3
< == {- definition of |(++)| -}
<     (x : (xs ++ l_2)) ++ l_3
< == {- definition of |(++)| -}
<     x : ((xs ++ l_2)) ++ l_3)
< == {- induction hypothesis -}
<     x : (xs ++ (l_2 ++ l_3))
< == {- definition of |(++)| -}
<     (x:xs) ++ (l_2 ++ l_3)

\end{description}
%-------------------------------------------------------------------------------
\subsection{Application: Optimization Correctness}

We start from a simple function that reverses lists:
\sethscode{customhscode}

> reverse :: [a] -> [a]
> reverse []      = []
> reverse (x:xs)  = reverse xs ++ [x]

This definition is known as \emph{naive reverse} because it has an unnecessarily
bad time complexity: $\mathcal{O}(n^2)$. The computation consists essentially
of a nested loop: the outer loop traverses the list and builds a new list by
repeatedly extending a new list by appending an element |x| to the back.
This appending operation is itself a loop over a list.

A more efficient definition of |reverse| is the following:
\sethscode{customhscode}

> reverse' :: [a] -> [a]
> reverse' xs = go xs []
>   where
>     go :: [a] -> [a] -> [a]
>     go []      acc  = acc
>     go (x:xs)  acc  = go xs (x:acc)

This definition makes use of an auxiliary function |go| that takes an additional parameter
|acc|. The function |go| assembles the result in this additional parameter by
adding elements to the front. Because this additional parameter accumulates the
result we call it an \emph{accumulating parameter} or \emph{accumulator} for short.
This function clearly has a linear time complexity. This difference from |reverse|
is due to adding elements to the front of a list in constant time instead of
adding them to the back in linear time.

Intuitively it should be clear that |reverse| and |reverse'| are equivalent functions.
This means that they yield the same output for the same input. Formally:

\begin{theorem}
\begin{eqnarray*}
  | reverse | & |==| & | reverse' | 
\end{eqnarray*}
\end{theorem}

Instead of directly proving the theorem, we first prove a more general
auxiliary theorem that relates |go| and |reverse|.

\begin{theorem}
\begin{eqnarray*}
  | go l acc| & |==| & | reverse l ++ acc | 
\end{eqnarray*}
\end{theorem}

We prove this theorem by structural induction on the list |l|.

\begin{description}
\sethscode{otherhscode}
\item[Base case:] |l = []|

<      go [] acc
< == {- definition of |go| -}
<      acc
< == {- definition of |(++)| -}
<      [] ++ acc
< == {- definition of |reverse| -}
<      reverse [] ++ acc

\item[Inductive case:] |l = (x:xs)|

<      go (x:xs) acc
< == {- definition of |go| -}
<      go xs (x:acc)
< == {- induction hypothesis -}
<      reverse xs ++ (x:acc)
< == {- definition of |(++)| -}
<      reverse xs ++ ([x] ++ acc)
< == {- associativity of |(++)| -}
<      (reverse xs ++ [x]) ++ acc
< == {- definition of |reverse| -}
<      reverse (x:xs) ++ acc

\end{description}

Now we can easily prove the main theorem.
\sethscode{otherhscode}

<      reverse' l
< == {- definition of |reverse'| -}
<      go l []
< == {- auxiliary thoerem -}
<      reverse l ++ []
< == {- property of |(++)| -}
<      reverse l


% \begin{theorem}
% \begin{eqnarray*}
%   | concat . concat | & |==| & |concat . map concat | 
% \end{eqnarray*}
% \end{theorem}
% 
% \begin{description}
% \item[Basisgeval:] |l = []|
% 
% <      concat (concat [])
% < == {- definition van |concat| -}
% <      concat []
% < == {- definition van |map| -}
% <      concat (map concat [])
% 
% \item[Inductief geval:] |l = (x:xs)|
% 
% <      concat (concat (x:xs))
% < == {- definition van |concat| -}
% <      concat (x ++ concat xs)
% < == {- hulpstelling -}
% <      concat x ++ concat (concat xs)
% < == {- inductiehypothese -}
% <      concat x ++ concat (map concat xs)
% < == {- definition van |concat| -}
% <      concat (concat x : map concat xs)
% < == {- definition van |map| -}
% <      concat (map concat (x:xs))
% 
% \end{description}

