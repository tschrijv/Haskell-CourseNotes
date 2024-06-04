%include Formatting.fmt
%include lhs2TeX.fmt

%if False

> import Data.Char
> import System.Random
> import Data.Set hiding (null, toList)
> import Data.Map hiding (null, toList, union)
> import qualified Data.Set as Set
> import qualified Data.Map as Map

%endif

This chapter serves a double purpose. Firstly, it introduces the \emph{Logic
Programming} paradigm by means of the Datalog language. Secondly, because we
develop our own Datalog implementation, this chapter serves as a case study of
a system implemented in Haskell.

%===============================================================================
\section{Propositional Datalog}

We start from a simplified version of Datalog, which we call Propositional
Datalog. It is based on propositional logic, rather than predicate logic (for
full Datalog).

%-------------------------------------------------------------------------------
\subsection{Propositional Logic}

A propositional logic is a quadruple $\mathcal{L} = \langle A, \Omega, Z, I \rangle$ where
the components are:
\begin{itemize}
\item the alpha set $A = \{ p, q, r, \ldots \}$ of atomic formulas or just atoms for short.
\item the omega set $\Omega = \bigcup_i \Omega_i$ of logical connectives, partitioned into disjoint
      subsets $\Omega_i$ of connectives with arity $i$. Typical connectives are:
\begin{eqnarray*}
\Omega_0 & = & \{ \bot, \top \} \\
\Omega_1 & = & \{ \neg \} \\
\Omega_2 & = & \{ \wedge, \vee, \rightarrow, \leftrightarrow \}
\end{eqnarray*}
\item the zeta set $Z$ of inference rules, which define valid reasoning steps. A key inference rule is known as \emph{modus ponens}. It states that, given any two formulas of the form $\phi$ and $\phi \rightarrow \psi$, we can conclude the formula $\psi$.
\item the iota set $I$ of formulas which are known as axioms.
\end{itemize}

The language of $\mathcal{L}$, also called the set of formulas, are inductively
defined by the following rules.
\begin{itemize}
\item Any $p \in A$ is a formula.
\item Given formulas $\phi_1,\ldots,\phi_i$ and connective $f \in \Omega_i$, then $f \phi_1 \ldots \phi_i$ is a formula.
\end{itemize}
For example, with $A = \{ p, q\}$ and $\Omega$ as defined above, these are a few formulas:
\begin{itemize}
\item $p$
\item $q$
\item $p \rightarrow q$
\item $(p \vee q) \rightarrow (q \wedge q)$
\item \ldots
\end{itemize}

While propositional logic is useful (though limited) for the purpose of reasoning, it
is not sufficiently structured for the purpose of computation. This is were Propositional
Datalog comes in.

%-------------------------------------------------------------------------------
\subsection{Datalog}

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Syntax}
Datalog only uses two connectives, $\wedge$ and $\leftarrow$. The latter
connective (reverse implication) is used exclusively in the set of axioms $I$.
All axioms are of the form $q \leftarrow p_1 \wedge \ldots \wedge p_n$ (with $n
\geq 0$). Such formulas are known as \emph{(Horn) clauses}. The atom $q$ is known
as the \emph{head} of the clause and the conjunction $p_1 \wedge \ldots \wedge p_n$ 
as the \emph{body}.
When the body is empth ($n = 0$), the Horn clause is also called a \emph{fact}.

Following programming terminology, the set of axioms $I$ is also called the
\emph{program} $P$. Besides using the above mathematical notation, programs
are also written using ASCII characters as follows:
\begin{itemize}
\item Atoms are written with a lower-case letter (possibly followed by more letters).
\item The connective $\wedge$ is written as a comma `\texttt{,}'.
\item The connective $\leftarrow$ is written as a colon--dash sequence `\texttt{:-}'.
\item Horn clauses end in a full stop `\texttt{.}'.
\item For facts, the colon--dash is omitted.
\end{itemize}
For example, the program $P = \{ p \leftarrow, r \leftarrow, q \leftarrow p \wedge r \}$
is written:
\begin{verbatim}
p.
r.
q :- p, r.
\end{verbatim}

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Semantics}
The one inference rule of Datalog is the resolution rule, which is
essentially the modus ponens rule for Horn clauses: Given a Horn clause
$q \leftarrow p_1 \wedge \ldots \wedge p_n$ and the conjunction $p_1 \wedge \ldots \wedge p_n$,
we can conclude $q$.

Formally, we usually represent a conjunction of given atoms $p_1 \wedge \ldots
\wedge p_n$ as a set $S = \{ p_1, \ldots, p_n \}$. Then the so-called
\emph{immediate consequence} operator $T_P$ performs resolution in bulk on the a given set $S$:
\begin{equation*}
T_P(S) = \{ q \mid (q \leftarrow p_1 \wedge \ldots \wedge p_n) \in P, \{ p_1, \ldots, p_n \} \subseteq S \}
\end{equation*}
This operator derives all the heads of clauses in the program whose body is in the given set.

The meaning of the program is then given as the least fixedpoint $\mathrm{lfp}(T_P)$ of this operator.
A fixedpoint is a set $S$ such that $S = T_P(S)$. The least fixedpoint is the smallest such set.
It can be computed iteratively, starting from the empty set:
\begin{eqnarray*}
S_0 & = & \emptyset \\
S_{i+1} & = & T_P(S_i)  
\end{eqnarray*}
When set no longer changes (i.e., $S_{i+1} = S_i$), we have reached the least fixedpoint. For example,
for the example program above, we get:
\begin{eqnarray*}
S_0 & = & \emptyset \\
S_1 & = & T_P(S_0) = \{ p, r \}  \\
S_2 & = & T_P(S_1) = \{ p, q, r \}  \\
S_3 & = & T_P(S_2) = \{ p, q, r \} = S_2
\end{eqnarray*}
Conceptually, the meaning of the program are all the atoms that can be derived from the program.

A finite program $P$ contains only finitely many distinct atoms. Hence, the
iterative process terminates in a finite number of steps. Indeed, in the worst
case, each iteration adds only one atom. Hence, in the worst case we need as
many steps as there are atoms.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Color Mixing Example}
As a first small example, we encode knowledge about mixing colors.  As atoms,
we use the colors \texttt{red}, \texttt{blue}, \texttt{yellow}, \ldots
These denote that paint of a particular color is available.

Our Datalog program consists of a number of clauses that encode what colors can 
be derived when particular other colors are available.
\begin{Verbatim}
orange :- red, yellow.
green :- blue, yellow.
purple :- red, blue.
black :- blue, orange.
black :- green, red.
black :- yellow, purple.
\end{Verbatim}
Observe that there are three different clauses that derive \texttt{black}.

We can also add some colors as facts, stating that they are available up front without
a need for mixing. For instance,
\begin{Verbatim}
red.
blue.
\end{Verbatim}
The semantics of the program then tell us which colors we have available.
In this case, they are \texttt{blue}, \texttt{red} and \texttt{purple}.

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
\paragraph{Haskell Implementation}
Now let's implement this propositional version of Datalog in Haskell.  

Firstly, we model the syntax with a number of datatypes.
For ease of use, we'll simply represent atoms by means of strings.

> type Atom = String

Hence, we encode atoms $p$ and $q$ as the Haskell strings |"p"| and |"q"|.
For clauses we introduce an algebraic datatype.

> data Clause = Atom :- [Atom]

Haskell has the special rule that operators whose first character is a colon,
are constructors. Here we conveniently make use of this fact to use the operator
|(:-)| as the constructor of a clause. The constructor has two fields: the head is
a single atom, and the body is a conjunction of atoms represented by a list. For
example, the clause $q \leftarrow p \wedge r$ is represented as |"q" :- ["p","r"]|.

Finally, a program is simply a list of clauses.

> type Prog = [Clause]

The small program we have used as our running example can be defined as follows:

> prog :: Prog
> prog = [("p" :- []), ("r" :- []), ("q" :- ["p","r"])]

With the syntax in place, we now turn to the semantics. The immediate consequence operator
$T_P$ processes sets. For this we use the polymorphic datatype |Set a| which
we import in our program by adding |import Data.Set| at the top of the Haskell source file.
The set operations rely on the element type being an instance of the |Ord| type class. This
is satisfied for |String|. Thus we can use |Set Atom|, and define the immediate
consequence operator as follows:

> t :: Prog -> Set Atom -> Set Atom
> t p s = Set.fromList [ h  | (h :- b) <- p, all (`elem` s) b ]

The list comprehension enumerates the |h :- b| clauses in the program |p| and returns 
the heads |h| of the clauses whose body atoms are all members of the given set |s|. 
The latter guard makes use of |member :: Ord a => a -> Set a -> Bool|.
The list that is obtained from the list comprehension is converted to a set with
|fromList :: Ord a => [a] -> Set a|.

For example, $T_P(\emptyset)$ is computed as follows:

< > t prog empty
< fromList ["p","r"]

where |empty :: Ord a => Set a| is the empty set. The |Show| instance of |Set a|
displays sets as the list of their elements preceded by the word |fromList|. If you
copy-paste this output in a program, it recreates that set.

We compute the least fixedpoint of the operator with the 
following higher-order function.

> lfp :: (Set Atom -> Set Atom) -> Set Atom
> lfp op = go op Set.empty
>   where
>     go op s = let s' = op s
>               in if s' == s then s else go op s'

This definition iterates the operator, starting from the empty set, until the
least fixedpoint is reached. 

Finally, the semantics of a Datalog program is defined as follows:

> sem :: Prog -> Set Atom
> sem p = lfp (t p)

For example,

< > sem prog
< fromList ["p","q","r"]

\paragraph{Semi-Naive Algorithm}
The iterative least fixedpoint computation is useful as a concise, executable
specification of the Datalog semantics. However, in terms of performance, it
is rather inefficient, and for that reason labeled ``naive''. 

The algorithm has the property that $S_{i+1} \supseteq S_i$. In every iteration
it recomputes, alongside possibly new elements, all previously computed
elements. This is of course incredibly wasteful.

A less naive algorithm, called the ``semi-naive'' algorithm, computes the
fixedpoint incrementally. The idea of this algorithm is to partition the
elements of an iteration into two sets $S_{i+1} = S_i \cup \Delta_{i+1}$.  Here,
$S_i$ are the elements of the previous iteration and $\Delta_{i + 1}$ are the new
elements. When we consider the subsequent iteration $S_{i+2} = S_i \cup
\Delta_{i+1} \cup \Delta_{i+2}$, the key observation is that the newly derived
elements $\Delta_{i+2}$ must essentially depend on the new elements $\Delta_{i+1}$
of the previous iteration. If they would only depend on $S_i$, they'd already
have been derived in the previous iteration.

The following modified operator only derives the new consequences that depend on $\Delta$.
\begin{equation*}
T'_P(S,\Delta) = \{ q \mid (q \leftarrow p_1 \wedge \ldots \wedge p_n) \in P, \{ p_1, \ldots, p_n \} \subseteq S, \{ p_1, \ldots, p_n \} \cap \Delta \neq \emptyset \} \setminus S 
\end{equation*}
To get the iteration going, we cannot start from $S_0 = \Delta_0 = \emptyset$. Instead,
we have to start from the axioms that are facts: $S_0 = \Delta_0 = F_P$, with
\begin{equation*}
F_P = \{ q \mid (q \leftarrow)  \in P \} 
\end{equation*}
Then we have for all subsequent iterations:
\begin{eqnarray*}
\Delta_{i+1} & = & T'_P(S_i,\Delta_i) \\
S_{i+1}      & = & S_i \cup \Delta_{i+1}
\end{eqnarray*}
The iteration can stop when $\Delta_{i+1} = \emptyset$, i.e., when
no new elements are found.

For our running example, we have:
\begin{eqnarray*}
\Delta_0 & = & \{ p, r \} \\ 
S_0 & = & \{ p, r \} \\ 
\Delta_1 & = & \{ q \} \\
S_1 & = & \{ p, q, r \} \\
\Delta_2 & = & \emptyset 
\end{eqnarray*}

\paragraph{Semi-Naive Implementation}

The function |facts| extracts the facts from a given Datalog program, which
serve as the starting point for the semi-naive iteration.

> facts :: Prog -> Set Atom
> facts p = Set.fromList [ q | (q :- []) <- p ]

Facts are clauses with an empty body. They are easily extracted from the program
by pattern matching in the list comprehension generator. For example,

< > facts prog
< fromList ["p","r"] 

To efficiently identify the clauses that depend on newly derived atoms, we create
an indexed datastructure that maps atoms to the clauses they appear in.

> clauseIndex :: Prog -> Map Atom [Clause]
> clauseIndex p = unionsWith (++) [ Map.singleton r [q :- b] | (q :- b) <- p, r <- b]

We use the |Map| datastructure from the |Data.Map| library for this index
structure.  It associates keys (here atoms) with values (here lists of
clauses).  The |singleton :: Ord k => k -> v -> Map k v| function creates a map
with a single key--value entry. We use it in a list comprehension to create a
singleton map for each body atom of a clause, and then aggregate all the
singleton maps into the resulting map. The aggregation function is |unionsWith
:: Ord k => (v -> v -> v) -> [Map k v] -> Map k v|. This is a higher-order
function that uses its function parameter to combine values associated with the
same key. Here, the values are lists of clauses and we combine them with
|(++)|. For example,

< > clauseIndex prog
< fromList [("p",["q" :- ["p","r"]]),("r",["q" :- ["p","r"]])]

This shows that a |Map| structure is displayed as a list of key--value pairs
preceded by the word |fromList|. In this case, the same clause is associated with both
the atoms |"p"| and |"q"|.

The modified immedicate consequence operator takes the clause index
as well as the $S_i$ and $\Delta_i$ atom sets as parameters.

> t' :: Map Atom [Clause] -> Set Atom -> Set Atom -> Set Atom
> t' m s d =
>   Set.fromList  [ q  | a <- Set.toList d
>                 , (q :- b) <- findWithDefault [] a m
>                 , not (q `elem` s)
>                 , all (`elem` s) b]

This definition differs in two ways from the original:
\begin{enumerate} 
\item 
It does not iterate over all clauses in the program. Instead, it iterates over
the atoms |a| in the delta set, and then retrieve corresponding clauses |q
:- b| in the index structure. The latter makes use of |findWithDefault :: Ord k
:: v -> k -> Map k v -> v|, which uses the empty list as the default value when
the atom does not occur as a key in the map.
\item
It discards any derived atoms |q| that are already in the previous generation |s|.
\end{enumerate}

Finally, the seminaive fixpoint computation creates the index structure and
then repeatedly computes the incremental consequences, starting from the facts
in the program.

> sem' :: Prog -> Set Atom
> sem' p =  go d0 d0 where
>   d0 = facts p
>   ci = clauseIndex p
>   go s d =
>       let d' = t' ci s d
>       in if null d then s
>                    else go (s `union` d') d'

This makes use of two additional |Set| functions: |null :: Ord a => Set a -> Bool|
checks whether a given set is empyt, and |union :: Ord a => Set a -> Set a -> Set a|
computes the union of two sets.

This definition produces the same result as |sem|; it simply avoids rederiving
many atoms.

< > sem' prog
< fromList ["p","q","r"]

%-------------------------------------------------------------------------------
\subsection{Duplication}
An annoying aspect of Propositional Datalog is that many programs involve a lot of
duplication. For example, suppose we are modelling a graph:
\begin{center}
\entrymodifiers={++[o][F-]}
\leavevmode
\xymatrix{
a\ar[r] & b\ar[d]\ar[dl] \\
c & d
}
\end{center}
We can do this with the following facts:
\begin{verbatim}
edgeAB.
edgeBC.
edgeBD.
\end{verbatim}
If we want to know wether we can reach one node from another node,
we can include the following 36 clauses for 9 atoms (3 clauses / atom):
\begin{verbatim}
reachAB :- edgeAB.
reachAB :- reachAC, reachCB.
reachAB :- reachAD, reachDB.

reachAC :- reachAB, reachBC.
reachAC :- edgeAC.
reachAC :- reachAD, reachDC.

reachAD :- reachAB, reachBD.
reachAD :- reachAC, reachCD.
reachAD :- edgeAD.

...
\end{verbatim}
The atom \verb|reachAB| expresses that we can reach node \verb|B| from node \verb|A|.
This is possible in three different ways, each of which is captured in a clause:
\begin{itemize}
\item We have a direct edge \verb|edgeAB| from \verb|A| to \verb|B|.
\item We have an indirect path through node \verb|C|.
\item We have an indirect path through node \verb|D|.
\end{itemize}
There are $4 \times 3 = 12$ distinct node pairs. Hence, we have \verb|reach| atoms.
As each atom has 3 clauses, we have in total $12 \ times 3 = 36$ clauses.

When computing the semantics of this program, only 5 \verb|reach| atoms are derived:
\begin{equation*}
\{
\texttt{reachAB}, 
\texttt{reachAC}, 
\texttt{reachAD}, 
\texttt{reachBC}, 
\texttt{reachBD} 
\}
\end{equation*}
Hence, the program is much larger than the set of derived atoms and it contains
duplicated clause shapes. Can we do something about this?

%===============================================================================
\section{Actual Datalog}

The actual definition of Datalog allows to capture the 36 clauses of 
the graph reachability example with just two clauses:
\begin{verbatim}
reach(X, Y) :- edge(X, Y).
reach(X, Y) :- reach(X, Z), reach(Z, Y).
\end{verbatim}
This makes use of an abstraction mechanism: the \texttt{reach} and
\texttt{edge} atoms take arguments.
The upper case arguments \texttt{X}, \texttt{Y} and \texttt{Z} are known
as (logical) variables; they are placeholders for any possible constants.

The \texttt{edge} facts mention specific constants (written with a lower case):
\begin{verbatim}
edge(a, b).
edge(b, c).
edge(b, d).
\end{verbatim}

The semantics of the program are still the derivable atoms (now with constants as arguments).
\begin{equation*}
\{
\texttt{reach(a,b)}, 
\texttt{reach(a,c)}, 
\texttt{reach(a,d)}, 
\texttt{reach(b,c))}, 
\texttt{reach(b,d)} 
\}
\end{equation*}

We now explore in detail the impact of the arguments on the definition of Datalog.

%-------------------------------------------------------------------------------
\subsection{Syntax}

A Datalog program still consists of clauses, and clauses consist of atoms.
What's different is that an atom takes \emph{arguments}: it is of the form
$p(t_1,\ldots,t_n)$ where $p$ is a \emph{predicate (name)} and $n$ is known as
the \emph{arity} (i.e., number of arguments of the predicate).

Predicates with the same name and different arity are considered to be distinct
predicates. To refer to a predicate with a specific name $p$ and arity $n$, we 
write $p/n$. This way we can for example differentiate the predicates \texttt{edge/2}
and \texttt{edge/3}.

The arguments of predicates are known as \emph{terms}. We distinguish two kinds of terms:
\begin{itemize}
\item \emph{(logical) variables}, written with an upper-case letter ($X$, $Y$, $Z$, \ldots), and
\item \emph{constants}, written with a lower-case letter ($a$, $b$, $c$, \ldots).
\end{itemize}

There is one additional rule about the use of variables: every variable that appears in the
head of a clause, must also appear in the body of that clause. Here are several examples 
of valid and invalid clauses:
\begin{verbatim}
p(a).                 % valid 
p(X).                 % invalid
p(X) :- q(X).         % valid
p(a) :- q(X).         % valid
p(X) :- q(a).         % invalid
p(X,X) :- q(X).       % valid
p(X) :- q(X), r(X).   % valid
\end{verbatim}
Notice that \verb|%| starts a comment.

%-------------------------------------------------------------------------------
\subsection{Grounding Semantics}

There are different ways to give a semantics to Datalog. The first we cover is
based on the semantics of Propositional Datalog, which we are already familiar
with.

The idea is to see a Datalog program as a short-hand for a Propositional
Datalog program. We get its semantics by expanding the former into the latter
and then computing the latter's semantics.

The expansion is known as \emph{grounding}. This word refers to the elimination
of variables. An atom without variables is known as a ground atom, as opposed to
a non-ground atom. Conceptually, we can define grounding in two steps:
\begin{enumerate}
\item Determine the set $C$ of all constants in the program.
\item Replace each clause in the program by set of corresponding ground clauses obtained by
      replacing each variable in the clause by a constant in all possible ways.
\end{enumerate}

For example, consider the Datalog program:
\begin{verbatim}
e(a,b).
p(X,Y) :- e(X,Y).
p(X,Y) :- p(X,Z), p(Y,Z).
\end{verbatim}
This program contains the constants $C = \{ a, b\}$. Its grounding is:
\begin{verbatim}
e(a,b).
%------------------------
p(a,a) :- e(a,a).
p(a,b) :- e(a,b).
p(b,a) :- e(b,a).
p(b,b) :- e(b,b).
%------------------------
p(a,a) :- p(a,a), p(a,a).
p(a,a) :- p(a,b), p(b,a).
p(a,b) :- p(a,a), p(a,b).
p(a,b) :- p(a,b), p(b,b).
p(b,a) :- p(b,a), p(a,a).
p(b,a) :- p(b,b), p(b,a).
p(b,b) :- p(b,a), p(a,b).
p(b,b) :- p(b,b), p(b,b).
\end{verbatim}
As the first clause is already ground, grounding just copies it.
The second clause contains two variables \texttt{X} and \texttt{Y}. As there
are two constants, this clause has $2^2 = 4$ different groundings.
The third clause has three variables \texttt{X}, \texttt{Y} and \texttt{Z}. Hence,
it has $2^3 = 8$ different groundings.

We can see each grounded atom as a propositional atom whose name contains
parentheses and commas.  Then, as before, the semantics can be computed as the
least fixedpoint of the immedicate consequence operator.

If we denote the grounded version of a Datalog programs $P$ as $\mathit{ground}(P)$, 
we can incorporate the grounding in the definition of the immediate consequence operator:
\begin{equation*}
T_P(S) = \{ q \mid (q \leftarrow p_1 \wedge \ldots \wedge p_n) \in \mathit{ground}(P), \{ p_1, \ldots, p_n \} \subseteq S \}
\end{equation*}
Then the overall definition of the semantics, $\mathit{lfp}(T_P)$ remains the same.
For example, for the above program, it is:
\begin{equation*}
\{
\texttt{e(a,b)}, 
\texttt{p(a,b)}
\}
\end{equation*}

This grounding-based semantics shows that Datalog is not actually more
expressive than Propositional Datalog; its programs are just more concise.
The downside of this semantics is that it is indirect.

%-------------------------------------------------------------------------------
\subsection{Haskell Implementation}

The presence of arguments, and notably variables, considerably complicates the implementation.

\paragraph{Syntax Updates}

We introduce a new datatype |Term| to represent the arguments of atoms.

> data Term = Var Int | Constant String

We distinguish two terms. 
Constants take their name, a string, as a parameter.
Variables are identified by an integer. Different integers
denote different variables.

With terms in place, we redefine the type of atoms.

> data Atom2 = Atom2 Predicate [Term]

An atom now consists of a predicate name and a list of terms as arguments.
The predicate name is just a string.

> type Predicate = String

The definitions of |Clause| and |Prog| do not change.
%if False

> data Clause2 = Atom2 := [Atom2]
> type Prog2 = [Clause2]


%endif

Here is the Haskell encoding of the reachability program we saw earlier:

> prog2 :: Prog2
> prog2 =  [ edge a b  := []
>          , edge b c  := []
>          , edge b d  := []
>          , reach varX varY  := [edge varX varY]
>          , reach varX varY  := [reach varX varZ, reach varZ varY]
>          ]
>   where edge x y = Atom2 "edge" [x,y]
>         reach x y = Atom2 "reach" [x,y]
>         varX = Var 1
>         varY = Var 2
>         varZ = Var 3
>         a = Constant "a"
>         b = Constant "b"
>         c = Constant "c"
>         d = Constant "d"

\paragraph{Semantics Updates}

Computing a full grounding of the Datalog program up front can be quite costly.
Instead, we will follow a more incremental approach where we ground clauses
incrementally, when needed.

The need for grounding will arise when we check whether a body atom in a clause
is equal to one of the given ground atoms. Hence, we ground