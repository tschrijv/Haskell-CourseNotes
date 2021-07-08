%include Formatting.fmt
%include lhs2TeX.fmt

Borrowing notation and concepts from mathematics is an integral part of the
Haskell and functional programming culture. To immerse ourselves in this
culture, we are using the mathematical notation for Haskell source code that
you will find in many papers and resources about Haskell.

This will be unusual at first, but you will get the hang of it quickly. Here
is a conversion table to help you along.
\begin{center}
\ra{1.3}
\begin{tabular}{@@{}ccl@@{}}\toprule
Symbolic & ASCII & Name \\
\midrule
|x == y| & \Verb!x == y! & equal \\
|x /= y| & \Verb!x /= y! & not equal \\
|x <= y| & \Verb!x <= y! & less than or equal \\
|x >= y| & \Verb!x >= y! & greater than or equal \\
|a -> b| & \Verb!a -> b! & function type \\
|x <- xs| & \Verb!x <- xs! & generator in list comprehension \\
|\ x -> e | & \Verb!\x -> e! & anonymous function aka lambda abstraction \\
|l1 ++ l2| & \Verb!l1 ++ l2! & list append \\
|f . g| & \Verb!f . g! & function composition \\
|m >>= f| & \Verb!m >>= f! & monadic bind \\
\bottomrule
\end{tabular}
\end{center}
