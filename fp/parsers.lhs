%include Formatting.fmt
%include lhs2TeX.fmt

%if False

> import Data.Char

%endif

In dit hoofdstuk ontwikkelen we een eenvoudige parserbibliotheek in Haskell.

%===============================================================================
\section{Het Parsertype}

%format >--> = "\rightarrowtail"

We beginnen onze ontwikkeling met het type van een parser.
De essenti\"ele functionaliteit van een parser is om tekst om te zetten in een
gestructureerde data. Typische toepassingen zijn: a) het omzetten van tekst in
een getal (eenvoudig) 

< "123" >--> 123

of b) broncode in een syntaxboom (gevorderd)

< "1 + 2" >--> Add (Lit 1) (Lit 2)

Omdat onze
bibliotheek a priori het resultaattype niet wil vastpinnen, maken we gebruik van
een typevariabele |a|. Op basis hiervan verwacht je dus het type |String -> a|
voor parsers.

Onze bibliotheek gaat echter compositioneel te werk in het opbouwen van
parsers: we willen twee parsers kunnen samenstellen tot een nieuwe parser.
Daartoe consumeert de eerste parser maar een deel van de invoer, net genoeg om
zijn resultaat te berekenen. De rest van de invoer blijft over voor de volgende
parser.  

< "1302 Guldensporenslag" >--> (1302, " Guldensporenslag")

In plaats van |String -> a|, hebben we daarom het type |String -> (a,
String)|: Het resultaat bestaat uit twee delen: een gestructureerde waarde van
type |a| en de overschot van de invoer van type |String|.

Tot slot laten we toe dat parsers ambigu zijn. Dit wil zeggen dat ze op basis
van dezelfde invoer meerdere mogelijke uitvoer kunnen produceren, meestal in functie
van of ze meer of minder invoer consumeren.

< "123" >--> [(0,"123"), (1, "23"), (12,"3"), (123,"")]

Dit levert ons het uiteindelijke parsertype op:

> type Parser a = String -> [(a, String)]

Bij een volledige parser zijn we meestal enkel geinteresseerd in het resultaat dat de invoer
volledig consumeert. Daarom voorzien we deze hulpfunctie:

> runParser :: Parser a -> String -> Maybe a
> runParser p str =
>   case [x | (x, rest) <- p str, null rest] of 
>     (x:_)  -> Just x
>     []     -> Nothing

%===============================================================================
\section{Implementatie}

Het is niet de bedoeling dat de gebruiker van de bibliotheek zijn parsers
volledig zelf schrijft. In de plaats daarvan is het de bedoeling dat de
gebruiker zijn parsers samenstelt op basis van bouwblokken die de bibliotheek
aanbiedt. Deze bouwblokken bestaan enerzijds uit \emph{primitieven} en anderzijds uit
\emph{combinatoren}. Door gebruik te maken van deze bouwblokken, kan de programmeur
zich richten op zijn toepassing zonder zich bezig te moeten houden met implementatiedetails
van parsers.

%------------------------------------------------------------------------------- 
\subsection{Primitieve Parsers}

Primitieve parsers bevatten minimale functionaliteit, die op zichzelf nutteloos
is, maar in combinatie met combinatoren zeer krachtig.

\paragraph{Karakters Inlezen} Deze primitieve parser leest het eerste karakter
in en geeft dit terug zonder omzetting naar een ander type:

> item :: Parser Char
> item = \str ->  case str of
>                   []      -> []
>                   (c:cs)  -> [(c,cs)]

Als de invoer leeg is, geeft de parser niets terug.
Hier zijn enkele voorbeeld:

< > item "abcd"
< [('a',"bcd")]
< 
< > item ""
< []

\paragraph{Falende Parser}
Deze primitieve parser leest geen invoer en faalt gewoonweg.

> abort :: Parser a
> abort = \str -> []

\paragraph{Lege Parser}
Deze primitieve parser geeft een waarde terug zonder iets van de invoer
te consumeren.

> ret :: a -> Parser a
> ret x = \str -> [(x,str)]

%-------------------------------------------------------------------------------
\subsection{Combinatoren}

Met behulp van twee combinatoren, kunnen we primitieve parsers samenstellen tot
complexere.

\paragraph{Parallelle Compositie} Deze combinator past twee parsers \emph{in parallel} toe op
dezelfde invoer. Hierdoor kan een ambigue parser gemaakt worden.

> (\/) :: Parser a -> Parser a -> Parser a
> p_1 \/ p_2 = \str -> p_1 str ++ p_2 str

Bijvoorbeeld:

< > (item \/ ret '?') "abc"
< [('a',"bc"), ('?',"abc")]
<
< > (item \/ ret '?') ""
< [('?',"")]

\paragraph{Sequenti\"ele Compositie} Deze combinator past twee parsers \emph{na elkaar} toe.
De eerste verwerkt een deel van de invoer en produceert een tussentijds resultaat.
De tweede verwerkt de rest van de invoer en het tussentijds resultaat om een eindresultaat
op te leveren en een overschot van de invoer.

> andThen :: Parser a -> (a -> Parser b) -> Parser b
> p_1 `andThen` fp_2 = \str ->
>   [(y,str2) | (x,str1) <- p_1 str, (y, str2) <- fp_2 x str1]

Bijvoorbeeld:

< > (item `andThen` \c -> ret (ord c - ord '0')) "123"
< [(1,"23")]
<
< > (item `andThen` (\c_1 -> item `andThen` (\c_2 -> ret (c_1,c_2)))) "abc"
< [(('a','b'),"c")]

%-------------------------------------------------------------------------------
\subsection{Afgeleide Combinatoren}

Naast de primitieve parsers en combinatoren biedt de bibliotheek nog een aantal handige
afgeleide combinatoren aan. Deze maken geen direct gebruik van de onderliggende voorstelling
van parsers. In de plaats daarvan zijn ze gedefinieerd in termen van de primitieven.

\paragraph{Conditionele Parser} Deze combinator aanvaardt de uitvoer van een
gegeven parser enkel als die aan een gegeven voorwaarde voldoet.

> sat :: (a -> Bool) -> Parser a -> Parser a
> sat cond p =
>   p `andThen` \x -> if  cond x
>                         then ret x
>                         else abort

Deze parser laat alleen kleine letters toe:

> lower :: Parser Char
> lower = sat isLower item

Deze parser laat alleen een gegeven karakter toe:

> char :: Char -> Parser Char
> char d = sat (== d) item

\paragraph{Uitvoertransformatie} Deze combinator transformeert
de uitvoer van een gegeven parser.

> mapP :: (a -> b) -> Parser a -> Parser b
> mapP f p = p `andThen` \x -> ret (f x)

Deze parser laat alleen een cijfer toe en zet het om naar een getal:

> digit :: Parser Int
> digit = mapP toDigit (sat isDigit item)
>   where toDigit c = ord c - ord '0'

\paragraph{Getallen} Met behulp van recursie kunnen we sequenties van karakters
lezen en verwerken. Deze parser leest een getal in:

> number :: Parser Int
> number = digit `andThen` go
>   where  go :: Int -> Parser Int
>          go acc  = (digit `andThen` \d -> go (acc * 10 + d)) \/ ret acc 

Bijvoorbeeld:

< > number "123"
< [(123,""),(12,"3"),(1,"23")]

\paragraph{Haakjes} Deze combinator leest haakjes rond de invoer
van een andere parser.

> parens :: Parser a -> Parser a
> parens p = 
>   char '('  `andThen` \_ ->
>   p         `andThen` \x ->
>   char ')'  `andThen` \_ ->
>   ret x

Bijvoorbeeld:

< > parens number "(123)"
< [(123,"")]

%-------------------------------------------------------------------------------
\subsection{Eigenschappen}

Het is natuurlijk niet de bedoeling dat de eindgebruiker van de bibliotheek
de implementatiedetails van de bibliotheek moet kennen om deze te gebruiken.
In de plaats daarvan baseert hij zich op de informele beschrijving van 
de primitieven en combinatoren. Bijkomend verschaffen we hem met een
meer formele specificatie, onder de vorm van een aantal eigenschappen.

Zo is |\/| een associatieve operator met |abort| als neutraal element:
\begin{IEEEeqnarray*}{rCl"s}
| abort \/ p | & \equiv & | p |                               & \textsc{(AbortLeftUnit)} \\
| p \/ abort | & \equiv & | p |   & \textsc{(AbortRightUnit)} \\
| p_1 \/ (p_2 \/ p_3) | & \equiv & | (p_1 \/ p_2) \/ p_3 |  & \textsc{(AssociativeChoice)}
\end{IEEEeqnarray*}

Ook |andThen| is een associatieve operator met |ret| als neutraal element:
\begin{IEEEeqnarray*}{rCl"s}
| ret x `andThen` fp | & \equiv & | fp x |                               & \textsc{(RetLeftUnit)} \\
| p `andThen` ret | & \equiv & | p |   & \textsc{(RetRightUnit)} \\
| p_1 `andThen` (\x -> fp_2 x `andThen` fp_3) | & \equiv & | (p_1 `andThen` fp_2) `andThen` fp_3 |  & \textsc{(AssociativeAndThen)}
\end{IEEEeqnarray*}

Tot slot is |abort| het opslorpende element van |andThen|, en distribueert |\/| door |andThen|:
\begin{IEEEeqnarray*}{rCl"s}
| abort `andThen` fp | & \equiv & | abort |                               & \textsc{(AbortLeftZero)} \\
| p `andThen` abort  | & \equiv & | abort |   & \textsc{(AbortRightZero)} \\
| (p_1 \/ p_2) `andThen` fp | & \equiv & | (p_1 `andThen` fp) \/ (p_2 `andThen` fp) |  & \textsc{(ChoiceLeftDistr)} \\
| p `andThen` (\x -> fp_1 x \/ fp_2 x)| & \equiv & |(p `andThen` fp_1) \/ (p `andThen` fp_2)| & \textsc{(ChoiceRightDistr)}
\end{IEEEeqnarray*}

Ga na dat aan alle bovenstaande rekenregels voldaan is.

Deze rekenregels zijn interessant om over de gelijkheid van parsers te redeneren. Een mogelijke toepassing is de optimalisatie
van parsers. De rechtse distributiviteit van |\/| is bijvoorbeeld interessant om van rechts naar links toe te passen. Hierdoor
wordt dezelfde invoer maar \'e\'en keer gelezen door |p| in plaats van twee keer.

%===============================================================================
\section{Voorbeeldparser}

Als illustratie van de bibliotheek, ontwikkelen we nu een kleine parser voor
sommen. Het gestructureerde resultaat dat we willen bekomen is van de vorm:

> data Exp =  Lit Int
>          |  Add Exp Exp
>          deriving Show

De parser hiervoor is:

> expr :: Parser Exp
> expr = expr' `andThen` \e_1 -> more e_1 \/ ret e_1
>   where expr'     =  parens expr \/ mapP Lit number
>         more e_1  =  char '+'  `andThen` \_   -> 
>                      expr       `andThen` \e_2 -> 
>                      ret (Add e_1 e_2)

Bijvoorbeeld:

< > runParser expr "1+2+3"
< Just (Add (Lit 1) (Add (Lit 2) (Lit 3)))
<
< > runParser expr "(1+2)+(3+4)"
< Just (Add (Add (Lit 1) (Lit 2)) (Add (Lit 3) (Lit 4)))

