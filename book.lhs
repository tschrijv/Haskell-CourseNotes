\documentclass{tstextbook}

\usepackage{hyperref}

%\usepackage[pdftex]{graphicx}
\usepackage{booktabs}
%\usepackage{amstext}
%\usepackage{amsmath}
%\usepackage{amssymb}
%\usepackage{amsthm}
\usepackage{framed}
\usepackage{multicol}
\usepackage{url}
%\usepackage[svgnames]{xcolor}
%\usepackage{pgf}
\usepackage{fancyvrb}
\usepackage[retainorgcmds]{IEEEtrantools}
%\usepackage{mathpartir}
%\usepackage{titlesec}
%\usepackage{lipsum}
%\usepackage{verbatim}
\usepackage[all]{xy}
\usepackage{enumerate}
\newcommand\titlebar{\llap{\colorbox[rgb]{0.99,0.78,0.07}{\kern0.15em\thesection\kern0.15em}\quad}}
\usepackage{forest}
%\titleformat{\section}{\Large\bf}{\titlebar}{0cm}{}[\hrule]
%\titleformat{\subsection}{\large\bf}{\llap{\kern0.15em\thesubsection\kern0.15em\quad}}{0cm}{}[]

\newcommand{\ra}[1]{\renewcommand{\arraystretch}{#1}}
\newcounter{excounter}[chapter]
\renewcommand{\theexcounter}{\thechapter.\arabic{excounter}}
%\newenvironment{excercise}{\addtocounter{excounter}{1}\begin{framed}\noindent\textbf{Exercise~\theexcounter}\quad}{\end{framed}}
%\newtheorem{example}{Voorbeeld}
% \newtheorem{excercise}{Oefening}
%\newtheorem{theorem}{Theorem}


%include lhs2TeX.fmt
%include Formatting.fmt
\renewcommand{\hscodestyle}{\ttfamily}
\newcommand{\doubleequals}{\equiv}
\newcommand{\TODO}[1]{{\large \bf TODO:}#1}

\definecolor{shadecolor}{RGB}{240,240,240}

\newenvironment{sidenote}{\begin{table}\begin{framed}\vspace{-3mm}\begin{shaded}}{\end{shaded}\vspace{-6mm}\end{framed}\end{table}}

%\input{Preamble}

\def\commentbegin{\quad$[\![\enskip$}
\def\commentend{$\enskip]\!]$}

\DefineVerbatimEnvironment{QVerbatim}{BVerbatim}{frame=leftline,framesep=5mm,framerule=0.8mm,baseline=t}
\newenvironment{Query}{
    \par\vspace{3mm}\noindent
    \hspace{5mm}
    \begin{pgfpicture}
    \pgfsetbaseline{0pt}
    \pgftext{\pgfimage[width=0.75cm]{symbols.png}}
    \end{pgfpicture}\hspace{5mm}
    \QVerbatim
}{
    \endQVerbatim
    \par\vspace{3mm}
}


\DefineVerbatimEnvironment{Prolog}{Verbatim}{xleftmargin=2cm,frame=leftline,framesep=5mm,framerule=0.8mm}





\begin{document}
\tsbook{Introduction to Haskell}
       {Tom Schrijvers}
       {Tom Schrijvers}
       {2023}
       {xxxxx}{xxx--xx--xxxx--xx--x}{2.0}
       {Scientica}
       {Leuven}


\chapter*{Preface}
%\addcontentsline{toc}{chapter}{Preface}
\input{preface}

%===============================================================================
% \part{Functionele Programmeertalen}

\chapter*{List of Symbols}\label{ch:symbols}
%\addcontentsline{toc}{chapter}{List of Symbols}
\input{fp/symbols}

% %-------------------------------------------------------------------------------
% \chapter{Introduction}\label{ch:inleiding}
% \input{fp/inleiding}
% 
% %===============================================================================
% \part{Basic Topics}
% 
% %-------------------------------------------------------------------------------
% \chapter{First Steps in Haskell}
% \input{fp/eerste_stappen}
%  
% %-------------------------------------------------------------------------------
% \chapter{Functions}
% \input{fp/functies_en_type_classes}
% 
% %-------------------------------------------------------------------------------
% \chapter{I/O}
% \input{fp/io}
% 
% %===============================================================================
% \part{Advanced Topics}
% 
% %-------------------------------------------------------------------------------
% \chapter{Functional Algorithms, a Case Study}
% \input{fp/functional_algorithms}
% 
% %-------------------------------------------------------------------------------
% \chapter{Equational Reasoning}
% \input{fp/EquationalReasoning}
% 
% %-------------------------------------------------------------------------------
% \chapter{Lazy Evaluation}\label{ch:lazy_evaluation}
% \input{fp/laziness}
% 
% % %-------------------------------------------------------------------------------
% % \chapter{Case Study: Parsers}
% % \input{fp/parsers}
% 
% %-------------------------------------------------------------------------------
% \chapter{Monads}\label{ch:monads}
% \input{fp/monads}

%-------------------------------------------------------------------------------
\chapter{Datalog}\label{ch:datalog}
\input{fp/datalog}

\appendix

% \chapter{Nuttige Haskell-Functies}
% \input{fp/nuttigefuncties}

\end{document}
