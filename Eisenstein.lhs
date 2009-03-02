\documentclass{article}[12pt]

\usepackage{beton,url}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

%format * = "\mskip-4mu\times\mskip-4mu"
%format floor(x) = "\lfloor" x "\rfloor"
%format % = "/"
%format == = "=="
%format $ = "\ "

%options ghci -fglasgow-exts
\def\prompt#1{\noindent$\gg$ #1}

\title{Enumerating the Elements of the Eisenstein Array}
\author{
   Roland Backhouse \\{\tt\small rcb@@cs.nott.ac.uk} 
   \and 
   Jo\~ao F. Ferreira \\{\tt\small joao@@joaoff.com}
}
\date{\today}

\begin{document}
\maketitle

\begin{abstract}
In \cite{jff*09:euclid-alg}, we discuss several algorithms that enumerate the elements of the Eisenstein 
Array \cite{stern1858:rationals}. In this document we show and discuss several Haskell implementations of these algorithms.
\end{abstract}

%if False

> module Eisenstein where

> import Data.Maybe
> import Data.Ratio

%endif

\section{The Eisenstein Array}
Given two natural numbers $m$ and $n$, Stern \cite{stern1858:rationals} describes a process 
(which he attributes to Eisenstein) of generating an infinite  sequence of rows of numbers.  
The \emph{zeroth}  row in the sequence (``nullte Entwickelungsreihe'') is the given pair of numbers:

\begin{displaymath}
m\ \ \ \ \ n\mbox{\ \ \ .}
\end{displaymath}

Subsequent rows are obtained by inserting between every pair of numbers the sum of the numbers.  Thus
the \emph{first} row is

\begin{displaymath}
m\ \ \ \ \ \ m{+}n\ \ \ \ \ \ n 
\end{displaymath}

and the \emph{second} row is

\begin{displaymath}
m\ \ \ \ \ \ 2{\times}m + n\ \ \ \ \ \ m{+}n\ \ \ \ \ \ m + 2{\times}n\ \ \ \ \ \ n~~. 
\end{displaymath}

The process of constructing such rows is repeated indefinitely.  The sequence of numbers obtained by
concatenating the individual rows in order is what is now called the \emph{Eisenstein array} and denoted by
 $\mathit{Ei\/}(m{,}n)$ (see, for example, \cite[sequence A064881]{sloane-integers}) .  
Stern refers to each occurrence of a number  in rows other than the
zeroth row as either  a 
\emph{sum element} (``Summenglied'')  or a  \emph{source element} (``Stammglied'').    The sum elements are the
newly added numbers.  For example, in the first row the
number $m{+}n$ is a sum element; in the second row the number $m{+}n$ is a  source element. 



\section{Newman's Algorithm}
An interesting question is whether Stern also documents the algorithm currently attributed to
Moshe Newman for enumerating the elements of $\mathit{Ei\/}(1{,}1)$ \cite[section 4.2.3]{jff*09:euclid-alg}. 
Newman's algorithm predicts that each triple of numbers in a given row of 
$\mathit{Ei\/}(1{,}1)$ has the form

\begin{displaymath}
a\ \ \ \ \ \ \ b\ \ \ \ \ \ \ (2{}\left\lfloor{\displaystyle\frac{a}{b}}\right\rfloor + 1){\times}b - a\ \ \ \ .
\end{displaymath}

In \cite[appendix A]{jff*08:rationals}, we have implemented Newman's algorithm as follows.

> cwnEnum :: [Rational]
> cwnEnum = iterate nextCW $ 1%1
>  where  nextCW :: Rational -> Rational
>         nextCW r  =  let  (n,m)  = (numerator r, denominator r)
>                           j      = floor (n%m)
>                      in   m%((2*j+1)*m-n)

For the purpose of this document, we are interested in the elements of $\mathit{Ei\/}(1{,}1)$, i.e.,
in the sequence of numerators given by |cwnEnum|. Function |newman| enumerates the elements of
$\mathit{Ei\/}(1{,}1)$, using the infinite list created by |cwnEnum| (we have to detect a change in level).

> newman :: [Integer]
> newman = concatMap dlevel cwnEnum
>   where dlevel r  | (denominator r) == 1  = [numerator r, 1]
>                   | otherwise             = [numerator r]

\section{Enumerating the Elements of $\mathit{Ei\/}(m{,}n)$}
One way of enumerating the elements of the array $\mathit{Ei\/}(m{,}n)$ is:

> ei :: Integer -> Integer -> [Integer]
> ei m n = m : eiloop 1 1 m n m n
>  where  eiloop a 1 m n cm cn =  n : cm : eiloop 1 (a+1) cm (a*cm+cn) cm cn
>         eiloop a b m n cm cn =  let k = 2*floor(a%b)+1
>                                 in  n: eiloop b (k*b-a) n (k*n-m) cm cn

We can test if the function |newman| is in fact enumerating the elements of $\mathit{Ei\/}(1{,}1)$.
Let's compare the first $1000$ elements of both enumerations:

\medskip
\prompt{(take 1000 newman) == (take 1000 (ei 1 1))}\\
\eval{(take 1000 newman) == (take 1000 (ei 1 1))}\\
\prompt{(take 1000 (map (2*) newman)) == (take 1000 (ei 2 2))}\\
\eval{(take 1000 (map (2*) newman)) == (take 1000 (ei 2 2))}\\
%\prompt{(take 1000 (map ((-2)*) newman)) == (take 1000 (ei (-2) (-2)))}\\
%\eval{(take 1000 (map ((-2)*) newman)) == (take 1000 (ei (-2) (-2)))}\\

The part after the prompt, $\gg$, is the Haskell code that \texttt{ghci} is executing. The result is shown in
the subsequent line. The second command shows an instance of the property:

\[
|map (k*) newman == ei k k| ~~~~~.
\]

%if False

% These are tests. Ignore them.

Replacing |a| and |b| by |m| and |n| in the calculation of |k| does not seem to affect
the function.

> ei' :: Integer -> Integer -> [Integer]
> ei' m n = m : eiloop 1 1 m n m n
>  where  eiloop a 1 m n cm cn =  n : cm : eiloop 1 (a+1) cm (a*cm+cn) cm cn
>         eiloop a b m n cm cn =  let k = 2*floor(m%n)+1
>                                 in  n: eiloop b (k*b-a) n (k*n-m) cm cn

> extnewman :: Integer -> Integer -> [Integer]
> extnewman cm cn = cm: loop 0 cm cn cm cn
>  where  loop r m n cm cn  | ((m==(cm+r*cn)) && (n==cn)) =  
>                                          n: cm: loop (r+1) cm ((r+1)*cm+cn) cm cn
>                           | otherwise =  let k = 2*floor(m%n)+1
>                                          in  n: loop r n (k*n-m) cm cn

%endif

\bibliographystyle{plain}
\bibliography{math,jff}
\end{document}
