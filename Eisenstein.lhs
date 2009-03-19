\documentclass[leqno,fleqn,12pt]{article}

\usepackage{euler,beton,concrete,url,a4wide}
\usepackage[T1]{fontenc}

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

\begin{abstract} In 1858 \cite{stern1858:rationals}, A.M.\ Stern
published a detailed study of a process of constructing an infinite
sequence of numbers from a given pair of numbers.  Stern attributed
the process to Eisenstein, and the sequence of numbers is now known as
the Eisenstein array.

In \cite{jff*09:euclid-alg}, we review Stern's paper and briefly
discuss algorithms that enumerate the elements of the Eisenstein
Array. In this document we present  several Haskell
implementations of these algorithms.  \end{abstract}

%if False

> module Eisenstein where

> import Data.Maybe
> import Data.Ratio
> import Math.OEIS

%endif

\section{The Eisenstein Array} 
Stern \cite{stern1858:rationals} describes a process (which he
attributes to Eisenstein) of generating an infinite sequence of rows
of numbers from a pair of  natural numbers $m$ and $n$.  
The \emph{zeroth} row in the sequence (``nullte
Entwickelungsreihe'') is the given pair of numbers:

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

The process of constructing such rows is repeated indefinitely.  The
sequence of numbers obtained by concatenating the individual rows in
order is what is now called the \emph{Eisenstein array} and denoted by
 $\mathit{Ei\/}(m{,}n)$ (see, for example, \cite[sequence
A064881]{sloane-integers}) .  Stern refers to each occurrence of a
number in rows other than the zeroth row as either a \emph{sum
element} (``Summenglied'') or a \emph{source element}
(``Stammglied'').  The sum elements are the newly added numbers.  For
example, in the first row the number $m{+}n$ is a sum element; in the
second row the number $m{+}n$ is a source element.



\section{Newman's Algorithm} 
In his paper, Stern observes two ways in which the Eisenstein array 
$\mathit{Ei\/}(1{,}1)$ (now known as Stern's diatomic series)
 enumerates the set of positive rationals.  The simplest of the two,
and the first in Stern's paper, is the sequence obtained by
considering consecutive pairs of elements ("zweigliedrige Gruppen") in
each row of the array $\mathit{Ei\/}(1{,}1)$.  (The second is what has
now become known as the Stern-Brocot tree of rationals.)  Stern proved
that the set of such pairs of numbers is exactly the set of pairs
of coprime positive numbers.  In other words, the set of all
consecutive pairs of numbers in rows of $\mathit{Ei\/}(1{,}1)$ is the
set of rational numbers in lowest form.  

Renewed interest in Stern's work has been sparked by Calkin and Wilf
\cite{Calkin-Wilf2000}. They give a recurrence relation for the $k$th
element in $\mathit{Ei\/}(1{,}1)$, and prove that it equals the number of
hyperbinary representations of $k$.  

Moshe Newman is credited with an iterative algorithm for enumerating
the rationals.  Just as Stern's analysis of the sequence of rows of
$\mathit{Ei\/}(1{,}1)$ embodies an algorithm for enumerating the
rationals, Newman's algorithm for enumerating the rationals embodies
an algorithm for enumerating the elements of $\mathit{Ei\/}(1{,}1)$
and, indeed, of $\mathit{Ei\/}(m{,}n)$ for arbitrary positive numbers
$m$ and $n$.  The purpose of the current document is to discuss
several Haskell  implementations of Newman's algorithm adapted to this
purpose.  

Newman's algorithm, in the form derived in \cite{jff*08:rationals}, is
implemented in Haskell as follows.

> cwnEnum :: [Rational]
> cwnEnum = iterate nextCW $ 1%1
>  where  nextCW :: Rational -> Rational
>         nextCW r  =  let  (a,b)  = (numerator r, denominator r)
>                           j      = floor (a%b)
>                      in   b%((2*j+1)*b-a)

Since each rational in the sequence |cwnEnum| is a pair of
consecutive numbers in a row of $\mathit{Ei\/}(1{,}1)$,  Newman's 
algorithm predicts that
each triple of numbers in a given row of $\mathit{Ei\/}(1{,}1)$ has
the form

\begin{displaymath}
a\ \ \ \ \ \ \ b\ \ \ \ \ \ \ (2{}\left\lfloor{\displaystyle\frac{a}{b}}\right\rfloor + 1){\times}b - a\ \ \ \ .
\end{displaymath}
(See \cite[appendix A]{jff*08:rationals} for further discussion on the
extent to which Stern anticipated this algorithm.)  

In order to adapt Newman's algorithm to enumerate 
$\mathit{Ei\/}(1{,}1)$, it suffices to determine when one row is
complete and the next begins.  This is easy since each row begins and
ends with the number $1$.  Function  |newman| below combines this fact
with |cwnEnum| in order to enumerate the elements of $\mathit{Ei\/}(1{,}1)$; 
specifically, the row is completed when the
denominator of the rational is $1$.

> newman :: [Integer]
> newman = concatMap dlevel cwnEnum
>   where dlevel r  | (denominator r) == 1  = [numerator r, 1]
>                   | otherwise             = [numerator r]

\section{Enumerating the Elements of $\mathit{Ei\/}(m{,}n)$}
In order to enumerate the elements of the array
$\mathit{Ei\/}(m{,}n)$, the main problem we have to solve is the
detection of a change of level (i.e.\ when one row is complete and the
next begins).  There are several ways to do this.  One is to
simultaneously enumerate $\mathit{Ei\/}(1{,}1)$ and use this to
control the detection process.

In the following code, the parameters $a$ and $b$ in |eiloop| sequence
through the elements of a row of  $\mathit{Ei\/}(1{,}1)$.  The end of
the row is detected when $b=1$.  (The second clause is
executed only when $b\neq 1$.)
 
> ei11 :: [Integer]  
> ei11 = 1 : eiloop 1 1 
>  where  eiloop a 1  =  1 : 1 : eiloop 1 (a+1)
>         eiloop a b  =  let  k  = 2*floor(a%b)+1
>                        in   b: eiloop b (k*b-a) 

Generalising to $\mathit{Ei\/}(m{,}n)$, we get the following.  (Note
that the parameters |cm| and |cn| of |eiloop| play the same role as
global constants in an imperative program; they record the initial
values of $m$ and $n$.)  The property exploited by the algorithm is that
each element of $\mathit{Ei\/}(m{,}n)$ is a linear combination of $m$
and $n$ of which the coefficients are independent of $m$ and $n$.

> ei :: Integer -> Integer -> [Integer]
> ei m n = m : eiloop 1 1 m n m n
>  where  eiloop a 1 m n cm cn =  n : cm : eiloop 1 (a+1) cm (a*cm+cn) cm cn
>         eiloop a b m n cm cn =  let  k = 2*floor(a%b)+1
>                                 in   n: eiloop b (k*b-a) n (k*n-m) cm cn

We can test if the function |newman| does in fact enumerate the
elements of $\mathit{Ei\/}(1{,}1)$.  Let's compare the first $1000$
elements of both enumerations:

\medskip
\prompt{(take 1000 newman) == (take 1000 (ei 1 1))}\\
\eval{(take 1000 newman) == (take 1000 (ei 1 1))}\\
\prompt{(take 1000 (map (2*) newman)) == (take 1000 (ei 2 2))}\\
\eval{(take 1000 (map (2*) newman)) == (take 1000 (ei 2 2))}\\
%\prompt{(take 1000 (map ((-2)*) newman)) == (take 1000 (ei (-2) (-2)))}\\
%\eval{(take 1000 (map ((-2)*) newman)) == (take 1000 (ei (-2) (-2)))}\\

The part after the prompt, $\gg$, is the Haskell code that
\texttt{ghci} executes. The result is shown in the subsequent
line. The second command shows an instance of the property:

\[
|map (k*) (ei 1 1) == ei k k| ~~~~~.
\]


A simplification of |ei| is obtained by replacing
 |a| and |b| by |m| and |n| in the calculation of |k| (when |m| and
|n| are both positive).  (The justification for this simplification
involves induction on rows.  The induction hypothesis is that if
|(a,b,c)| is a triple (a "driegliedrige Gruppe" in Stern's
terminology) in a given row of $\mathit{Ei\/}(1{,}1)$ and |(i,j,k)| is
a triple in the same position in the same row of
$\mathit{Ei\/}(m{,}n)$, then |floor(a%b) = floor(i%j)|.  (The role
of variable |c| in this hypothesis is simply to exclude the last two
elements in a row.  Indeed, the equality does not hold for these two
elements. Symmetrically, of course, |floor(b%c) = floor(j%k)|.) 

> ei' :: Integer -> Integer -> [Integer]
> ei' m n = m : eiloop 1 1 m n m n
>  where  eiloop a 1 m n cm cn =  n : cm : eiloop 1 (a+1) cm (a*cm+cn) cm cn
>         eiloop a b m n cm cn =  let  k = 2*floor(m%n)+1
>                                 in   n: eiloop b (k*b-a) n (k*n-m) cm cn

We now define the function |test|, which compares the first $1000$
elements of two enumerations of $\mathit{Ei\/}(m{,}n)$, with
$0{\leq}m{\leq}x$ and $1{\leq}n{\leq}x$ :

> test f g x = and [take 1000 (f m n) == take 1000 (g m n) | m<-[0..x] , n<-[1..x]]

We can use |test| to see if the first $1000$ elements of |ei m n| and |ei' m n|,
for $0{\leq}m{\leq}100$ and $1{\leq}n{\leq}100$, are the same (we are testing $10100$ pairs).

\medskip
\prompt{test ei ei' 100}\\
|True|\\ %eval takes too long! But I've checked it (02/03/2009)
%\eval{test ei ei' 100}

Building on |ei'|, a further transformation is to replace the test for
the end of a row in the sequence.  This has been done in the
 function |extnewman|, defined below.  It is the same as |ei|, but it
replaces variables |a| and |b| by variable |r| which counts the rows.
The last two elements in row |r| of $\mathit{Ei\/}(cm{,}cn)$ are
|cm+r*cn| and |cn|, and the first two elements in row |r+1| are |cm|
and |(r+1)*cm+cn|.

> extnewman :: Integer -> Integer -> [Integer]
> extnewman cm cn = cm: loop 0 cm cn cm cn
>  where  loop r m n cm cn  | ((m==(cm+r*cn)) && (n==cn)) =  
>                                       n: cm: loop (r+1) cm ((r+1)*cm+cn) cm cn
>                           | otherwise =  let  k = 2*floor(m%n)+1
>                                          in   n: loop r n (k*n-m) cm cn

We can do a similar test for |extnewman| as we did for |ei'|:

\medskip
\prompt{test ei extnewman 100}\\
|True| %eval takes too long! But I've checked it (02/03/2009)
%\eval{test ei extnewman 100}

\section{The Online Encyclopedia of Integer Sequences} In this
section, we show how we can use the functions from the Haskell module
|Math.OEIS|\footnote{To run this literate Haskell file, you need to
have the module |Math.OEIS| installed. You can download it from
\url{http://hackage.haskell.org/cgi-bin/hackage-scripts/package/oeis}.}
to search for occurrences of the Eisenstein array on the Online
Encyclopedia of Integer Sequences (OEIS) \cite{sloane-integers}.

We start by defining the number of elements, |numElems|, that we want
to send to the OEIS, and a function that converts a list
$[\,x_1,\cdots,x_n\,]$ to the string $"\,x_1,\cdots,x_n\,"$:

> numElems  :: Int
> numElems  = 20

> list2string  :: (Show a) => [a] -> String
> list2string  =  init . tail . show

The following function, |oeis|, inputs two integer
numbers, |m| and |n|,  computes the list of the first |numElems| of |ei
m n|, transforms the list  into a string and checks if it exists in the
OEIS. It prints the description of the sequence, together with its
reference.

> oeis      :: Integer -> Integer -> IO ()
> oeis m n  =  do  s <- searchSequence_IO . list2string . (take numElems) $ ei m n
>                  r <- getDataSeq s
>                  putStrLn $ "Ei(" ++ show m ++ "," ++ show n ++ "):\n\t" ++ r
>   where  getDataSeq             :: Maybe OEISSequence -> IO String
>          getDataSeq Nothing     =  return "Sequence not found."
>          getDataSeq (Just seq)  =  return $ (description seq) 
>                                             ++ " ( " 
>                                             ++ (concatMap (++ " ") (catalogNums seq)) 
>                                             ++ ")"

As an example, here is the output for the sequence |ei 1 1|:

\medskip
\prompt{oeis 1 1}
\noindent\begin{verbatim}
Ei(1,1):
        Triangle T(n,k) = denominator of fraction in k-th term of n-th row of
variant of Farey series. This is also Stern's diatomic array read by
rows (version 1). ( A049456 )
\end{verbatim}
%\eval{oeis 1 1}

%\section{Summary}
%***Summarise what was obtained***


\bibliographystyle{plain}
\bibliography{math,jff,bibliogr}
\end{document}
