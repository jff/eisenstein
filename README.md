Enumerating elements of the Eisenstein array
==========

This is a Literate Haskell module with functions to enumerate the elements of the Eisenstein array. 
Also, we provide a program that searches for occurrences of the Eisenstein array on OEIS. 
(Joint work with Roland Backhouse.)

I suggest that you read the [PDF version](https://github.com/jff/eisenstein/blob/master/Eisenstein.pdf?raw=true) of the Literate Haskell program.

Background
----------

In 1858, A.M. Stern published a detailed study of a process of constructing an infinite
sequence of numbers from a given pair of numbers.  Stern attributed
the process to Eisenstein, and the sequence of numbers is now known as
the Eisenstein array.

In a recent paper [0], we review Stern's paper and briefly
discuss algorithms that enumerate the elements of the Eisenstein
Array. In this document we present  several Haskell
implementations of these algorithms.

References
----------
[0] Roland C. Backhouse and João F. Ferreira. On Euclid's algorithm and elementary number theory.
Sci. Comput. Program. (76:3), 2011. URL: http://joaoff.com/publications/2010/euclid-alg/
