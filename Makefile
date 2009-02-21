eisenstein:
	ghc EnumeratingRationals.lhs Einsenstein.hs -o Einsenstein -package oeis

list:
	grep " )" Ei-OEIS100100.txt
