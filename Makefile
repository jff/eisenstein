main:
	lhs2TeX --poly Eisenstein.lhs -o Eisenstein.tex
	pdflatex Eisenstein.tex
oeis:
	ghc EisensteinOEIS.hs -o EisensteinOEIS -package oeis
