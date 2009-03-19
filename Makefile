main:
	lhs2TeX --poly Eisenstein.lhs -o Eisenstein.tex
	latex Eisenstein.tex
	bibtex Eisenstein
	latex Eisenstein.tex
	latex Eisenstein.tex
	dvips Eisenstein.dvi -o Eisenstein.ps
	dvipdfm Eisenstein.dvi

#	pdflatex Eisenstein.tex
oeis:
	ghc EisensteinOEIS.hs -o EisensteinOEIS -package oeis
