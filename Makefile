all: pyretc.exe

pyretc.exe: lexer.mll parser.mly pyretc.ml w.ml
	dune build
