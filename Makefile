all: pyretc.exe

pyretc.exe: lexer.mll parser.mly pyretc.ml
	dune build
