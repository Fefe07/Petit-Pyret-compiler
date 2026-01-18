# Petit-Pyret-compiler
A compiler written in OCaml for a sublanguage of Pyret

# Utilisation
Use  `make` to create the executable pyretc.exe.
Use the executable on a __.arr__ file to create a .s file.
Compile the assembly code with your favorite compiler, for example with gcc :
`gcc -no-pie file.s -o out`

