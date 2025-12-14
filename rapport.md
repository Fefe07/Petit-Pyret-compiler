# Rapport 
Hugo Dischert  
Félix Landreau  


## Choix techniques
### Lexing :  
- Blancs gérés au lexing.

- Les commentaires ne sont pas gérés partout comme des blancs : du fait des commentaires imbriqués, les sont gérés par une règle à part, ce qui fait que les règles qui nécéssitent des blancs (notamment les opérateurs) nécéssitent de traiter à part les débuts de commentaires.  



### Parsing :
- le caractère '<' produit le lexème LT ou le léxème BINOP Blt en fonction des espaces autour, ce qui fait qu'un BINOP Blt peut être un crochet notant les types polymorphes/arguments de liste.

### Typage
- Variables et schémas séparés  
- Type Tintstr qui sert à gèrer la surcharge de +

## Éléments pas encore réalisés

Généralisation des types polymorphes
