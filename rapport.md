# Rapport 
Hugo Dischert  
Félix Landreau  


## Choix techniques
### Lexing :  
- Blancs gérés au lexing.

- Les commentaires ne sont pas gérés partout comme des blancs : du fait des commentaires imbriqués, les commentaires sont gérés par une règle à part, ce qui fait que les règles qui nécéssitent des blancs (notamment les opérateurs) nécéssitent de traiter à part les débuts de commentaires.  



### Parsing :
- le caractère '<' produit le lexème LT ou le léxème BINOP Blt en fonction des espaces autour, ce qui fait qu'un BINOP Blt peut être un crochet notant les types polymorphes/arguments de liste. Heureusement, ceci ne produit pas de conflits entre les déclarations de fonctions et les comparaisons.
- Le sucre syntaxique qu'est la construction for est supprimé à cette étape

### Typage
- Variables et schémas séparés
- Utilisation d'un type Tintstr pour gérer la surcharge de + lors de la première étape, abandonné au profit d'une simple tentative d'unification des types avec int, puis avec str en cas d'échec.
- Le typage renvoie un nouvel arbre de syntaxe abstraite. Cela permet notamment de gérer la surcharge du +, et d'enlever les informations de type qui deviennent encombrantes après cette étape.

### Production de code
- Utilisation de l'instruction syscall pour les programmes devant lever une erreur à l'éxecution, notamment lorsqu'il y a des comparaisons de fonctions.
- Légère modification de la représentation en mémoire des chaînes de caractère par rapport à l'énoncé : présence d'un champ contenant la taille de la chaîne, utilisé par malloc lors de la concaténation de chaînes.

## Défauts du compilateur
- La mémoire n'étant jamais libéré, et étant allouée non pas seulement lorsque qu'une variable ou constante est déclarée, mais à chaque opération, le compilateur a une utilisation extrêmement inneficace de la mémoire.
  Par exemple, si on évalue l'expressionn "a" + "b", 55 octets vont être alloué de façon permanente en mémoire (18 pour "a", 18 pour "b", et 19 pour "ab"), quand bien même aucune de ces chaînes ne serait réutilisée par la suite. Le choix de représenter le type de donnée sur 8 octets au lieu de 1 empire cette situation.
- Les fermetures capturent la valeur des variables au moment de la déclaration de la fonction, ce qui fait que lorsque qu'une variable est modifiée, l'appel à la fonction utilisera toujours l'ancienne valeur. Ceci n'est pas conforme au langage Pyret. Il aurait été possible de réparer cela en stockant les variables sous forme de références, c'est à dire de pointeurs vers la donnée (qui est elle-même un unique pointeur vers le champ représentant son type), mais nous ne l'avons pas implémenté car ceci aurait nécessité de distinguer les variables et les constantes dans tous le processus de compilation.
