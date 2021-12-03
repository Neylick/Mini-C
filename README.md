# Implementation
## Implementation necessaire :
### Variables Globales
- int taille = 64;
### Fonctions 
- int pgcd(int a,int b) { ... }
### Instuctions
- Fonction d'affichage par code ASCII `putchar(n);`
- Affectation de valeur a une variable `x = n;`
- Blocs conditionnels `if(c) { s1 } else { s2 }`
- Boucle conditionnelle `while (c) { s }`
- Renvoie de valeur des fonctions `return (c);`
- expressions
### Expressions
- Constantes booleennes et entieres `42`, `true`... 
- Operateurs logiques et entiers `+`, `*`, `<` ... 
- Variables `x` 
- Appel de fonction `f(x1,x2...)` 
### Types
- Entiers `int` 
- Booleens `bool` 
- Vide `void`	
### Fonctionnalites
- Gestion du cas `int n = 0;` (Declaration et initialisation)
- Analyse lexicale
- Analyse syntaxique
- Verification des types (Types de retour, type de parametres, type des variables)

## Ajouts :
### Fonctionnalites
- Constante entieres : hexadecimale `0xeFF1c4cE`, octale `0432213` et binaires `0b0_1_0_1_0_1`
- Commentaires : `//` ligne et `/*` bloc `*/`
- Boucles `do while`	et `for`
- Operation conditionelle `switch`, avec les instructions associees : `break`, `default`, `case`.
- Instruction `continue` (sans effet dans le typechecker)
- Instructions `--` et `++` 
- Formes de `return` alternatives : `return n;` et `return;`
- Formes de `if` alternatives : `if(c) { s1 }` `if(c) i1 else i2` `if(c) i1`
- Forme de `while` alternative : `while(c);`
- Possibilite de verifier le typage de plusieurs fichier source en donnant plusieurs noms de fichier a notre execurable. Arret a la premiere erreur.
### Gestion des erreurs
- Verification de l'existence d'une fonction `main`
- Verification de l'existence d'un `return` dans une fonction non `void`
- Verification de l'ordre dans les definitions des **variables globales** et des **fonctions** : *erreur lors de l'utilisation d'un element non defini*
- Detection des **doubles declarations** de variables avec la definition des **limites des blocs** de code.