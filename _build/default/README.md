# Implementation

## Implementation necessaire :

### Instuctions
- `putchar(n);`
- `x = n;`
- `if(c) { s1 } else { s2 }`
- `while (c) { s }`
- `return (c);`
- expressions
### Expressions
- `42`, `true`...
- `+`, `*`, `<` ...
- `x`
- `f(x1,x2...)`
### Types
- `int`
- `bool`
- `void`	
### Gestion du cas `int n = 0;` (Declaration et initialisation)

## Ajouts :

### Fonctionnalitees
- Grande quantitee d'operateurs. ()
- Commentaires : `//` ligne et `/*` bloc `*/`
- Formes de `return` alternatives : `return n;` et `return;`
- Formes de `if` alternatives : `if(c) { s1 }` `if(c) i1 else i2` `if(c) i1`
- Forme de `while` alternative : `while(c);`
- Possibilite de verifier le typage de plusieurs fichier source en donnant plusieurs noms de fichier a notre execurable. (Et affichage en etape)

### Gestion des erreurs
- Verification de l'existence d'une fonction `main`
- Verification de l'existence d'un return dans une fonction non `void`
- Verification de l'ordre dans les definitions des **variables globales** et des **fonctions** : *erreur lors de l'utilisation d'un element non defini*
- Detection des **doubles declarations** de variables avec la definition des **limites des blocs** de code.