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
## Details d'implementation :
- Utilisation de Hashmap pour remplacer les "valeurs" apres les initialisation, permettre la sortie du code des expressions et donc l'utilisation pour les variables globales.
## Ajouts :
### Fonctionnalites
- Affichage de la position de l'element lie a l'erreur
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
## Details d'implementation
- Ajout d'une chaine de caractere pour les expressions et instructions (et pour les fonctions et variables globales), correspondant a une chaine de characteres "ligne:colonne" donnant une meilleure indication de la source des erreurs.
- Changement de la representation du programme : liste de declaration arrivant les unes apres les autres, permettant de savoir si une fonction ou une variable est definie a un moment donne.
- Definition de fonctions d'aide : convertir la position en chaine de charactere dans le generateur d'ast et une fonction indiquant la presence d'un return dans chaque branche du code d'une fonction dans le typechecker.