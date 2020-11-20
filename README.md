# Mushrooms:mushroom:

Pràctica de Haskell de l'assignatura de Llenguatges de Programació, de la facultat FIB (UPC).
L'enunciat es troba a: https://gebakx.github.io/hs-dts/.

Per a executar la pràctica, només cal compilar i executar l'arxiu ```dts.hs```:
```
ghc dts.hs
./dts
```

## Enfocament del problema
L'arbre de desició (Dts) s'ha desenvolupat amb l'objectiu de que sigui completament independent de la resta del programa un cop creat. Això vol dir que només amb l'estrucutra de l'arbre és suficient per a fer la classificació, no fa falta cap estrucutra auxiliar. Amb aquest objectiu en ment, l'arbre és un arbre general on cada node està format per dos Strings:
1. Valor de l'atribut anterior
2. Pot prendre dos valors (segons si es node o fulla -node sense fills-)
  a. Millor atribut computat a partir d'haver escollit el valor guardat a la primera String 
  b. En cas de que sigui una fulla, predicció de la classe (o error en cas que no es pugui fer la predicció).

Per a realitzar aquest arbre es segueix un procés recursiu que acaba quan la predicció té una accuracy del 100% o quan no queden més atributs (en aquest cas és llença un missatge d'error).
En cada crida recursiva és segueixen aquests passos:
1. A partir de l'atribut i el seu valor per a la branca en qüestió, es calcula el nou dataset (qun subconjunt del dataset anterior però amb els exemples els quals tenen com a valor de l'atribut l'escollit per a aquesta branca). A parti d'ara tots els càlculs que ho requereixin seràn amb aquest nou dataset.
2. Per a cada atribut no utilitzat anteriorment, es calcula la freqüència de la relació de cada valor amb cada classe i es guarda en una estructura.
3. Amb el càlcul anterior, es càlcula el valor de cada atribut i s'agafa el que té valor màxim
    - Important: En cas d'empat (més d'un atribut amb valor màxim) es desempata agafant l'atribut que té més porcentatge de valors relacionats amb una sola classe (per tant, que té més valors que seràn una fulla). Si també ens trobem un empat amb aquesta heurística, s'agafa el primer valor màxim al computar aquesta heurística.
4. Ara que ja s'ha escollit el millor atribut, es posa a la llista d'atributs utilitzats per a no repetri-lo en crides recursives.
5. Per a cada valor de l'atribut utilitzat al dataset es fa una crida recursiva que serà un fill d'aquest node.
