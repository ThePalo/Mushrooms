# Mushrooms:mushroom:

Pràctica de Haskell de l'assignatura de Llenguatges de Programació, de la facultat FIB (UPC).
L'enunciat es troba a: https://gebakx.github.io/hs-dts/.
Els arxius del repositori són:
* ```dts.hs```: codi en haskell del programa
* ```agaricus-lepiota.data```: dataset (dels bolets)
* ```README.md```: explicacions de la implementació de la solució i precondicions del codi

Per a executar la pràctica, només cal compilar i executar l'arxiu ```dts.hs```:
```
ghc dts.hs
./dts
```

## Implementació de la solució
L'arbre de decisió (Dts) s'ha desenvolupat amb l'objectiu que sigui completament independent de la resta del programa un cop creat. Això vol dir que només amb l'estructura de l'arbre és suficient per a fer la classificació, no fa falta cap estructura auxiliar. Amb aquest objectiu en ment, l'arbre és un arbre general on cada node està format per dos Strings:
1. Valor de l'atribut anterior
2. Pot prendre dos valors (segons si és node o fulla -node sense fills-)
   - Si és node, millor atribut computat a partir d'haver escollit el valor guardat a la primera String 
   - Si és fulla, predicció de la classe (o error en cas que no es pugui fer la predicció).

Per a realitzar aquest arbre es segueix un procés recursiu que acaba quan la predicció té una accuracy del 100% o quan no queden més atributs (en aquest cas és llença un missatge d'error).
En cada crida recursiva es segueixen aquests passos:

1. Si la crida detecta que el nou node és fulla, es genera la fulla amb la predicció en qüestió (o un missatge d'error si no es pot fer aquesta predicció) i no fa més crides recursives. Si no és fulla, segueix els següents passos:
2. A partir de l'atribut i el seu valor per a la branca en qüestió, es calcula el nou dataset (un subconjunt del dataset anterior, però amb els exemples els quals tenen com a valor de l'atribut l'escollit per a aquesta branca). A partir d'ara tots els càlculs que ho requereixin seràn amb aquest nou dataset.
3. Per a cada atribut no utilitzat anteriorment, es calcula la freqüència de la relació de cada valor amb cada classe i es guarda en una estructura.
4. Amb el càlcul anterior, es computa el valor de cada atribut i s'agafa el que té valor màxim
   - Important: En cas d'empat (més d'un atribut amb valor màxim) es desempata agafant l'atribut que té el percentatge més alt entre els seus valors relacionats amb una sola classe i el total de valors de l'atribut utilitzats al dataset (per tant, s'agafa l'atribut que té més valors que crearan fulles respecte al total de valors de l'atribut utilitzats al dataset). Si també hi ha un empat, s'agafa el primer valor màxim en computar aquesta heurística.
5. Ara que ja s'ha escollit el millor atribut, es posa a la llista d'atributs utilitzats per a no repetir-lo en crides recursives.
6. Per a cada valor de l'atribut es fa una crida recursiva que serà un fill d'aquest node.


Una vegada s'ha creat l'arbre, es printa per pantalla de tal manera que sigui fàcilment llegible i es comença el procés de classificació.

Aquesta part utilitza una funció amb un sol paràmetre (el Dts) i que té com a sortida la mònada IO, per a l'entrada i sortida dels valors. La funció és simple: va recorrent l'arbre, deixant que l'usuari esculli el valor de l'atribut i calculant el millor atribut en funció d'aquestes eleccions, fins que s'arriba a una predicció o a un error (per no poder fer una predicció amb el 100% d'accuracy per falta d'atributs).

_En el codi es poden veure comentades totes les funcions i estructures que implementen aquesta solució per al problema._

### Precondicions del programa
Per al correcte funcionament del codi, els exemples del dataset sempre han de seguir aquest format:

```Class | Att1 | Att2 | ... | Att_n```

Això és degut a que la id del atribut contingut a la posició x del dataset és (x-1). Aquesta id és manté per a tota la lògica del programa.
Per exemple, el nom dels atributs es guarda tenint en compte aquest ordre: en una llista de Strings on la posició indica la id de l'atribut al qual pertany el nom contingut en la posició:
```[NomAtt1, NomAtt2, ... NomAtt_n]```

La traducció de valors d'atributs d'un caràcter al seu nom complet també té aquest requeriment, ja que l'estructura és una llista de llistes on la primera llista (posició 0) fa referència a la traducció dels valors de l'atribut 1, la segona llista (posició 1) a la traducció de valors de l'atribut 2 i així seqüencialment.

Si es volen afegir nous atributs o modificar la posició dels ja existents, s'ha de tenir en compte aquest ordre i modificar tot el dataset original, la llista dels noms d'atributs i la llista de llistes de les traduccions dels valors dels atributs en consonància amb l'ordre comentat.

- Important: els valors d'atributs que no es coneixen (indicats al dataset amb el caràcter '?' i que són exclusius de l'atribut _stalk-root_) s'han tractat com un valor més de l'atribut.
   
