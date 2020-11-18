data Argal a = Argal a [Argal a]

data DtsNode = Node String String 
    deriving (Show)

type Dts = Argal DtsNode

offsetIndent:: Int -> String
offsetIndent x 
    | x > 0 = "\t" ++ (offsetIndent (x-1))
    | otherwise = ""

showIndent:: Dts -> Int -> String
showIndent (Argal (Node attVal attName) []) indent =
    (offsetIndent indent) ++ attVal ++ "\n" ++ 
    offsetIndent (indent+1) ++ attName ++ "\n" 
showIndent (Argal (Node attVal attName) children) indent =
    (offsetIndent indent) ++ attVal ++ "\n" ++ 
    offsetIndent (indent+1) ++ attName ++ "\n" ++ 
    (concatMap (\x -> showIndent x (indent+2)) children)

showDts:: Dts -> String
showDts dts = showIndent dts 0

------------ FASE DE CONSTURCCIÓ DEL DTS ------------

---- Processat dels exemples a un set manipulable ----

-- Modificar exemple per a que no tingui comes i així es pugui indexar fàcilment
str2char:: String -> String
str2char [] = []
str2char (c:cs)
    | c == ',' = str2char cs
    | otherwise = [c] ++ str2char cs

-- Processa tots els exemples per a fer-los més fàcils de manipular
processExamples:: [String] -> [String]
processExamples [] = []
processExamples (ex:exs) = [str2char ex] ++ processExamples exs


---- Evaluació de cada atribut ----

-- Taf: Estructura per a crear tuples de freqüència per a cada valor d'atribut
-- en funció del valor de la classs
-- (Char,       --> Valor d'atribut
--      (Int,   --> Freq. per a classe 'p'
--      Int))   --> Freq. per a classe 'e'
type Taf = (Char,(Int, Int)) 


---- Traducció de IDs a atributs i valors d'atributs --- 
-- Els exemples sempre han de tenir tenen aquest format:
-- Class | Att1 | Att2 | ... | Att_n
-- Per tant, el nom dels atributs es guarda tenint en compte aquest ordre, en una llista
-- de Strings on la posició indica a quin atribut pertany el nom contingut en la posició.
-- [NomAtt1, NomAtt2, ... NomAtt_n] Per tant, la id del atribut x és (x-1)
-- Aquesta id és mantè per a tota la lògica del programa. Per exemple, la matriu de Taf, la fila
-- 0 fa referència a la llista de Taf de l'atribut 1, i així seqüencialment
tradAttf:: [String]
tradAttf = ["cap-shape", "cap-color", "gill-color"]

-- Per a la tradució de valors d'atribut al seu nom complet, s'utilitza una matriu [[(Char, String)]]
-- on char es la id (que surt en els exemples, i String el nom complet)
tradAttVal:: [[(Char,String)]]
tradAttVal = [[('b',"bell"),('x',"convex")],
              [('n',"brown"),('y',"yellow"),('w',"white")],
              [('k',"black"),('n',"brown"),('p',"pink")]]

takeIdAttVal:: [(Char,String)] -> Char -> Int
takeIdAttVal [] _ = 0
takeIdAttVal ((id,_):xs) att
    | att == id = 0
    | otherwise = 1 + takeIdAttVal xs att

takeNameAttVal:: [(Char,String)] -> Char -> String
takeNameAttVal [] _ = ""
takeNameAttVal ((id,name):xs) att
    | att == id = name
    | otherwise = takeNameAttVal xs att


---- Comput de la llista Taf de tots els atributs ----

-- Donada una llista de Taf d'un atribut, un valor d'aquest atribut i la classe a la que pertany,
-- es retorna la mateixa llista però havent modificat la freq en funció del nou valor-classe
modifyFreq:: [Taf] -> Char -> Char -> [Taf]
modifyFreq [] attVal 'p' = [(attVal, (1,0))]
modifyFreq [] attVal 'e' = [(attVal, (0,1))]
modifyFreq ((att,(pFreq, eFreq)):xs) attVal classVal =
    if attVal == att then
        if classVal == 'p' then ((att,((pFreq+1), eFreq)):xs)
        else ((att,(pFreq, (eFreq+1))):xs)
    else ((att,(pFreq, eFreq)): (modifyFreq xs attVal classVal))


-- Amb el set d'exemples i un int que representa un atribut, es computa una llista 
-- amb totes les freqüències dels valors dels atributs per a cada classe
computeAttributeAux:: [String] -> Int -> [Taf] -> [Taf]
computeAttributeAux [] _ tafList = tafList
computeAttributeAux (ex:exs) att tafList =
    let posatt = att + 1
        attVal = ex !! posatt
        classVal = ex !! 0
    in
        computeAttributeAux exs att (modifyFreq tafList attVal classVal)

-- Computa la llista taf d'un atribut
computeAttribute:: [String] -> Int -> [Int] -> [Taf]
computeAttribute set att updated
    | elem att updated = []
    | otherwise = computeAttributeAux set att []

-- Computa les llistes taf de tots els atributs
computeAllAttributes:: [String] -> [Int] -> [[Taf]]
computeAllAttributes set updated = map (\x -> computeAttribute set x updated) [0..((length $ set !! 0)-2)]


---- Calcul dels valors dels atributs a partir de les seves llistes Taf ----

-- Donada una llista Taf, calcula la suma dels valors relacionats amb les classes amb més moda
calculateAttValueAux:: [Taf] -> [Int] -> Int 
calculateAttValueAux [] mode = foldl (+) 0 mode
calculateAttValueAux ((att,(pFreq, eFreq)):xs) mode
    | pFreq > eFreq = calculateAttValueAux xs (mode ++ [pFreq])
    | otherwise = calculateAttValueAux xs (mode ++ [eFreq])

-- Per a un atribut computat, calcula el seu valor
calculateAttValue:: [Taf] -> Int -> Double
calculateAttValue tafList setSize = (fromIntegral (calculateAttValueAux tafList [])) / (fromIntegral setSize)

-- Calcula el valor de tots els atributs
calculateAllAttValue:: [[Taf]] -> Int -> [Double]
calculateAllAttValue tafM setSize = map (\x -> calculateAttValue x setSize) tafM


---- Procés d'agafar el millor atribut en funció del valor càlculat ----
-- IMPORTANT: En cas d'empat (més d'un atribut amb valor màxim), s'agafa l'atribut amb
-- més valors relacionats amb una única classe. (Si també hi ha empat en aquest segon criteri
-- s'agafa el primer de tots ells)

-- Agafa la primera posició maxima d'una llista
takeFirstMaxPos:: [Int] -> Int
takeFirstMaxPos list = takeFirstMaxPosAux list 0 0 0

takeFirstMaxPosAux:: [Int] -> Int -> Int -> Int -> Int
takeFirstMaxPosAux [] _ maxPos _ = maxPos
takeFirstMaxPosAux (x:xs) pos maxPos maxVal
    | x >= maxVal = takeFirstMaxPosAux xs (pos+1) pos x
    | otherwise = takeFirstMaxPosAux xs (pos+1) maxPos maxVal

-- Per a una llista taf d'un atribut concret, calcula quants valors tenen relacionats una sola classe
computeAttMore0branch:: [Taf] -> Int
computeAttMore0branch [] = 0
computeAttMore0branch ((att,(0,_)):xs) = 1 + computeAttMore0branch xs
computeAttMore0branch ((att,(_,0)):xs) = 1 + computeAttMore0branch xs
computeAttMore0branch (_:xs) = computeAttMore0branch xs

-- Agafa l'atribut que té més valors d'atribut a una sola classe
takeArrMore0Branch:: [Int] -> [[Taf]] -> Int
takeArrMore0Branch maxList tafM =
    let attbranch = map (\x -> computeAttMore0branch (tafM !! x)) maxList
    in takeFirstMaxPos attbranch 

-- Agafa la posició dels valors màxims (més d'una si hi ha empat)
takeAttMaxAux:: [Double] -> Int -> Double -> [Int] -> [Int]
takeAttMaxAux [] _ _ maxList = maxList
takeAttMaxAux (x:xs) pos maxVal maxList
    | x == maxVal = takeAttMaxAux xs (pos+1) maxVal (maxList ++ [pos])
    | otherwise = takeAttMaxAux xs (pos+1) maxVal maxList

-- Sel·lecciona el millor atribut per a consturir el Dts
takeBestAtt:: [Double] -> [[Taf]] -> Int
takeBestAtt values tafM =
    let maxList = takeAttMaxAux values 0 (maximum values) []
    in 
        -- Si hi ha empat, desempatat amb criteri comentat anteriorment
        if (length maxList) > 1 then takeArrMore0Branch maxList tafM
        else maxList !! 0


---- Modificació del set i la llista d'atributs utilitzats a partir de l'atribut sel·leccionat----

-- Una vegada s'ha computat el millor atribut es modifica el set per a eliminar els exemples
-- els quals un valor de l'atribut estava lligat exclusivament a una classe i s'actualitza la
-- llista d'atributs ja utilitzats per a no tornar a utilitzar el mateix

-- Retorna els diferents valors d'un atribut que estan relacionats unicament amb una classe
ex2Delete:: [Taf] -> String
ex2Delete [] = []
ex2Delete ((att,(0,_)):xs) = (att : (ex2Delete xs))
ex2Delete ((att,(_,0)):xs) = (att : (ex2Delete xs))
ex2Delete (_:xs) = ex2Delete xs

-- Donat un exemple del set, mira si s'ha d'eliminar o no 
deleteExFromEachEx:: Int -> String -> String -> String
deleteExFromEachEx att [] ex = ex
deleteExFromEachEx att (x:xs) ex
    | (ex !! att) == x = []
    | otherwise = deleteExFromEachEx att xs ex

-- Donat tot el set, retorna el mateix set però amb els exemples eliminats (si n'hi ha)
deleteExFromAllSet:: Int -> String -> [String] -> [String]
deleteExFromAllSet _ _ [] = []
deleteExFromAllSet att ex2Delete (x:xs) = 
    let newex = deleteExFromEachEx att ex2Delete x 
    in
        if null newex then (deleteExFromAllSet att ex2Delete xs)
        else [deleteExFromEachEx att ex2Delete x] ++ (deleteExFromAllSet att ex2Delete xs)

-- Modifica el set per a eliminar exemples que ja no interessin
modifySet:: Int -> [[Taf]] -> [String] -> [String]
modifySet att tafM set =
    let posAtt = att+1
        delete = ex2Delete (tafM !! att)
    in
        deleteExFromAllSet posAtt delete set

-- Actualitza la llista d'atributs utilitzats per a no repetir-los
updateUsedAtt:: Int -> [Int] -> [Int]
updateUsedAtt att upAtt = upAtt ++ [att]


---- Construcció del Dts a partir del millor atribut i crida recursiva ----

--buildDtsFromAtt:: Int -> [Taf] -> Dts
--buildDtsFromAtt att tafList =
--    let childs = listFromTaf taf
--    Argal att []

-- Donat una llista taf d'un atribut i la traducció unica dels valors, retorna la
-- llista dels valors amb el seu nom complet
listFromTaf:: [Taf] -> [(Char, String)] -> [String]
listFromTaf [] _ = []
listFromTaf ((att,_):xs) trad = ((takeNameAttVal trad att):(listFromTaf xs trad))

-- Donat un Taf, mira si es fulla (1 si p, 2 si e) o no (0 si node)
isLeaf:: Taf -> Int
isLeaf (_,(_,0)) = 1
isLeaf (_,(0,_)) = 2
isLeaf (_,(_,_)) = 0

-- A partir de la llista taf, extreiem els valors de l'atribut que crein fulles
-- i els que crein nodes
nodeOrLeafAttVal:: [Taf] -> [Int]
nodeOrLeafAttVal tafList = map isLeaf tafList

buildDts:: [String] -> [String] -> Dts
buildDts set tradAtt = 
    let tradVal = tradAttVal
        --tradAtt = tradAtt
        tcafreq = computeAllAttributes set []
        maxims = calculateAllAttValue tcafreq (length set)
        posmax = takeBestAtt maxims tcafreq
        newset = modifySet posmax tcafreq set
        updated = updateUsedAtt posmax []
        attName = (tradAtt !! posmax)
        valNames = listFromTaf (tcafreq !! posmax) (tradVal !! posmax)
        valNorL = nodeOrLeafAttVal (tcafreq !! posmax)
        list = zip valNames valNorL
    in (Argal (Node "init" attName) (map (\x-> recursiveBuildDts x newset updated tradAtt) list))


recursiveBuildDts:: (String, Int) -> [String] -> [Int] -> [String] -> Dts
recursiveBuildDts (valName, 1) _ _ _ = (Argal (Node valName "poisonous") [])
recursiveBuildDts (valName, 2) _ _ _ = (Argal (Node valName "edible") [])
recursiveBuildDts (valName, _) set updated tradAtt = 
    let tradVal = tradAttVal
        tcafreq = computeAllAttributes set updated
        maxims = calculateAllAttValue tcafreq (length set)
        posmax = takeBestAtt maxims tcafreq
        newset = modifySet posmax tcafreq set
        newUpdated = updateUsedAtt posmax updated
        attName = (tradAtt !! posmax)
        valNames = listFromTaf (tcafreq !! posmax) (tradVal !! posmax)
        valNorL = nodeOrLeafAttVal (tcafreq !! posmax)
        list = zip valNames valNorL
    in (Argal (Node valName attName) (map (\x-> recursiveBuildDts x newset newUpdated tradAtt) list))






main :: IO ()
main = do
    
    -- Take dataset
    contents <- readFile "agaricus-lepiota.example"
    let examples = lines contents
    let set = processExamples examples

    -- Build decision tree
    let dts = buildDts set ["cap-shape", "cap-color", "gill-color"]

    putStrLn $ (showDts dts)

    return ()
