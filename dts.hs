---- Extructura de dades de l'arbre Dts----
data Argal a = Argal a [Argal a]

data DtsNode = Node String String 
    deriving (Show)

type Dts = Argal DtsNode

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
tradAttF:: [String]
tradAttF = ["cap-shape", "cap-surface", "cap-color", "bruises?", "odor", "gill-attachment", "gill-spacing",
            "gill-size", "gill-color", "stalk-shape", "stalk-root", "stalk-surface-above-ring", "stalk-surface-below-ring",
            "stalk-color-above-ring", "stalk-color-below-ring", "veil-type", "veil-color", "ring-number", "ring-type",
            "spore-print-color", "population", "habitat"]

-- Per a la tradució de valors d'atribut al seu nom complet, s'utilitza una matriu [[(Char, String)]]
-- on char es la id (que surt en els exemples, i String el nom complet)
tradAttVal:: [[(Char,String)]]
            ---bell=b,conical=c,convex=x,flat=f,
tradAttVal=[[('b',"bell"),('c',"conical"),('x',"convex"),('f',"flat"),('k',"knobbed"),('s',"sunken")],
            [('f',"fibrous"),('g',"grooves"),('y',"scaly"),('s',"smooth")],
            [('n',"brown"),('b',"buff"),('c',"cinnamon"),('g',"gray"),('r',"green"),('p',"pink"),('u',"purple"),('e',"red"),('w',"white"),('y',"yellow")],
            [('t',"bruises"),('f',"no")],
            [('a',"almond"),('l',"anise"),('c',"creosote"),('y',"fishy"),('f',"foul"),('m',"musty"),('n',"none"),('p',"pungent"),('s',"spicy")],
            [('a',"attached"),('d',"descending"),('f',"free"),('n',"notched")],
            [('c',"close"),('w',"crowded"),('d',"distant")],
            [('b',"broad"),('n',"narrow")],
            [('k',"black"),('n',"brown"),('b',"buff"),('h',"chocolate"),('g',"gray"),('r',"green"),('o',"orange"),('p',"pink"),('u',"purple"),('e',"red"),('w',"white"),('y',"yellow")],
            [('e',"enlarging"),('t',"tapering")],
            [('b',"bulbous"),('c',"club"),('u',"cup"),('e',"equal"),('z',"rhizomorphs"),('r',"rooted"),('?',"missing")],
            [('f',"fibrous"),('y',"scaly"),('k',"silky"),('s',"smooth")],
            [('f',"fibrous"),('y',"scaly"),('k',"silky"),('s',"smooth")],
            [('n',"brown"),('b',"buff"),('c',"cinnamon"),('g',"gray"),('o',"orange"),('p',"pink"),('e',"red"),('w',"white"),('y',"yellow")],
            [('n',"brown"),('b',"buff"),('c',"cinnamon"),('g',"gray"),('o',"orange"),('p',"pink"),('e',"red"),('w',"white"),('y',"yellow")],
            [('p',"partial"),('u',"universal")],
            [('n',"brown"),('o',"orange"),('w',"white"),('y',"yellow")],
            [('n',"none"),('o',"one"),('t',"two")],
            [('c',"cobwebby"),('e',"evanescent"),('f',"flaring"),('l',"large"),('n',"none"),('p',"pendant"),('s',"sheathing"),('z',"zone")],
            [('k',"black"),('n',"brown"),('b',"buff"),('h',"chocolate"),('r',"green"),('o',"orange"),('u',"purple"),('w',"white"),('y',"yellow")],
            [('a',"abundant"),('c',"clustered"),('n',"numerous"),('s',"scattered"),('v',"several"),('y',"solitary")],
            [('g',"grasses"),('l',"leaves"),('m',"meadows"),('p',"paths"),('u',"urban"),('w',"waste"),('d',"woods")]]


-- Donat un caràcter referent al valor d'un atribut, el tradueix al seu nom complet
takeNameAttVal:: [(Char,String)] -> Char -> String
takeNameAttVal [] _ = ""
takeNameAttVal ((idc,name):xs) att
    | att == idc = name
    | otherwise = takeNameAttVal xs att


---- Comput de la llista Taf de tots els atributs ----

-- Donada una llista de Taf d'un atribut, un valor d'aquest atribut i la classe a la que pertany,
-- es retorna la mateixa llista però havent modificat la freq en funció del nou valor-classe
modifyFreq:: [Taf] -> Char -> Char -> [Taf]
modifyFreq [] attVal 'e' = [(attVal, (0,1))]
-- Si no es [] attVal 'e', sera si o si: [] attVal 'p'
modifyFreq [] attVal _ = [(attVal, (1,0))]
modifyFreq ((att,(pFreq, eFreq)):xs) attVal classVal =
    if attVal == att then
        if classVal == 'p' then ((att,((pFreq+1), eFreq)):xs)
        else ((att,(pFreq, (eFreq+1))):xs)
    else ((att,(pFreq, eFreq)):(modifyFreq xs attVal classVal))


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
calculateAttValueAux ((_,(pFreq, eFreq)):xs) mode
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

-- Agafa la primera posició maxima d'una llista de reals
takeFirstMaxPos:: [Double] -> Int
takeFirstMaxPos list = takeFirstMaxPosAux list 0 0 0.0

takeFirstMaxPosAux:: [Double] -> Int -> Int -> Double -> Int
takeFirstMaxPosAux [] _ maxPos _ = maxPos
takeFirstMaxPosAux (x:xs) pos maxPos maxVal
    | x >= maxVal = takeFirstMaxPosAux xs (pos+1) pos x
    | otherwise = takeFirstMaxPosAux xs (pos+1) maxPos maxVal

-- Per a una llista taf d'un atribut concret, calcula quants valors tenen relacionats una sola classe
computeAttMore0branch:: [Taf] -> Int
computeAttMore0branch [] = 0
computeAttMore0branch ((_,(0,_)):xs) = 1 + computeAttMore0branch xs
computeAttMore0branch ((_,(_,0)):xs) = 1 + computeAttMore0branch xs
computeAttMore0branch (_:xs) = computeAttMore0branch xs

-- Agafa l'atribut que té més proporció de valors d'atribut a una sola classe
takeArrMore0Branch:: [Int] -> [[Taf]] -> Int
takeArrMore0Branch maxList tafM =
    let attbranch = map (\x -> (fromIntegral(computeAttMore0branch (tafM !! x)))/fromIntegral((length(tafM !! x)))) maxList
    in maxList !! (takeFirstMaxPos attbranch) 

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

-- Donat tot el set, retorna el mateix set però amb els exemples eliminats (si n'hi ha)
deleteExFromAllSet:: Int -> Char -> [String] -> [String]
deleteExFromAllSet _ _ [] = []
deleteExFromAllSet posAttAnt valAnt (x:xs)
    | (x !! posAttAnt) /= valAnt = deleteExFromAllSet posAttAnt valAnt xs
    | otherwise = (x:(deleteExFromAllSet posAttAnt valAnt xs))

-- Modifica el set per a eliminar exemples que ja no interessin
modifySet:: Int -> Char -> [String] -> [String]
modifySet attAnt valAnt set =
    let posAttAnt = attAnt + 1
    in
        deleteExFromAllSet posAttAnt valAnt set

-- Actualitza la llista d'atributs utilitzats per a no repetir-los
updateUsedAtt:: Int -> [Int] -> [Int]
updateUsedAtt att upAtt = upAtt ++ [att]


---- Construcció del Dts a partir del millor atribut i crida recursiva ----

-- Donat una llista taf d'un atribut, retorna la
-- llista dels valors de l'atribut
listFromTaf:: [Taf] -> String
listFromTaf [] = []
listFromTaf ((att,_):xs) = (att:(listFromTaf xs))

-- Donat un Taf, mira si es fulla (1 si p, 2 si e) o no (0 si node)
isLeaf:: Taf -> Int
isLeaf (_,(_,0)) = 1
isLeaf (_,(0,_)) = 2
isLeaf (_,(_,_)) = 0

-- A partir de la llista taf, extreiem els valors de l'atribut que crein fulles
-- i els que crein nodes
nodeOrLeafAttVal:: [Taf] -> [Int]
nodeOrLeafAttVal tafList = map isLeaf tafList

buildDts:: [String] -> Dts
buildDts set = 
    let
        tcafreq = computeAllAttributes set []
        maxims = calculateAllAttValue tcafreq (length set)
        posmax = takeBestAtt maxims tcafreq
        updated = updateUsedAtt posmax []
        attName = (tradAttF !! posmax)
        valNames = listFromTaf (tcafreq !! posmax)
        valNorL = nodeOrLeafAttVal (tcafreq !! posmax)
        list = zip valNames valNorL
    in (Argal (Node "init" attName) (map (\x-> recursiveBuildDts x set updated) list))


recursiveBuildDts:: (Char, Int) -> [String] -> [Int] -> Dts
recursiveBuildDts (valAtt, 1) _ updated = (Argal (Node (takeNameAttVal (tradAttVal !! attAnt) valAtt) "poisonous") [])
    where attAnt = updated !! ((length updated)-1)
recursiveBuildDts (valAtt, 2) _ updated = (Argal (Node (takeNameAttVal (tradAttVal !! attAnt) valAtt) "edible") [])
    where attAnt = updated !! ((length updated)-1)
recursiveBuildDts (valAtt, _) set updated = 
    let
        attAnt = updated !! ((length updated)-1)
        newset = modifySet attAnt valAtt set
        tcafreq = computeAllAttributes newset updated
        maxims = calculateAllAttValue tcafreq (length newset)
        posmax = takeBestAtt maxims tcafreq
        newUpdated = updateUsedAtt posmax updated
        attName = (tradAttF !! posmax)
        valName = (takeNameAttVal (tradAttVal !! attAnt) valAtt) 
        valNames = listFromTaf (tcafreq !! posmax) 
        valNorL = nodeOrLeafAttVal (tcafreq !! posmax)
        list = zip valNames valNorL
        moreAtt = ((length (newset !! 0))-1) - (length updated)
    in 
        if (moreAtt > 0) then (Argal (Node valName attName) (map (\x-> recursiveBuildDts x newset newUpdated) list))
        else (Argal (Node valName "Error: No more attributes to make an accuracy prediction") [])

---- Mostreig de l'arbre per pantalla ----
offsetIndent:: Int -> String
offsetIndent x 
    | x > 0 = "  " ++ (offsetIndent (x-1))
    | otherwise = ""

showIndent:: Dts -> Int -> String
showIndent (Argal (Node attVal attName) children) indent 
    | attVal == "init" = attName ++ "\n" ++ (concatMap (\x -> showIndent x (indent+1)) children)
    | otherwise = (offsetIndent indent) ++ attVal ++ "\n" ++ 
                  offsetIndent (indent+1) ++ attName ++ "\n" ++ 
                  (concatMap (\x -> showIndent x (indent+2)) children)

showDts:: Dts -> String
showDts dts = showIndent dts 0


------------ FASE DE CLASSIFICACIÓ ------------

correctAnwAndPos:: String -> [Dts] -> Int -> Int
correctAnwAndPos _ [] _ = -1
correctAnwAndPos answer ((Argal (Node valName _) _):xs) pos
    | answer == valName = pos
    | otherwise = correctAnwAndPos answer xs (pos+1)


classificate:: Dts -> IO String
classificate (Argal (Node _ attName) []) = do
    if (attName == "edible" || attName == "poisonous") then putStrLn ("Prediction: " ++ attName)
    else putStrLn (attName)
    return (attName)
classificate (Argal (Node valName attName) children) = do
    putStrLn ("Which " ++ attName ++ "?")
    answer <- getLine
    let pos = correctAnwAndPos answer children 0
    _ <- if (pos >= 0) then classificate (children !! pos)
         else do classificate (Argal (Node valName attName) children)
    return (answer)

    




main :: IO ()
main = do
    
    -- Take dataset
    contents <- readFile "agaricus-lepiota.data"
    let examples = lines contents
    let set = processExamples examples

    -- Build decision tree
    let dts = buildDts set
    putStrLn $ (showDts dts)

    -- Classify 
    _ <- classificate dts
    
    return ()
