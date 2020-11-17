data Argal a = Argal a [Argal a]        -- (no hi ha arbre buit en els arbres generals)
    deriving (Show)

type Dts = Argal (String,String)        


------------ FASE DE CONSTURCCIÓ DEL DTS ------------

---- Processat dels exemples a un set manipulable ----

-- Modificar exemple per a que no tingui comes i així es pot indexar fàcilment
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

-- Estructura auxiliar per a crear tuples de freq per a cada valor d'atribut
-- en funció del valor de la classs
-- fst és valor d'atribut i snd freq en funció de valor de classe
-- IMPORTANT: Primer Int per freq en classe 'p' i segon per a classe 'e'
type Taf = (Char,(Int, Int)) 

-- Donada una llista de Taf d'un atribut, un valor de l'atribut i la classe a la que pertany,
-- es retorna la mateixa llista però havent modificat la freq
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
    let attVal = ex !! att
        classVal = ex !! 0
    in
        computeAttributeAux exs att (modifyFreq tafList attVal classVal)

-- Computa un atribut
computeAttribute:: [String] -> Int -> [Taf]
computeAttribute set att = computeAttributeAux set att []

-- Computa totes els atributs
computeAllAttributes:: [String] -> [[Taf]]
computeAllAttributes set = map (\x -> computeAttribute set x) [1..((length $ set !! 0)-1)]

-- Per a un atribut computat, calcula el seu valor
calculateAttValue:: [Taf] -> Int -> Double
calculateAttValue tcaFreq setSize = 0.0




-- buildDts :: [String] -> Dts

main :: IO ()
main = do
    
    -- Take dataset
    contents <- readFile "agaricus-lepiota.example"
    let examples = lines contents
    
    -- Build decision tree
    --let dts = buildDts examples
    let set = processExamples examples

    let tcafreq = computeAllAttributes set
    
    putStrLn $ show set
    print $ show $ tcafreq !! 0
    print $ show $ tcafreq !! 1
    print $ show $ tcafreq !! 2

    return ()
