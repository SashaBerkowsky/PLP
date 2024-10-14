module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit


--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right

        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio _ = []

procId :: Procesador a a
procId a = [a]

procCola :: Procesador [a] a
procCola [] = []
procCola (_:xs) = xs

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose _ children) = children

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern _ left middle right) = [left, middle, right]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie  (TrieNodo a _) = [a]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo _ children) = children


--Ejercicio 2

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT cNil cTern at = case at of
  Nil -> cNil
  Tern v l c r -> cTern v (rec l) (rec c) (rec r)
  where rec = foldAT cNil cTern

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose cRose (Rose n children) = cRose n (map rec children)
  where rec = foldRose cRose

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie cTrie (TrieNodo n children) = cTrie n (map (\(c, trie) -> (c, rec trie)) children)
  where rec = foldTrie cTrie


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = map (: [])

sufijos :: Procesador [a] [a]
sufijos = foldr (\x suf -> (x : head suf) : suf) [[]]


--Ejercicio 4
preorder :: Procesador (AT a) a
preorder = foldAT [] (\v l c r -> [v] ++ l ++ c ++ r)

inorder :: Procesador (AT a) a
inorder = foldAT [] (\v l c r -> l ++ c ++ [v] ++ r)

postorder :: Procesador (AT a) a
postorder = foldAT [] (\v l c r -> l ++ c ++ r ++ [v])

--Ejercicio 5

-- PARA FUTURA ACLARACION
-- concatMap combina concat y map, recibe dos parametros:
-- 1 - una funcion lambda la cual recibe un parametro y retorna un array en base al parametro
-- 2 - un array
-- concatmap mapea el array (2) y le aplica a cada elemento la función lambda (1) y luego concatena todos los resultados
-- el retorno de concatmap es un array compuesto de aplicar f(2) a cada elemento de (1) y luego unir los resultados

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\v leaves -> v : concat leaves)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\v leaves -> if null leaves then [v] else concat leaves)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\v leaves -> if null leaves
                                    then [[v]]
                                    else map (v :) (concat leaves))

--Ejercicio 6

caminos :: Procesador (Trie a) String
caminos = foldTrie (\_ children -> "" : concatMap (\(char, path) -> map (char :) path) children)


--Ejercicio 7

palabras :: Procesador (Trie a) String
palabras = foldTrie (\v children -> case v of
  Nothing -> concatMap (\(char, path) -> map (char :) path) children
  Just _  -> "" : concatMap (\(char, path) -> map (char :) path) children)


--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc f g h s | f s        = g s
               | otherwise  = h s

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) f g s = f s ++ g s

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) f g s = concatMap f (g s)

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}

main :: IO Counts
main = do runTestTT allTests
--Para mejor claridad declaramos algunas variables utilizadas en los casos de Test
trieConElem :: Trie Bool 
trieConElem = TrieNodo (Just True) [('0', TrieNodo Nothing [])]

trieVacio :: Trie Bool
trieVacio = TrieNodo Nothing []

rosaConRaiz :: RoseTree Int
rosaConRaiz = Rose 1 []

atVacio :: AT Int
atVacio = Nil

atEnunciado :: AT Int
atEnunciado = Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil)) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))

atSingle :: AT Int
atSingle = Tern 1 Nil Nil Nil

atAlto :: AT Int
atAlto = Tern 5 (Tern 10 (Tern 40 (Tern 50 Nil (Tern 60 Nil Nil Nil) Nil) Nil Nil) Nil (Tern 70 Nil Nil (Tern 80 Nil Nil Nil))) (Tern 20 Nil Nil Nil) (Tern 30 Nil Nil Nil)

atChar :: AT Char
atChar = Tern 'a' (Tern 'b' (Tern 'c' Nil Nil Nil) Nil Nil) (Tern 'd' Nil Nil Nil) (Tern 'e' (Tern 'f' Nil Nil Nil) Nil (Tern 'g' Nil Nil Nil))

isAtVacio :: AT Int -> Bool
isAtVacio Nil = True
isAtVacio _ = False 

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1a" ~: testsEj1a,
  "ejercicio1b" ~: testsEj1b,
  "ejercicio1c" ~: testsEj1c,
  "ejercicio1d" ~: testsEj1d,
  "ejercicio1e" ~: testsEj1e,
  "ejercicio1f" ~: testsEj1f,
  "ejercicio1g" ~: testsEj1g,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

testsEj1a = test [ -- Casos de test para el ejercicio 1
  procVacio rosaConRaiz ~=? ([]:: [RoseTree Int]), --RosaConRaiz     
  procVacio Nil ~=? ([]:: [AT Int]),     
  procVacio trieVacio ~=? ([]:: [Trie Bool]),  --Caso de test ProcVacio 3
  procVacio (Rose 1 [Rose 2 []])  ~=? ([]:: [RoseTree Int]),    --Caso de test ProcVacio 4 (estructura no vacía)
  procVacio (Tern 1 Nil Nil Nil) ~=?([]:: [AT Int]), 
  procVacio trieConElem ~=?([]:: [Trie Int])
  ]
testsEj1b = test [ 
    procId rosaConRaiz ~=? [rosaConRaiz], --RosaConRaiz
    procId Nil ~=? ([Nil]:: [AT Int]),
    procId trieVacio ~=? [trieVacio], 
    procId (Rose 1 [Rose 2 []])~=? [Rose 1 [Rose 2[]]], --rosaConRaizYUnHijo
    procId (Tern 1 Nil Nil Nil) ~=? [Tern 1 Nil Nil Nil],
    procId trieConElem ~=? [trieConElem]
  ]
testsEj1c = test[ 
    procCola [rosaConRaiz] ~=? ([]:: [RoseTree Int]), --RosaConRaiz
    procCola [Nil] ~=? ([]:: [AT Int]),
    procCola [trieVacio] ~=? ([]:: [Trie Bool]), 
    procCola [Rose 1 [Rose 2 []], Rose 3 []] ~=? [Rose 3 []],
    procCola [Tern 1 Nil Nil Nil, Nil] ~=? ([Nil]::[AT Int]), -- CAMBIAR NO SIRVE!
    procCola [trieConElem, TrieNodo (Just True) []] ~=? [TrieNodo (Just True) []]
  ]
testsEj1d = test[ 
    procHijosRose rosaConRaiz ~=?([]:: [RoseTree Int]), --RosaConRaiz
    procHijosRose (Rose 1 [Rose 2 [], Rose 3 [Rose 4 []]])  ~=? [Rose 2 [], Rose 3 [Rose 4[]]]
  ]
testsEj1e = test[ 
    procHijosAT (Tern 1 (Tern 2 Nil Nil Nil) Nil Nil )~=? [Tern 2 Nil Nil Nil, Nil, Nil],
    procHijosAT Nil ~=? ([]:: [AT Int])
  ]

testsEj1f = test[ --para este ej hago 3 casos, sin raiz (trie vacio), solo con un elem y con mas de uno
    procRaizTrie (TrieNodo (Just True) [('1', TrieNodo (Just True) [('3', TrieNodo Nothing[])])]) ~=? [Just True :: Maybe Bool],--caso con + de 1 
    procRaizTrie trieVacio ~=? [Nothing :: Maybe Bool],  --vacio 
    procRaizTrie trieConElem ~=? [Just True :: Maybe Bool] --caso con un solo elem
  ]

testsEj1g = test[ --para este ej hago 3 casos, sin raiz (trie vacio), solo con un elem y con mas de uno
    procSubTries (TrieNodo (Just True) [('1', TrieNodo (Just True) [])]) ~=? [('1', TrieNodo (Just True) [])],
    procSubTries trieVacio ~=? ([] :: [(Char, Trie Bool)]),
    procSubTries (TrieNodo (Just True) []) ~=? ([] :: [(Char, Trie Bool)])
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  foldAT 0 (\val i m d -> val + i + m + d) (Tern 1 (Tern 2 Nil Nil Nil) Nil Nil ) ~=? 3,
  foldAT 0 (\val i m d -> val + i + m + d) Nil ~=? 0,
  foldRose (\val results -> val + sum results)(Rose 1 [Rose 2 []]) ~=? 3,
  foldTrie (\val hijos -> if val == Just True then 1 + sum (map snd hijos) else sum (map snd hijos)) (TrieNodo (Just True) [('e', TrieNodo Nothing [])]) ~=? 1,  
  foldTrie (\val hijos -> if val == Just True then (1 :: Int) + sum (map snd hijos) else sum (map snd hijos)) trieVacio ~=? 0
  ]

testsEj3 = test [
  sufijos [Rose 1 [Rose 2 []], Rose 3[]] ~=? [[Rose 1 [Rose 2 []], Rose 3[]], [Rose 3[]], []],
  sufijos [TrieNodo (Just True) [('0', TrieNodo (Just True) [])], TrieNodo (Just True) []] ~=? [[TrieNodo (Just True) [('0', TrieNodo (Just True) [])], TrieNodo (Just True) []], [TrieNodo (Just True) []], []],
  sufijos [Tern 1 Nil Nil Nil, Tern 2 (Tern 3 Nil Nil Nil) Nil Nil] ~=? [[Tern 1 Nil Nil Nil, Tern 2 (Tern 3 Nil Nil Nil) Nil Nil], [Tern 2 (Tern 3 Nil Nil Nil) Nil Nil], []]
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  preorder  at ~=? [1,2,3,4],
  postorder at ~=? [2,3,4,1],
  inorder   at ~=? [2,3,1,4],
  preorder  atEnunciado ~=? [16,1,9,7,2,14,0,3,6,10,8,5,4],
  postorder atEnunciado ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16],
  inorder   atEnunciado ~=? [9,7,1,2,0,3,14,6,16,8,5,10,4],
  preorder  atVacio ~=? [],
  postorder atVacio ~=? [],
  inorder   atVacio ~=? [],
  preorder  atSingle ~=? [1],
  postorder atSingle ~=? [1],
  inorder   atSingle ~=? [1],
  preorder  atAlto ~=? [5,10,40,50,60,70,80,20,30],
  postorder atAlto ~=? [60,50,40,80,70,10,20,30,5],
  inorder   atAlto ~=? [60,50,40,10,70,80,20,5,30],
  preorder  atChar ~=? ['a','b','c','d','e','f','g'],
  postorder atChar ~=? ['c','b','d','f','g','e','a'],
  inorder   atChar ~=? ['c','b','d','a','f','e','g']
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  preorderRose (Rose 1 [Rose 2 [Rose 2 []], Rose 3 [], Rose 4 [], Rose 5 []]) ~=? [1,2,2,3,4,5],
  preorderRose (Rose 1 [Rose 2 [Rose 3 [Rose 4 []]]]) ~=? [1,2,3,4],
  preorderRose (Rose 1 []) ~=? ([1]:: [Int]),
  hojasRose (Rose 1 [Rose 2 [Rose 2 []], Rose 3 [], Rose 4 [], Rose 5 []]) ~=? [2,3,4,5],
  hojasRose (Rose 1 []) ~=? [1],
  hojasRose (Rose 1 [Rose 2 [],  Rose 2 []]) ~=? [2,2],
  ramasRose (Rose 1 [Rose 2 [Rose 2 []], Rose 3 [], Rose 4 [], Rose 5 []]) ~=? [[1,2,2],[1,3],[1,4],[1,5]],
  ramasRose (Rose 1 []) ~=? [[1]]
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  caminos (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]) ~=? ["", "a", "b", "ba", "bad", "c"],
  caminos (TrieNodo (Just True) [('a', TrieNodo (Just True) [('b', TrieNodo Nothing [('c', TrieNodo (Just True) [])])]), ('c', TrieNodo (Just True) [('b', TrieNodo Nothing [])])]) ~=? ["", "a", "ab", "abc", "c", "cb"],
  caminos trieVacio ~=? [""],
  caminos (TrieNodo (Just True) [('a', TrieNodo Nothing [])]) ~=? ["", "a"],
  caminos (TrieNodo Nothing [('a', TrieNodo Nothing [])]) ~=? ["", "a"]
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  palabras (TrieNodo (Just True) [('a', TrieNodo Nothing [])]) ~=? [""],
  palabras (TrieNodo Nothing [('a', TrieNodo Nothing [])]) ~=? [],
  palabras trieVacio ~=? [],
  palabras (TrieNodo Nothing [('a', TrieNodo (Just True) [])]) ~=? ["a"],
  palabras (TrieNodo Nothing [('a', TrieNodo Nothing [('b', TrieNodo (Just True) [('c', TrieNodo (Just True) [])])])]) ~=? ["ab", "abc"],
  palabras (TrieNodo (Just True) [('a', TrieNodo Nothing [('b', TrieNodo (Just True) [('c', TrieNodo (Just True) [])])])]) ~=? ["","ab", "abc"],
  palabras (TrieNodo Nothing [('a', TrieNodo Nothing [('b', TrieNodo (Just True) [('c', TrieNodo (Just True) [])])]) , ('d', TrieNodo (Just True) [('e', TrieNodo Nothing [])])]) ~=? ["ab", "abc", "d"]
  ]

testsEj8a = test [ -- Casos de test para el ejercicio 8a
  ifProc isAtVacio procVacio procId (Tern 1 (Tern 2 Nil Nil Nil) Nil Nil ) ~=? [Tern 1 (Tern 2 Nil Nil Nil) Nil Nil],
  ifProc isAtVacio procVacio procId Nil ~=? ([]:: [AT Int])
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 8b
  (++!) procVacio procId 2 ~=? [2], -- [] ++ [2]
  (++!) caminos palabras (TrieNodo Nothing [('a', TrieNodo Nothing [('b', TrieNodo (Just True) [('c', TrieNodo (Just True) [])])])]) ~=? ["","a","ab","abc","ab","abc"]
  ]
testsEj8c = test [ -- Casos de test para el ejercicio 8c
  (.!) inorder procHijosAT atAlto ~=? [60, 50, 40, 10, 70, 80, 20, 30],
  (.!) preorderRose procHijosRose (Rose 1 [Rose 2 [], Rose 3 [Rose 4 []]]) ~=? [2,3,4],
  (.!) procVacio procHijosAT atAlto ~=? ([]:: [AT Int])
  ]
