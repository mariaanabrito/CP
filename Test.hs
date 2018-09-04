--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 29 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também identificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: a73580
-- Nome: Maria Ana da Cunha Monteiro Fernandes de Brito
-- Curso: Mestrado Integrado em Engenharia Informática
--
-- Aluno 2
-- Número: a75135
-- Nome: Bruno Martins Pereira
-- Curso: Mestrado Integrado em Engenharia Informática
--


module Main where

import System.Random
import Graph
import Test.HUnit hiding (path, Path)
import Test.QuickCheck
import Data.Set as Set hiding (union)

--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--

g1 :: Graph Int
g1 = Graph { nodes = fromList [1, 2, 3, 4, 5], 
             edges = fromList [Edge 1 2, Edge 2 3, Edge 2 5, Edge 5 4]
            }

test_swap :: Test
test_swap = swap (Edge 1 2) ~?= (Edge 2 1)

test_isEmpty1 :: Test
test_isEmpty1 = isEmpty g1 ~?= False

gempty :: Graph Int
gempty = Graph { nodes = fromList [],
                 edges = fromList []
               }

test_isEmpty2 :: Test
test_isEmpty2 = isEmpty gempty ~?= True

test_isValid1 :: Test
test_isValid1 = isValid g1 ~?= True

gnotvalid1 :: Graph Int
gnotvalid1 = Graph { nodes = fromList [1, 2, 3, 4, 5], 
                     edges = fromList [Edge 1 2, Edge 2 1, Edge 2 3, Edge 2 5, Edge 5 4]
                   }

test_isValid2 :: Test
test_isValid2 = isValid gnotvalid1 ~?= True

gnotvalid2 :: Graph Int
gnotvalid2 = Graph { nodes = fromList [1, 2, 3, 4, 5], 
                     edges = fromList [Edge 1 2, Edge 2 3, Edge 2 5, Edge 5 4, Edge 4 6]
                   }

test_isValid3 :: Test
test_isValid3 = isValid gnotvalid2 ~?= False

gciclico :: Graph Int
gciclico = Graph { nodes = fromList [1, 2, 3, 4, 5], 
                   edges = fromList [Edge 1 2,  Edge 2 5, Edge 3 2, Edge 4 3, Edge 5 4]
                 }

test_isDAG1 :: Test
test_isDAG1 = isDAG gciclico ~?= False

test_isDAG2 :: Test
test_isDAG2 = isDAG g1 ~?= True

gnotforest :: Graph Int
gnotforest = Graph { nodes = fromList [1, 2, 3, 4, 5, 6], 
                     edges = fromList [Edge 2 1, Edge 3 2, Edge 3 4, Edge 4 6, Edge 5 4]
                   }

test_isForest1 :: Test
test_isForest1 = isForest gnotforest ~?= False

gforest :: Graph Int
gforest = Graph { nodes = fromList [1, 2, 3, 4], 
                  edges = fromList [Edge 1 2, Edge 2 3, Edge 3 4]
                }

test_isForest2 :: Test
test_isForest2 = isForest gforest ~?= True

test_isForest3 :: Test
test_isForest3 = isForest gempty ~?= True 

gsubg2 :: Graph Int
gsubg2 = Graph { nodes = fromList [1, 2, 3], 
                 edges = fromList [Edge 1 2, Edge 2 3]
               }

gnotsub2 :: Graph Int
gnotsub2 = Graph { nodes = fromList [1, 2], 
                   edges = fromList [Edge 1 2, Edge 2 7]
                 }

test_isSubGraphOf1 :: Test
test_isSubGraphOf1 = isSubgraphOf gsubg2 g1 ~?= True

test_isSubGraphOf2 :: Test
test_isSubGraphOf2 = isSubgraphOf gnotsub2  g1 ~?= False

test_adj1 :: Test
test_adj1 = adj g1 2 ~?= fromList [Edge 2 3, Edge 2 5]

test_adj2 :: Test
test_adj2 = adj g1 4 ~?= fromList []

test_transpose1 :: Test
test_transpose1 = transpose g1 ~?= Graph {nodes = fromList [1, 2, 3, 4, 5], edges = fromList [Edge 2 1, Edge 3 2, Edge 5 2, Edge 4 5]}

test_transpose2 :: Test
test_transpose2 = transpose gempty ~?= gempty

gunion1 :: Graph Int
gunion1 = Graph { nodes = fromList [1, 2, 3], 
                  edges = fromList [Edge 2 1, Edge 2 3]
                }

gunion2 :: Graph Int
gunion2 = Graph { nodes = fromList [3, 5, 6, 7],
                  edges = fromList [Edge 3 5, Edge 5 6, Edge 7 3]
                }

test_union1 :: Test
test_union1 = union gunion1 gunion2 ~?= Graph { nodes = fromList [1, 2, 3, 5, 6, 7],
                                                edges = fromList [Edge 2 1, Edge 2 3, Edge 3 5, Edge 5 6, Edge 7 3]
                                              }

test_union2 :: Test
test_union2 = union gunion1 gempty ~?= gunion1

greach :: Graph Int
greach = Graph { nodes = fromList [1, 2, 3, 4, 5, 6, 7], 
                 edges = fromList [Edge 2 1, Edge 2 3, Edge 2 4, Edge 2 5, Edge 4 7]
               }

test_reachable1 :: Test
test_reachable1 = reachable greach 2 ~?= fromList [1, 2, 3, 4, 5, 7]

test_reachable2 :: Test
test_reachable2 = reachable greach 7 ~?= fromList [7]

p1 :: Path Int
p1 = [Edge 2 4, Edge 4 7]

pnotpath :: Path Int
pnotpath = [Edge 4 1]

test_isPathOf1 :: Test
test_isPathOf1 = isPathOf p1 greach ~=? True

test_isPathOf3 :: Test
test_isPathOf3 = isPathOf pnotpath greach ~?= False

g2 :: Graph Int
g2 = Graph { nodes = fromList [1, 2, 3, 4, 5, 6, 7], 
             edges = fromList [Edge 1 2, Edge 1 3, Edge 1 6, Edge 2 4, Edge 3 5, Edge 4 5]
           }

test_path1 :: Test
test_path1 = path g2 1 5 ~=? Just [Edge 1 3, Edge 3 5]

test_path2 :: Test
test_path2 = path g2 1 7 ~?= Nothing

g3 :: Graph Int
g3 = Graph { nodes = fromList [1, 2, 3, 4, 5, 6, 7, 8],
             edges = fromList [Edge 1 4, Edge 1 6, Edge 2 4, Edge 3 2, Edge 4 7, Edge 7 5, Edge 8 3, Edge 8 5]
           }

set2 :: Set Int
set2 = fromList[1, 3, 8]

test_bft1 ::Test
test_bft1 = bft g3 set2 ~?= Graph { nodes = fromList [1, 2, 3, 4, 5, 6, 7, 8],
                                    edges = fromList [Edge 2 3, Edge 4 1, Edge 5 8, Edge 6 1, Edge 7 4]
                                  }

set3 :: Set Int
set3 = fromList [6]

test_bft2 :: Test
test_bft2 = bft g3 set3 ~?= Graph { nodes = fromList [6],
                                    edges = fromList []
                                  }

g4 :: Graph Int
g4 = Graph { nodes = fromList [1, 2, 3, 4, 5, 6],
             edges = fromList [Edge 1 4, Edge 3 1, Edge 3 4, Edge 5 2, Edge 5 3, Edge 6 3, Edge 6 4]
           }

test_topo1 :: Test
test_topo1 = topo g4 ~?= [fromList [5, 6], fromList [2, 3], fromList[1], fromList[4]]

test_topo2 :: Test
test_topo2 = topo gempty ~?= []

guno :: Graph Int
guno = Graph { nodes = fromList [1],
               edges = fromList []
             }

test_topo3 :: Test
test_topo3 = topo guno ~?= [fromList[1]]

--
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--

-- Gerador de grafos aleatórios
instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
  arbitrary = aux `suchThat` isValid
    where aux = do ns <- arbitrary
                   es <- geraArestas (length ns) ns ns
                   return $ Graph {nodes = fromList ns, edges = fromList es}

geraArestas :: Int -> [v] -> [v] -> Gen[Edge v]
geraArestas n l [] = return []
geraArestas n l (h:t) = do x <- gera n l h
                           y <- geraArestas n l t
                           return $ (x++y)

gera :: Int -> [v] -> v -> Gen[Edge v]
gera n l v = do q <- choose(0, n)
                x <- shuffle l
                let ai = take q x
                es <- arestas v ai
                return $ es

arestas :: v -> [v] -> Gen[Edge v]
arestas v [] = return []
arestas v (h:t) = do let e = Edge {source = v, target = h}
                     es <- arestas v t
                     return $ (e:es)
 
prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Random v, Num v, Arbitrary v) => Gen (DAG v)
dag = aux `suchThat` isDAG
  where aux = do ns <- arbitrary
                 es <- geraArestasDAG (length ns) ns
                 return $ Graph {nodes = fromList ns, edges = fromList es}

geraArestasDAG :: Int -> [v] -> Gen[Edge v]
geraArestasDAG n [] = return []
geraArestasDAG n (h:t) = do x <- gera n (h:t) h
                            y <- geraArestasDAG (n-1) t
                            return $ (x++y)

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Random v, Num v, Arbitrary v) => Gen (Forest v)
forest = aux `suchThat` isForest
  where aux = do ns <- arbitrary
                 es <- geraArestasForest (length ns) ns
                 return $ Graph {nodes = fromList ns, edges = fromList es}

geraArestasForest :: Int -> [v] -> Gen[Edge v]
geraArestasForest n [] = return []
geraArestasForest n (h:t) = do x <- geraForest n (h:t) h
                               y <- geraArestasForest (n-1) t
                               return $ (x++y)

geraForest :: Int -> [v] -> v -> Gen[Edge v]
geraForest n l v = do q <- choose(0, 1)
                      x <- shuffle l
                      let ai = take q x
                      es <- arestas v ai
                      return $ es

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--Para as propriedades de isPathOf
instance (Arbitrary v) => Arbitrary (Edge v) where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 return $ Edge{ source = x, target = y}

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--
      
prop_adj :: Property
prop_adj = forAll(arbitrary :: Gen(Graph Int)) $ \g -> (forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g)

prop_swap :: Property
prop_swap = forAll(arbitrary :: Gen(Graph Int)) $ \g -> if (length(edges g) > 0) then property $ forAll(elements $ elems $ edges g) $ \v -> swap(swap v) == v
                                                                                 else label "Trivial" True

prop_isEmpty :: Property
prop_isEmpty = forAll(arbitrary :: Gen(Graph Int)) $ \g -> if (isEmpty g == True) then property $ length(nodes g) == 0
                                                                                  else label "Não Vazio" True

prop_isValid :: Property
prop_isValid = forAll(arbitrary :: Gen(Graph Int, Graph Int)) $ \(g1,g2) -> isValid(union g1 g2) == ((isValid g1) && (isValid g2))

prop_isSubgraphOf :: Property
prop_isSubgraphOf = forAll(arbitrary :: Gen(Graph Int, Graph Int)) $ \(g1,g2) -> if ((isSubgraphOf g1 g2) == True) then ((union g1 g2) == g2)
                                                                                                                   else True

prop_transpose :: Property
prop_transpose = forAll(arbitrary :: Gen(Graph Int)) $ \g -> and[prop1_transpose g]

prop1_transpose g = isValid(transpose g)

insertEdge :: Ord v => Edge v -> Set(Edge v) -> Set(Edge v)
insertEdge v set = v `Set.insert` set

prop_transpose2 :: Property
prop_transpose2 = forAll(arbitrary :: Gen(Graph Int)) $ \g -> if (length(edges g) > 0) then property $ forAll(elements $ elems $ edges g) $ \v -> (insertEdge (swap v) Set.empty) `isSubsetOf` edges (transpose g)
                                                                                         else label "Trivial" True    

prop_union :: Property
prop_union = forAll(arbitrary :: Gen(Graph Int, Graph Int)) $ \(g1,g2) -> forAll(elements $ elems $ nodes g1) $ \v -> adj g1 v `isSubsetOf` edges (union g1 g2)

prop_bft1 :: Property
prop_bft1 = forAll(arbitrary :: Gen(Graph Int, Set Int)) $ \(g,s) -> if(s `isSubsetOf` (nodes g)) then edges (bft g s) `isSubsetOf` edges (transpose g)
                                                                                                 else True

prop_bft2 :: Property
prop_bft2 = forAll(arbitrary :: Gen(Graph Int, Set Int)) $ \(g,s) -> and [prop1_bft2 g s]

prop1_bft2 g s = isForest(bft g s)

prop_reachable :: Property
prop_reachable = forAll(arbitrary :: Gen(Graph Int, Int)) $ \(g,v) -> and [prop1_reachable g v, prop2_reachable g v, prop3_reachable g v]

prop1_reachable g v = if(isEmpty g == False) then (length(reachable g v) <= length(nodes g))
                                             else True 

prop2_reachable g v = if (verticePertenceAoGrafo g v == True) then (reachable g v) `isSubsetOf` (nodes g)
                                                              else True

prop3_reachable g v = if (verticePertenceAoGrafo g v == False) then ((length(reachable g v) == 1) && (toList(reachable g v)) == [v])
                                                               else True

verticePertenceAoGrafo :: Eq v => Graph v -> v -> Bool
verticePertenceAoGrafo g v = aux (toList(nodes g)) v
  where aux :: Eq v => [v] -> v -> Bool
        aux [] _ = False
        aux (h:t) v = if (h == v) then True
                                  else aux t v

prop_reachable2 :: Property
prop_reachable2 = forAll(arbitrary :: Gen(Graph Int, Graph Int, Int)) $ \(g1, g2, v) -> (isEmpty g1 ==  False) && (isEmpty g2 == False) ==> collect(length(reachable g1 v)) $ length (reachable g1 v) <= length (reachable (union g1 g2) v)

prop_isPathOf :: Property
prop_isPathOf = forAll(arbitrary :: Gen(Graph.Path Int, Graph Int)) $ \(p,g) -> and [prop1_isPathOf p g, prop2_isPathOf p g, prop3_isPathOf p g]

prop1_isPathOf p g = if (isPathOf p g == True) then length p <= length (edges g)
                                               else True

prop2_isPathOf p g | isPathOf p g = (fromList p) `isSubsetOf` edges g
                   | otherwise =  True

invertePath :: Path v -> Path v
invertePath [] = []
invertePath (h:t) = (swap h):(invertePath t)

prop3_isPathOf p g = if (isPathOf p g == True) then (isPathOf (invertePath p) (transpose g))
                                       else True

prop_isPathOf2 :: Property
prop_isPathOf2 = forAll(arbitrary :: Gen(Graph.Path Int, Graph Int, Graph Int)) $ \(p, g1, g2) -> if (isPathOf p g1) then property $ (fromList p) `isSubsetOf` edges (union g1 g2)
                                                                                                                     else label "Trivial" True

prop_path :: Property
prop_path = forAll(arbitrary :: Gen(Graph Int, Int, Int)) $ \(g, o, d) -> and [prop1_path g o d, prop2_path g o d]

prop1_path g o d = if (path g o d /= Nothing) then adj g o `isSubsetOf` edges g
                                              else True

prop2_path g o d = if (path g o d /= Nothing) then length (retiraPath (path g o d)) <= length (edges g)
                                              else True

retiraPath :: Maybe(Path v) -> Path v
retiraPath Nothing = []
retiraPath (Just p) = p

somaLengths :: [Set v] -> Int
somaLengths [] = 0
somaLengths (h:t) = (length h) + somaLengths t

prop_topo :: Property
prop_topo = forAll(dag :: Gen(DAG Int)) $ \g -> collect(length(nodes g)) $ length(nodes g) == somaLengths(topo g) 
             
prop_topo2 :: Property
prop_topo2 = forAll(dag :: Gen(DAG Int)) $ \g -> if (isEmpty g == False) then property $ (apareceNosTarget (toList(head(topo g))) (toList((edges g))) []) == True     
                                                                        else label "Trivial" True
apareceNosTarget :: Eq v => [v] -> [Edge v] -> [Edge v] -> Bool
apareceNosTarget [] _ _ = True
apareceNosTarget (h:t) [] l = apareceNosTarget t l []
apareceNosTarget (h:t) (x:xs) l | h /= (target x)= apareceNosTarget t xs (x:l)
                                | otherwise = False

main = do runTestTT $ TestList [test_isSubGraphOf1, test_isSubGraphOf2, test_isPathOf1, test_isPathOf3, test_isEmpty1, test_isEmpty2, test_isValid1, test_isValid2, test_isValid3, test_isForest1, test_isForest2, test_isForest3, test_isDAG1, test_isDAG2, test_swap, test_path2, test_path1, test_reachable2, test_reachable1, test_union2, test_union1, test_transpose1, test_transpose2, test_adj1, test_adj2, test_bft1, test_bft2, test_topo1, test_topo2, test_topo3]
          quickCheck prop_adj
          quickCheck prop_swap
          quickCheck prop_isEmpty
          quickCheck prop_isValid
          quickCheck prop_isSubgraphOf
          quickCheck prop_isPathOf
          quickCheck prop_isPathOf2
          quickCheck prop_valid
          quickCheck prop_forest
          quickCheck prop_transpose
          quickCheck prop_transpose2
          quickCheck prop_union
          quickCheck prop_bft1
          quickCheck prop_bft2
          quickCheck prop_reachable
          quickCheck prop_reachable2
          quickCheck prop_path
          quickCheck prop_topo2
          quickCheck prop_topo
          quickCheck prop_dag
