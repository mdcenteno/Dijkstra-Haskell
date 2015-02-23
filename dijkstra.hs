
primeiro :: (a, b, c) -> a
primeiro (x, _, _) = x
segundo :: (a, b, c) -> b
segundo (_, y, _) = y
terceiro :: (a, b, c) -> c
terceiro (_, _, z) = z

-- Vertice
v_id :: (Int, Int, Bool) -> Int
v_id vertice = primeiro(vertice)
v_peso :: (Int, Int, Bool) -> Int
v_peso vertice = segundo(vertice)
visitado  :: (Int, Int, Bool) -> Bool
visitado vertice = terceiro(vertice)

-- Aresta
origem :: (Int,Int,Int) -> Int
origem aresta = primeiro(aresta)
destino :: (Int,Int,Int) -> Int
destino aresta = segundo(aresta)
peso :: (Int,Int,Int) -> Int
peso aresta = terceiro(aresta)


iNF = 1000000 -- Infinito, inatingível.

-- chamada do programa com entrada adequada para o usuário
dijkstra :: Int -> Int -> [(Int,Int,Int)] -> Int
dijkstra partida chegada grafo = dijkstraCore [(partida,0,False)] chegada grafo

-- programa propriamente dito, com entrada formatada para o algorítmo
-- testa condições de parada e chama a varredura dos vértices
dijkstraCore :: [(Int, Int, Bool)] -> Int -> [(Int,Int,Int)] -> Int
dijkstraCore visitar@(v:vs) chegada grafo    
    | v_id v == chegada = v_peso v -- Menor peso
	| visitado v /= False = iNF -- Inatingível
    | otherwise = varreArestas visitar grafo chegada grafo

-- percorre a arestas com origem no vertice a se verificar e alimenta a fila de arestas a visitar.
varreArestas :: [(Int, Int, Bool)] -> [(Int,Int,Int)] -> Int -> [(Int,Int,Int)] -> Int
varreArestas visitar [] chegada grafo = 
	    dijkstraCore (posicionarMenorNaFrente(atualizarVisitadoPrimeiro(visitar))) chegada grafo
varreArestas visitar@(v:vs) arestas@(ar:ars) chegada grafo
    | v_id v == origem ar && estaEm (destino ar) visitar == False = 
    	    varreArestas (visitar ++ [(destino ar, v_peso v + peso ar, False)]) ars chegada grafo  
    | v_id v == origem ar && estaEmNaoVisitados (destino ar) visitar && v_peso v + peso ar < verificaPeso (destino ar) visitar =
	    varreArestas (atualizarPeso (destino ar, v_peso v + peso ar, visitado v) visitar) ars chegada grafo
    | otherwise = varreArestas visitar ars chegada grafo

-- dijkstra 1 2 [(1,2,8),(1,3,1),(3,2,1)]
-- 2,2,false;1,0,t;3,1,t;
atualizarPeso :: (Int, Int, Bool) -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
atualizarPeso novovalor lista = atualizarPesoCore [] novovalor lista
atualizarPesoCore :: [(Int, Int, Bool)] -> (Int, Int, Bool) -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
atualizarPesoCore verificados novovalor porverificar@(p:ps)
    | v_id(novovalor) == v_id p = verificados ++ [novovalor] ++ ps
    | otherwise = atualizarPesoCore (verificados ++ [p]) novovalor ps
	
atualizarVisitadoPrimeiro :: [(Int, Int, Bool)] -> [(Int, Int, Bool)]
atualizarVisitadoPrimeiro lista@(l:ls) = (v_id l,v_peso l,True):ls

-- verificaPeso 2 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)]
-- 5
-- verificaPeso 3 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)]
-- 3
-- verificaPeso 4 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)]
-- 1000000
-- verificaPeso 5 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)]
-- 1000000
verificaPeso :: Int -> [(Int, Int, Bool)] -> Int
verificaPeso procurado lista@(l:ls)
    | lista == [] = iNF
	| procurado == v_id l && visitado l /= True = v_peso l
	| otherwise = verificaPeso procurado ls
	
-- posicionarMenorNaFrente [(1,1,True),(1,5,False),(1,3,False),(1,2,False),(1,4,False),(1,9,True)]
-- [(1,2,False),(1,9,True),(1,4,False),(1,3,False),(1,5,False),(1,1,True)]
-- posicionarMenorNaFrente [(1,1,True),(1,5,True),(1,3,True),(1,2,True),(1,4,True),(1,9,True)]
-- [(1,1,True),(1,9,True),(1,4,True),(1,2,True),(1,3,True),(1,5,True)]
-- posicionarMenorNaFrente [(1,1,True),(1,5,True),(1,3,True),(1,2,True),(1,4,True),(1,9,False)]
-- [(1,9,False),(1,1,True),(1,4,True),(1,2,True),(1,3,True),(1,5,True)]
-- posicionarMenorNaFrente [(1,1,False),(1,5,True),(1,3,True),(1,2,True),(1,4,True),(1,9,True)]
-- [(1,1,False),(1,9,True),(1,4,True),(1,2,True),(1,3,True),(1,5,True)]
-- posicionarMenorNaFrente [(1,8,False),(1,5,True),(1,3,True),(1,2,False),(1,4,True),(1,9,True)]
-- [(1,2,False),(1,9,True),(1,4,True),(1,8,False),(1,3,True),(1,5,True)]
posicionarMenorNaFrente :: [(Int, Int, Bool)] -> [(Int, Int, Bool)]
posicionarMenorNaFrente lista = posicionarMenorNaFrenteCore [] (head(lista)) (tail(lista))
posicionarMenorNaFrenteCore :: [(Int, Int, Bool)] -> (Int, Int, Bool) -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
posicionarMenorNaFrenteCore verificados candidato [] = candidato:verificados
posicionarMenorNaFrenteCore verificados candidato porverificar@(p:ps)    
	| visitado p == True = 
		posicionarMenorNaFrenteCore (p:verificados) candidato ps
	| visitado(candidato) == True = 
		posicionarMenorNaFrenteCore (candidato:verificados) p ps
    | visitado p == False && 
	  v_peso(candidato) < v_peso p = 
		posicionarMenorNaFrenteCore (p:verificados) candidato ps
    | otherwise = 
		posicionarMenorNaFrenteCore (candidato:verificados) p ps

estaEm :: Int -> [(Int, Int, Bool)] -> Bool
estaEm _ [] = False
estaEm procurado lista@(l:ls) 
    | procurado == v_id l = True
    | otherwise = estaEm(procurado) ls
	
-- estaEmNaoVisitados 3 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)]
-- True
-- estaEmNaoVisitados 4 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)]
-- False
-- estaEmNaoVisitados 5 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)]
-- False
estaEmNaoVisitados :: Int -> [(Int, Int, Bool)] -> Bool
estaEmNaoVisitados _ [] = False
estaEmNaoVisitados procurado lista@(l:ls)
    | procurado == v_id l && visitado l /= True = True
    | otherwise = estaEmNaoVisitados procurado ls 

atualizarVisitado :: (Int, Int, Bool) -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
atualizarVisitado novovalor lista = atualizarVisitadoCore [] novovalor lista
atualizarVisitadoCore :: [(Int, Int, Bool)] -> (Int, Int, Bool) -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
atualizarVisitadoCore verificados@(v:vs) novovalor porverificar@(p:ps)
    | v_id(novovalor) == v_id p = verificados ++ [(v_id novovalor, v_peso novovalor, True)] ++ ps
    | otherwise = atualizarVisitadoCore (p:verificados) novovalor ps

	
