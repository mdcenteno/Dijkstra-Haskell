-- chamada do programa com entrada adequada para o usuário
-- Casos de teste:
-- dijkstra 1 4 [(1,2,1),(1,3,1),(2,3,1),(2,4,1),(3,4,1)] : 2
-- dijkstra 1 4 [(1,2,5),(1,3,1),(2,3,1),(2,4,5),(3,4,1)] : 2
-- dijkstra 1 4 [(1,2,1),(1,3,5),(2,3,1),(2,4,5),(3,4,1)] : 3
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
varreArestas visitar@(v:vs) [] chegada grafo = 
        dijkstraCore (posicionarMenorNaoVisitadoNaFrente((v_id v,v_peso v,True):vs)) chegada grafo
varreArestas visitar@(v:vs) arestas@(ar:ars) chegada grafo
    | v_id v == origem ar 
	  && not (or (map (\ y -> (destino ar) == v_id y) visitar))
	    = varreArestas (visitar ++ [(destino ar, v_peso v + peso ar, False)]) ars chegada grafo  
    | v_id v == origem ar 
	  && or (map (\ y -> (destino ar) == v_id y && visitado y /= True) visitar)
	  && v_peso v + peso ar < verificaPeso (destino ar) visitar 	
		= varreArestas (atualizarPeso (destino ar, v_peso v + peso ar, visitado v) visitar) ars chegada grafo
    | otherwise = varreArestas visitar ars chegada grafo

atualizarPeso :: (Int, Int, Bool) -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
atualizarPeso novovalor lista = atualizarPesoCore [] novovalor lista
atualizarPesoCore :: [(Int, Int, Bool)] -> (Int, Int, Bool) -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
atualizarPesoCore verificados novovalor porverificar@(p:ps)
    | v_id(novovalor) == v_id p = verificados ++ [novovalor] ++ ps
    | otherwise = atualizarPesoCore (verificados ++ [p]) novovalor ps

-- verificaPeso 2 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)] : 5
-- verificaPeso 4 [(1,2,False),(2,5,False),(3,3,False),(4,1,True)] : 1000000
verificaPeso :: Int -> [(Int, Int, Bool)] -> Int
verificaPeso procurado lista@(l:ls)
    | lista == [] = iNF
    | procurado == v_id l && visitado l /= True = v_peso l
    | otherwise = verificaPeso procurado ls
	
-- posicionarMenorNaoVisitadoNaFrente [(1,1,True),(1,5,False),(1,3,False),(1,2,False),(1,4,False),(1,9,True)]
-- : [(1,2,False),(1,9,True),(1,4,False),(1,3,False),(1,5,False),(1,1,True)]
posicionarMenorNaoVisitadoNaFrente :: [(Int, Int, Bool)] -> [(Int, Int, Bool)]
posicionarMenorNaoVisitadoNaFrente lista = posicionarMenorNaoVisitadoNaFrenteCore [] (head(lista)) (tail(lista))
posicionarMenorNaoVisitadoNaFrenteCore :: [(Int, Int, Bool)] -> (Int, Int, Bool) -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
posicionarMenorNaoVisitadoNaFrenteCore verificados candidato [] = candidato:verificados
posicionarMenorNaoVisitadoNaFrenteCore verificados candidato porverificar@(p:ps)    
    | visitado p == True 
	    = posicionarMenorNaoVisitadoNaFrenteCore (p:verificados) candidato ps
    | visitado(candidato) == True 
        = posicionarMenorNaoVisitadoNaFrenteCore (candidato:verificados) p ps
    | visitado p == False && v_peso(candidato) < v_peso p 
        = posicionarMenorNaoVisitadoNaFrenteCore (p:verificados) candidato ps
    | otherwise = posicionarMenorNaoVisitadoNaFrenteCore (candidato:verificados) p ps

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
