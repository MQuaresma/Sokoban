{-|
Module : Main
Descrição : Módulo de Haskell referente à primeira tarefa do projeto de LI.
Copyright : Miguel Quaresma <miguelquaresma97@gmail.com>
			João Nogueira	<joaonogueira097@hotmail.com>
			
Conjunto de funções responsáveis por verificar se o mapa e as coordenadas recebidos como input respeitam os requisitos estabelecidos, devolvendo OK caso
estes sejam válidos ou, caso contrário, o número da linha onde ocorre o primeiro erro.
-}

module Main where

import qualified Data.Text as T
import Data.List
import Data.Char
import System.Directory



main = do
	inp <- getContents
	putStr (outStr (tarefa1 (inStr inp)))

inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)

tarefa1 :: [String] -> [String]
tarefa1 linhas = [verificaTudo (dividemapa linhas)]

-- | Corre todos os testes de um determinado diretório automaticamente, recorrendo à função correTeste
correTestes :: IO ()
correTestes = do
      files1 <- getDirectoryContents "/home/miguel/Documentos/Disciplinas/Laboratórios_de_Informática/li1g030/tests/T1/"
      let inputs1 = map ("/home/miguel/Documentos/Disciplinas/Laboratórios_de_Informática/li1g030/tests/T1/" ++) $ filter (isSuffixOf ".in") files1 
      mapM_ (correTeste tarefa1) inputs1


-- | Corre um teste e compara o resultado obtido com o esperado
correTeste :: ([String] -> [String]) -> String -> IO ()
correTeste tarefa input = do
    -- nome do ficheiro
    let nome = reverse $ drop 3 $ reverse input
    -- texto do mapa
    inp <- readFile input
    -- resultado da tarefa
    let o = outStr (tarefa (inStr inp))
    -- resultado esperado
    esp <- readFile (nome ++ ".out")
    putStr ("[" ++ nome ++ "]: ")
    if (o == esp)   -- compara resultados
    then putStrLn "OK"
    else do
        putStrLn "FALHOU"
        putStr esp
        putStrLn o



-- | Divide o ficheiro em duas partes a primeira contendo o mapa e a segunda as coordenadas
dividemapa :: [String] -> ([String], [String]) 
dividemapa [] = ([], [])
dividemapa l = splitAt (aux l) l
	where 
		aux :: [String] -> Int
		aux [] = 0
		aux (x:xs) = if(aux2 x) then 1 + aux xs else 0  
		
		aux2 :: String -> Bool
		aux2 [] = False
		aux2 (x:xs) = if(ord x == 35) then True else aux2 xs  


-- |Faz check de todos os erros (mapa e coordenadas) ao mesmo tempo e devolve a primeira linha onde há erro ou devolve OK caso não haja nenhum
verificaTudo :: ([String], [String]) -> String  
verificaTudo l = if(comparaErros l1 == 0) then if(verificaCords l2 nro lncd (reverse l1) (length l1, length (head l1)) == 0) then if(nrocaixas /= contaCaixas l1) then if(nrocaixas < contaCaixas l1) then show(length l1 + nrocaixas + 2) 
																																									   else show(lncd + (percorreCaixas 1 (contaCaixas l1))) 
																																  else "OK"	 
								               else show (verificaCords l2 nro lncd (reverse l1) (length l1, length (head l1)))  
			     else show (comparaErros l1)
				
		where
			nro = processacoordenadas (removeInv l2)
			lncd = (length l1) + 1
			l1 = fst l
			l2 = snd l
			nrocaixas = (length (processacoordenadas (removeInv l2))) - 1


-- | Executa todos os testes de verificação do mapa
verificaMapa :: [String] -> Int 
verificaMapa (h:t) = if(contornosCheck h) then if(verificaInterior 2 (take (length t - 1) t) == 0) then if(contornosCheck (last t) && length (last t) == length h) then 0
																										else length (h:t)
										  	   else verificaInterior 2 (take (length t - 1) t)           					 
		    		 else 1

-- | Compara as linhas onde ha erros e devolve a linha que vem primeiro
comparaErros :: [String] -> Int 
comparaErros l |verificaComprimento 2 l /= 0 && verificaMapa l /= 0 = min (verificaComprimento 2 l) (verificaMapa l)
			   |verificaComprimento 2 l == 0 && verificaMapa l /= 0 = verificaMapa l
			   |verificaComprimento 2 l /= 0 && verificaMapa l == 0 = verificaComprimento 2 l
			   |otherwise = 0 

-- | Verifica se ha algum caracter do mapa que não seja (.), (#), (espaço)
verificaInterior :: Int -> [String] -> Int 
verificaInterior _ [] = 0
verificaInterior n (h:t) = if(aux1 h && verificarLados h) then verificaInterior (n+1) t else n 
	where 
		aux1 :: String -> Bool
		aux1 [] = True
		aux1 (x:xs) = if(ord x == 32 || ord x == 46 || ord x == 35) then aux1 xs else False


-- | Verifica se todas linhas do mapa têm o mesmo comprimento
verificaComprimento :: Int -> [String] -> Int
verificaComprimento _ [] = 0
verificaComprimento n [x,y] = if(length x == length y) then 0 else n
verificaComprimento n (h:x:t) = if(length h == length x) then verificaComprimento (n+1) (x:t) else n


-- | Verifica se todos os caracteres da primeira/ultima linha sao cardinais (#)
contornosCheck :: String -> Bool 
contornosCheck [] = True
contornosCheck (x:xs) = if(ord x == 35) then contornosCheck xs else False


-- | Verifica se os contornos laterais são cardinais (#)
verificarLados :: String -> Bool 
verificarLados (h:t) = if(ord h == 35 && ord (last t) == 35) then True else False


-- | Converte a lista de coordenadas, ainda em lista de /strings/, numa lista de pares de /Int/s 
processacoordenadas :: [String] -> [(Int, Int)]
processacoordenadas [] = []
processacoordenadas (h:t) = (read x, read y) : processacoordenadas t 
		where
			[x, y] = words h



-- |Devolve o primeiro par de coordenadas onde há erro
verificaCoordenadas :: [(Int,Int)] ->  Int -> [String] -> (Int,Int) -> Int
verificaCoordenadas crd pos mp size |v1 /= 0 && v2 /= 0 && v3 /= 0 = min v1 (min v2 v3)
								 	|v1 == 0 && v2 /= 0 && v3 /=0 = min v2 v3
								 	|v1 /= 0 && v2 /= 0 && v3 == 0 = min v1 v2
								 	|v1 /= 0 && v2 == 0 && v3 /= 0 = min v1 v3
								 	|v1 == 0 && v2 == 0 && v3 /= 0 = v3
								 	|v1 == 0 && v2 /= 0 && v3 == 0 = v2
								 	|v1 /= 0 && v2 == 0 && v3 == 0 = v1
								 	|otherwise = 0
	where													  
		v1 = dentroTabuleiro size crd pos 
		v2 = posicaoValida mp pos crd
		v3 = posicoesRepetidas crd (pos + 1)


verificaCords :: [String] -> [(Int,Int)] ->  Int -> [String] -> (Int,Int) -> Int
verificaCords cordS cords pos mp size |testII == 0 = testI
									  |testI == 0 = testII
									  |otherwise = min testI testII
	where
		testI = verificaCoordenadas cords pos mp size
		testII = devolveInv cordS (length mp + 1)


-- | Verifica se as coordenadas correspondem a pontos dentro do tabuleiro
dentroTabuleiro :: (Int,Int) -> [(Int, Int)] -> Int -> Int
dentroTabuleiro _ [] _ = 0
dentroTabuleiro c (h:t) n = if(aux c h) then dentroTabuleiro c t (n+1) else n
	where 
	 aux :: (Int, Int) -> (Int, Int) -> Bool -- ^ O primeiro par de inteiros refere-se ao tamanho do mapa (altura e largura)
	 aux (l1, l2) (x, y) = (x >= 0 && x<= l2 && y>= 0 && y <= l1)


-- |Verifica se as coordenadas coincidem com partes válidas do mapa
posicaoValida :: [String] -> Int -> [(Int,Int)] -> Int
posicaoValida _ _ [] = 0 
posicaoValida tab n (h:t) = if(ord (percorreLinhas tab 0 (l1,l2)) == 32) then posicaoValida tab (n+1) t else n 
	where
		(l1,l2) = h
		percorreLinhas :: [String] -> Int -> (Int,Int) -> Char
		percorreLinhas [] _ _ = '#'
		percorreLinhas (h:t) li (p1, p2) |li == p2 = percorreColunas h 0 (p1,p2)
										 |otherwise = percorreLinhas t (li+1) (p1, p2)
		
		percorreColunas :: String -> Int -> (Int, Int) -> Char
		percorreColunas [] _ _ = '#'
		percorreColunas (x:xs) c (p1,p2) |c == p1 = x 
										 |otherwise = percorreColunas xs (c+1) (p1,p2)

-- | Procura por coordenadas repetidas na lista e devolve o primeiro par que se repita
posicoesRepetidas :: [(Int, Int)] -> Int -> Int
posicoesRepetidas [] _ = 0
posicoesRepetidas [x] _ = 0
posicoesRepetidas (h:t) c = if(repete c h t == 0) then posicoesRepetidas t (c+1) else repete c h t 
	where
		repete :: Int -> (Int, Int) -> [(Int, Int)] -> Int
		repete _ _ [] = 0
		repete n (l1, l2) (z:zs) = if(l1 == fst z && l2 == snd z) then n else repete (n+1) (l1, l2) zs


-- | Conta o número de pontos (.) no mapa obtendo assim o número de caixas esperadas 
contaCaixas :: [String] -> Int 
contaCaixas [] = 0
contaCaixas (h:t) = aux h + contaCaixas t
	where 
		aux :: String -> Int
		aux [] = 0
		aux (h:t) = if(ord h == 46) then 1 + aux t else aux t 


-- |Devolve a linha onde está a primeira coordenada a mais
percorreCaixas :: Int -> Int -> Int
percorreCaixas nr m = if(nr == m + 1) then nr else percorreCaixas (nr + 1) m  


-- |Remove a coordenadas inválidas (com um so nro; com caracteres que não são nros ou linhas vazias)
removeInv :: [String] -> [String]
removeInv [] = []
removeInv (x:xs) | x==""= removeInv xs
                 | length (words x) /= 2 = removeInv xs
                 | remaux x == False = removeInv xs
                 | otherwise = x : removeInv xs

remaux :: String -> Bool
remaux [] = True
remaux (h:t) = if isDigit h || h == ' ' then remaux t else False

-- | Percorre as coordenadas ainda em /string/ e devolve a linha, caso haja, do primeiro par com caracteres inválidos
devolveInv :: [String] -> Int -> Int
devolveInv [] _ = 0
devolveInv (x:xs) n | x==""= n
                 	| length (words x) /= 2 = n
                 	| devolveAux x == False = n
                 	| otherwise = devolveInv xs (n+1)

devolveAux :: String -> Bool
devolveAux [] = True
devolveAux (h:t) = if isDigit h || h == ' ' then devolveAux t else False