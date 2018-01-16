{-|
Module : Main
Descrição : Módulo de Haskell referente à quarta tarefa do projeto de LI.
Copyright : Miguel Quaresma <miguelquaresma97@gmail.com>
			João Nogueira	<joaonogueira097@hotmail.com>

Módulo responsável por realizar uma série de movimentos com boneco até que o mapa esteja completo ou que já não haja mais movimentos definidos, sendo que o output
corresponderá a FIM ou INCOMPLETO , consoante o estado do mapa, seguido do número de movimentos válidos realizados pelo boneco.
-}

module Main where

import qualified Data.Text as T
import Data.List
import Data.Char
import System.Directory


main = do inp <- getContents
          putStr (outStr (tarefa4 (inStr inp)))


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

tarefa4 :: [String] -> [String]
tarefa4 linhas = [devolveFinal h muv mapaF 0]
	where
		(parte1, parte2) = dividemapa linhas
		cords = processacoordenadas (removeInv parte2) -- Remove o caracter referente ao movimento e devolve as coordenadas
		h = head cords -- Devolve as coordenadas do boneco
		t = tail cords -- Devolve as coordenadas das caixas
		muv = last (removeV parte2) 
		mapaF = colocaCaixas parte1 t -- Devolve o mapa com as caixas colocadas facilitando assim a verificação referente ao movimento do boneco


-- | Corre todos os testes de um determinado diretório automaticamente, recorrendo à função correTeste
correTestes :: IO ()
correTestes = do
      files4 <- getDirectoryContents "/home/miguel/Documentos/Disciplinas/Laboratórios_de_Informática/li1g030/tests/T4/"
      let inputs4 = map ("/home/miguel/Documentos/Disciplinas/Laboratórios_de_Informática/li1g030/tests/T4/" ++) $ filter (isSuffixOf ".in") files4 
      mapM_ (correTeste tarefa4) inputs4


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




-- | Separa o tabuleiro das coordenadas e da letra referente ao movimento do boneco
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


-- | Devolve FIM ou INCOMPLETO seguido do número de movimentos realizados pelo boneco
devolveFinal :: (Int, Int) -> String -> [String] -> Int -> String
devolveFinal _ [] _ m = "INCOMPLETO " ++ show(m) 
devolveFinal cb (h:t) mp m|movimentoValido mp h cbm = if(devolveCarater cbm mp == 'H' || devolveCarater cbm mp == 'I') then if(caixasF mpNovo) then "FIM " ++ show(m + 1) 
						   																									else devolveFinal cbm t mpNovo (m+1)
					                                  else devolveFinal cbm t mp (m+1)
					      |otherwise = devolveFinal cb t mp m  	                                                                         
	where
		cbm = move cb h
		cordCaixa = move cbm h
		mpNovo = colocaCaixa (removeCx mp cbm) cordCaixa

-- | Verifica se o movimento é valido
movimentoValido :: [String] -> Char -> (Int,Int) -> Bool
movimentoValido tab c par = percorreLinhas (reverse tab) par 
	where
		final = podeMexerCaixa (reverse tab) c par 
		percorreLinhas :: [String] -> (Int,Int) -> Bool
		percorreLinhas [] _ = False
		percorreLinhas (h:t) (p1, p2) |p2 == 0 = percorreColunas h p1
									  |otherwise = percorreLinhas t (p1, p2 - 1)
		
		percorreColunas :: String -> Int -> Bool
		percorreColunas [] _  = False
		percorreColunas (x:xs) p1 |p1 == 0 = if(x == 'I' || x == 'H') then final 
											 else if(x == '#') then False 
											 	  else True
								  |otherwise = percorreColunas xs (p1 - 1)


-- | Consoante o caracter presente no fim do ficheiro devolve a coordenada resultante de mover o boneco nessa direção
move :: (Int,Int) -> Char -> (Int, Int)
move (l1, l2) c |c == 'U' = (l1 , l2 + 1)
				|c == 'D' = (l1 , l2 - 1)
				|c == 'L' = (l1 - 1  , l2)
				|c == 'R' = (l1 + 1  , l2)
				|otherwise = (l1, l2)

-- | Coloca as caixas no tabuleiro, facilitando assim a validação do movimento do boneco
colocaCaixas :: [String] -> [(Int, Int)] -> [String]
colocaCaixas l [] = l
colocaCaixas l [z] = colocaCaixa l z 
colocaCaixas l (h:t) = colocaCaixas (colocaCaixa l h) t 

-- | Função auxiliar da função __colocaCaixas__ que coloca cada caixa individualmente
colocaCaixa :: [String] -> (Int,Int) -> [String]
colocaCaixa l (x,y) = reverse (percorreLinhas (reverse l) (x,y)) 
	where
		percorreLinhas :: [String] -> (Int, Int) -> [String]
		percorreLinhas (h:t) (p1, p2) = if(p2 == 0) then ((percorreColunas h  p1) : t) else (h : percorreLinhas t (p1, p2 - 1)) 

		percorreColunas :: String -> Int -> String
		percorreColunas [] _ = []
		percorreColunas (z:zs) n = if(n == 0) then if(z == '.') then 'I' : zs 
																else 'H' : zs 
											  else (z : percorreColunas zs (n-1))


-- | Verifica se a caixa que o boneco está a empurrar se pode mexer na direção pretendida
podeMexerCaixa :: [String] -> Char -> (Int, Int) -> Bool
podeMexerCaixa tab c cx = percorreLinhas tab nextMove
	where
		nextMove = move cx c
		percorreLinhas :: [String] -> (Int,Int) -> Bool
		percorreLinhas [] _ = False
		percorreLinhas (h:t) (p1, p2) = if(p2 == 0) then percorreColunas h p1
										else percorreLinhas t (p1, p2 - 1)
		
		percorreColunas :: String -> Int -> Bool
		percorreColunas [] _ = False
		percorreColunas (x:xs) p1 |p1 == 0 = if(x == '#' || x == 'I' || x == 'H') then False else True
								  |otherwise = percorreColunas xs (p1 - 1)

-- | Devolve o carater correspondente a um par de coordenadas
devolveCarater :: (Int, Int) -> [String] -> Char
devolveCarater crd mp = percorreLinhas (reverse mp) crd 0
	where
		percorreLinhas :: [String] -> (Int,Int) -> Int -> Char
		percorreLinhas [] _ n = '#'
		percorreLinhas (h:t) (p1, p2) n |p2 == n = percorreColunas h p1 0
										|otherwise = percorreLinhas t (p1, p2) (n+1)
		
		percorreColunas :: String -> Int -> Int -> Char
		percorreColunas [] _ n = '#'
		percorreColunas (x:xs) p1 n |p1 == n = x 
								  	|otherwise = percorreColunas xs p1 (n+1)

-- | Verifica se ainda ha caixas que nao estao na posiçao final
caixasF :: [String] -> Bool
caixasF [] = True
caixasF (h:t) = if(aux h) then caixasF t else False
	where
		aux :: String -> Bool
		aux [] = True
		aux (h:t) = if(h == 'H') then False else aux t

-- | Remove a caixa da sua posição anterior
removeCx :: [String] -> (Int, Int) -> [String]
removeCx mp cords = reverse(percorreLinhas (reverse mp) cords 0)
	where
		percorreLinhas :: [String] -> (Int,Int) -> Int -> [String]
		percorreLinhas [] _ n = []
		percorreLinhas (h:t) (p1, p2) n |p2 == n = (percorreColunas h p1 0) : t
										|otherwise = h : (percorreLinhas t (p1, p2) (n+1))
		
		percorreColunas :: String -> Int -> Int -> String
		percorreColunas [] _ n = []
		percorreColunas (x:xs) p1 n |p1 == n = ' ' : xs 
								  	|otherwise = x : percorreColunas xs p1 (n+1)

-- | Converte a lista de coordenadas, ainda em lista de /strings/, numa lista de pares de /Int/s  
processacoordenadas :: [String] -> [(Int, Int)]
processacoordenadas [] = []
processacoordenadas (h:t) = (read x, read y) : processacoordenadas t 
		where
			[x, y] = words h -- Separa os elementos de uma /string/ através dos espaços existentes entre eles e coloca-os numa lista

-- | Remove a coordenadas inválidas (com um so nro; com caracteres que não são nros ou linhas vazias)
removeInv :: [String] -> [String]
removeInv [] = []
removeInv (x:xs) | x==""= removeInv xs
                 | length (words x) /= 2 = removeInv xs
                 | remaux x == False = removeInv xs
                 | otherwise = x : removeInv xs

remaux :: String -> Bool
remaux [] = True
remaux (h:t) = if isDigit h || h == ' ' then remaux t else False 

-- | Remove as linhas vazias da segunda parte do mapa, eliminando possíveis erros de execução
removeV :: [String] -> [String]
removeV []= []
removeV (x:xs) |x == "" = removeV xs
			   |otherwise = x : removeV xs