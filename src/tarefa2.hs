{-|
Module : Main
Descrição : Módulo de Haskell referente à segunda tarefa do projeto de LI.
Copyright : Miguel Quaresma <miguelquaresma97@gmail.com>
			João Nogueira	<joaonogueira097@hotmail.com>

Um módulo que contém funções que ao receberem um mapa com coordenadas procedem à remoção das paredes (representadas por '#') redundantes bem como
à colocação, no mapa, do boneco e das caixas usando as coordenadas dadas.
-}

module Main where

import qualified Data.Text as T
import Data.List
import Data.Char
import System.Directory

main = do inp <- getContents
          putStr (outStr (tarefa2 (inStr inp)))

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


tarefa2 :: [String] -> [String]
tarefa2 linhas = colocaTudo (reverse (simplificaMapa (reverse tab) todas (reverse tab))) coordenadasF 
	where
		(tab, coordenadas) = dividemapa linhas
		coordenadasF = processacoordenadas (removeInv coordenadas) 
		todas = recolheCords 0 0 tab

-- | Corre todos os testes de um determinado diretório automaticamente, recorrendo à função correTeste
correTestes :: IO ()
correTestes = do
      files2 <- getDirectoryContents "/home/miguel/Documentos/Disciplinas/Laboratórios_de_Informática/li1g030/tests/T2/"
      let inputs2 = map ("/home/miguel/Documentos/Disciplinas/Laboratórios_de_Informática/li1g030/tests/T2/" ++) $ filter (isSuffixOf ".in") files2 
      mapM_ (correTeste tarefa2) inputs2


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



-- |Separa as coordenadas do tabuleiro/mapa
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
							
-- | Remove os cardinais redundantes do mapa inteiro
simplificaMapa :: [String] -> [(Int, Int)] -> [String] -> [String]
simplificaMapa [] _ _ = []
simplificaMapa (h:t) cds tab = removeDeLinha h cds tab : simplificaMapa t (drop (length h) cds) tab


-- | Remove os cardinais redundates de cada linha, verificando, caso o caracter em questão seja um cardinal, todos os caracteres à sua volta e atuando em conformidade
removeDeLinha :: String-> [(Int, Int)]-> [String] -> String
removeDeLinha [] _ _ = []
removeDeLinha (h:t) (x:xs) tab |devolveCarater x tab == ' ' = ' ' : removeDeLinha t xs tab
						   	   |devolveCarater x tab == '.' = '.' : removeDeLinha t xs tab
							   |(devolveCarater (l1, l2 + 1) tab == ' ' || devolveCarater (l1 + 1, l2) tab == ' ' || devolveCarater (l1, l2 - 1) tab == ' ' || devolveCarater (l1 - 1, l2) tab == ' ' || devolveCarater (l1 + 1, l2 + 1) tab == ' ' || devolveCarater (l1 - 1, l2 - 1) tab == ' ' || devolveCarater (l1 - 1, l2 + 1) tab == ' ' || devolveCarater (l1 + 1, l2 - 1) tab == ' ') = '#' :  removeDeLinha t xs tab
							   |(devolveCarater (l1, l2 + 1) tab == '.' || devolveCarater (l1 + 1, l2) tab == '.' || devolveCarater (l1, l2 - 1) tab == '.' || devolveCarater (l1 - 1, l2) tab == '.' || devolveCarater (l1 + 1, l2 + 1) tab == '.' || devolveCarater (l1 - 1, l2 - 1) tab == '.' || devolveCarater (l1 - 1, l2 + 1) tab == '.' || devolveCarater (l1 + 1, l2 - 1) tab == '.') = '#' :  removeDeLinha t xs tab
							   |(devolveCarater (l1, l2 + 1) tab == '#' && devolveCarater (l1 + 1, l2) tab == '#' && devolveCarater (l1, l2 - 1) tab == '#' && devolveCarater (l1 - 1, l2) tab == '#' && devolveCarater (l1 + 1, l2 + 1) tab == '#' && devolveCarater (l1 - 1, l2 - 1) tab == '#' && devolveCarater (l1 - 1, l2 + 1) tab == '#' && devolveCarater (l1 + 1, l2 - 1) tab == '#') = ' ' :  removeDeLinha t xs tab
	where
		(l1, l2) = x


-- | Devolve o carater correspondente a um par de coordenadas
devolveCarater :: (Int, Int) -> [String] -> Char
devolveCarater crd mp = percorreLinhas mp crd 0
	where
		percorreLinhas :: [String] -> (Int,Int) -> Int -> Char
		percorreLinhas [] _ n = '#'
		percorreLinhas (h:t) (p1, p2) n |p2 == n = percorreColunas h p1 0
										|otherwise = percorreLinhas t (p1, p2) (n+1)
		
		percorreColunas :: String -> Int -> Int -> Char
		percorreColunas [] _ n = '#'
		percorreColunas (x:xs) p1 n |p1 == n = x 
								  	|otherwise = percorreColunas xs p1 (n+1)


-- | Recolhe todas as coordenadas referentes a todas posições possíveis no mapa para facilitar a remoção dos cardinais					  
recolheCords :: Int -> Int -> [String] -> [(Int, Int)]
recolheCords _ _ [] = []
recolheCords c l (x:xs) = (aux c l x) ++ recolheCords c (l+1) xs
	where
		aux :: Int -> Int -> String -> [(Int, Int)]
		aux c l  [] = []
		aux c l (h:t) = (c, l) : aux (c+1) l t


-- | Coloca o boneco e as caixas no mapa
colocaTudo :: [String] -> [(Int, Int)] -> [String]
colocaTudo l (h:t) = colocaBoneco (colocaCaixas l t) h


-- | Coloca o boneco no mapa
colocaBoneco :: [String] -> (Int,Int) -> [String]
colocaBoneco l (x,y) = reverse (percorreLinhas (reverse l) (x,y)) 
	where
		percorreLinhas :: [String] -> (Int, Int) -> [String]
		percorreLinhas (h:t) (p1, p2) |p2 == 0 = (percorreColunas h  p1) : t 
									  |otherwise = h : percorreLinhas t (p1, p2 - 1) 

		percorreColunas :: String -> Int -> String
		percorreColunas [] _ = []
		percorreColunas (z:zs) n |n == 0 = 'o' : zs 
								 |otherwise = z : percorreColunas zs (n-1)


-- | Chama a função __colocaCaixa__ para colocar todas as caixas no mapa
colocaCaixas :: [String] -> [(Int, Int)] -> [String]
colocaCaixas l [z] = colocaCaixa l z 
colocaCaixas l (h:t) = colocaCaixas (colocaCaixa l h) t 


-- | Coloca uma caixa no mapa
colocaCaixa :: [String] -> (Int,Int) -> [String]
colocaCaixa l (x,y) = reverse (percorreLinhas (reverse l) (x,y)) 
	where
		percorreLinhas :: [String] -> (Int, Int) -> [String]
		percorreLinhas (h:t) (p1, p2) | p2 == 0 = (percorreColunas h  p1) : t 
									  |otherwise = h : percorreLinhas t (p1, p2 - 1) 

		percorreColunas :: String -> Int -> String
		percorreColunas [] _ = []
		percorreColunas (z:zs) n |n == 0 = if(z == '.') then 'I' : zs else 'H' : zs 
								 |otherwise = z : percorreColunas zs (n-1)


-- | Converte a lista de coordenadas, ainda em lista de /strings/, numa lista de pares de /Int/s 
processacoordenadas :: [String] -> [(Int, Int)]
processacoordenadas [] = []
processacoordenadas (h:t) = (read x, read y) : processacoordenadas t 
		where
			[x, y] = words h

-- | Remove as coordenadas inválidas
removeInv :: [String] -> [String]
removeInv [] = []
removeInv (x:xs) | x==""= removeInv xs
                 | length (words x) /= 2 = removeInv xs
                 | remaux x == False = removeInv xs
                 | otherwise = x : removeInv xs

remaux :: String -> Bool
remaux [] = True
remaux (h:t) = if isDigit h || h == ' ' then remaux t else False