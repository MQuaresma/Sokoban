{-|
Module : Main
Descrição : Módulo de Haskell referente à quinta tarefa do projeto de LI.
Copyright : Miguel Quaresma <miguelquaresma97@gmail.com>
			João Nogueira   <joaonogueira097@hotmail.com> 

Módulo que recebe uma imagem e calcula a devolve a altura e lagura do menor retângulo onde essa mesma imagem cabe.
-}
module Main where

import qualified Data.Text as T
import Graphics.Gloss
import GlossExtras

main = do inp <- getContents
          let (x,y) = tarefa5 (readPicture inp)
          putStrLn (show (round x) ++ " " ++ show (round y))

teste :: Picture -> IO()
teste pic = let (x,y) = tarefa5 pic in putStrLn (show (round x) ++ " " ++ show (round y))


tarefa5 :: Picture -> (Float, Float)
tarefa5 pic = medidas pic

-- | Devolve as medidas do retangulo envolvente de uma Picture
medidas :: Picture -> (Float, Float)
medidas pic = ((maximum l)-(minimum l),(maximum a)-(minimum a))
	where
		path = pontosFig pic
		(l, a) = unzip path


-- | Recolhe os pontos principais de uma figura, ou de um conjunto de figuras
pontosFig :: Picture -> Path
pontosFig (Blank) = [(0,0)]
pontosFig (Polygon pts) = pts
pontosFig (Line pts) = pts
pontosFig (Circle r) = pontosCirculo r
pontosFig (Bitmap w1 h1 _ _ ) = [(-w / 2, -h / 2), (w / 2, h / 2), (w / 2, -h / 2), (-w / 2, h / 2)]
	where
		w = fromIntegral w1
		h = fromIntegral h1
pontosFig (Color _ pic) = pontosFig pic
pontosFig (Translate x y pic) = zip (map (+ x) x1) (map (+ y) y1)
	where
		(x1,y1) = unzip (pontosFig pic)
pontosFig (Rotate a pic) = rotatePts a (pontosFig pic)
pontosFig (Scale x y pic) = zip (map (* x) x1) (map (* y) y1)
	where
		(x1,y1) = unzip (pontosFig pic)
pontosFig (Pictures []) = []
pontosFig (Pictures (h:t)) = (pontosFig h) ++ pontosFig (Pictures t)
pontosFig _ = []


-- | Devolve os pontos após um rotação de x graus
rotatePts :: Float -> Path -> Path
rotatePts _ [] = []
rotatePts a (h:t) = (rotatePt ar h) : rotatePts a t
	where
		-- * Converte o angulo de radianos para graus
		ar = (a*pi)/180
		rotatePt :: Float -> (Float, Float) -> (Float, Float)
		rotatePt a (x, y) = ((x*(cos (-a))-y*(sin (-a)),x*(sin (-a))+y*(cos (-a))))

-- | Devolve as coordenadas de pontos do circulo, de 5 em 5 graus
pontosCirculo :: Float -> Path
pontosCirculo r = map (\x -> (sin(x) * r, cos(x) * r)) l
		where
			l = [5,10..360]
			