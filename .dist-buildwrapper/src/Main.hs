{--
        Esqueleto de programa para gera√ß√£o de bubble cloud em Haskell.
        Mais informa√ß√µes em: http://www.inf.ufsm.br/~andrea/elc117-2012b
--}

--Luis Fernando Battisti de Araujo e Silva
--MatrÌcula: 2012510280
--link da p·gina da disciplina: http://www-usr.inf.ufsm.br/~andrea/elc117/

--Trabalho 3: Tagcloud
--Entrega: 26/11/2012

module Main where

import Text.Printf -- Oba, Haskell tem printf! :-)
import System.Random
import System.IO.Unsafe

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Float
imageWidth = 360

imageHeight :: Float
imageHeight = 360


-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readFloat (map snd pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de Float
readFloat :: [String] -> [Float]
readFloat ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Float -> Float -> [Float] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen [] 0 dataset ) ) ++ "</svg>\n"


-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
svgBubbleGen :: [Circle] -> Float -> [Float] -> [String]
svgBubbleGen _ _ dataset
    |dataset == [] = []
svgBubbleGen listacir t dataset = 
        let cir = posicaocirculo  t (head dataset/15) listacir
         in [svgCircle (cir, (head dataset/15)+3)] ++ (svgBubbleGen ([(cir, (head dataset/15)+3)] ++ listacir) t (tail dataset))


--Move em espiral a partir da posiÁ„o
posicaocirculo :: Float -> Float -> [Circle] -> Point
posicaocirculo t r listacir = if (intersecao ((x,y),r) listacir) then posicaocirculo (t + 0.2) r listacir else (x,y)
                              where x = (1 * t * (cos t) +180)
                                    y = (1 * t * (sin t) +180)

    
--Se a soma do raio for maior que a distancia ent„o È verdadeiro...
intersecao :: Circle -> [Circle] -> Bool
intersecao circulo listacir = if listacir == [] then False else auxintersecao circulo listacir

auxintersecao :: Circle -> [Circle] -> Bool
auxintersecao circulo listacir = if ((distancia (fst circulo) (fst (head listacir))) < ((snd circulo) + (snd (head listacir)))) then True else intersecao circulo (tail listacir)

--Dist‚ncia entre dois pontos (P e Q) -> PeQ = sqrt (x2 - x1)≤ + (y2 - y1)≤
distancia :: Point -> Point -> Float
distancia p q = let x = ((fst p) - (fst q)) ^ 2 
                    y = ((snd p) - (snd q)) ^ 2
                 in sqrt (x + y) 


-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro. 
svgCircle :: Circle -> String
svgCircle ((x,y),r)= printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y r (unsafePerformIO randomcor) (unsafePerformIO randomcor) (unsafePerformIO randomcor)


-- gera um numero aleatorio de 0 atÈ 255        
randomcor :: IO Int
randomcor = randomRIO (0, 255 :: Int)


-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Float -> Float -> String
svgViewBox w h =
        printf  "<svg width=\"%f\" height=\"%f\" viewBox=\"0 0 %f %f\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%f\" height=\"%f\" style=\"fill:white;\"/>\n" w h