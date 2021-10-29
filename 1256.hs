module Main where

import Numeric

data Imovel = Construtor {qtdMoradores, qtdConsumo :: Int} deriving Show

instance Eq Imovel where
    imovel1 == imovel2  = qtdConsumo imovel1 == qtdConsumo imovel2

instance Ord Imovel where 
    imovel1 <= imovel2  = qtdConsumo imovel1 <= qtdConsumo imovel2

formatarMediaConsumoCidade :: Float -> String
formatarMediaConsumoCidade media = mediaFormatada
    where
        mediaFormatada = "Consumo medio: " ++ (showGFloat (Just 2) media "") ++ " m3." 

mediaConsumoCidade :: [Imovel] -> Float
mediaConsumoCidade imoveis = somaConsumo `divisaoExata` somaPessoas
    where
        somaConsumo    = foldl (+) 0 listaConsumo
        somaPessoas    = foldl (+) 0 listaMoradores
        listaConsumo   = map qtdConsumo imoveis
        listaMoradores = map qtdMoradores imoveis

quickSort :: [Imovel] -> [Imovel]
quickSort [] = []
quickSort (imovel:caldaImoveis) = 
    quickSort minors ++ [imovel] ++ quickSort majors
    where
        minors = filter (<=imovel) caldaImoveis
        majors = filter (>imovel) caldaImoveis

mostrarConsumoPorImovel :: [Imovel] -> String
mostrarConsumoPorImovel (imovel:[])  = consumoDoImovel imovel
mostrarConsumoPorImovel (imovel:caldaImoveis) = consumoDoImovel imovel ++ " " ++ mostrarConsumoPorImovel caldaImoveis

consumoDoImovel :: Imovel -> String
consumoDoImovel imovel = (show $ qtdMoradores imovel) ++ "-" ++ (mediaConsumo imovel)        

mediaConsumo :: Imovel -> String
mediaConsumo (Construtor qtdMoradores qtdConsumo) = show $ floor $ qtdConsumo `divisaoExata` qtdMoradores

divisaoExata :: Int -> Int -> Float
divisaoExata x y = (fromIntegral x) / (fromIntegral y)

main = do
        print "Ola mundo"