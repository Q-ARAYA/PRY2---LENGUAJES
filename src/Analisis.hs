{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Analisis where

import System.IO (hFlush, stdout)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import GHC.Generics (Generic)
import Importacion (Venta(..))


menuAnalisis :: [Venta] -> IO ()
menuAnalisis ventas = do
    putStrLn "\n--- ANALISIS DE DATOS ---"
    putStrLn "1. Total de ventas"
    putStrLn "2. Total de ventas mensual y anual"
    putStrLn "3. Promedio de ventas por categoría por año"
    putStrLn "7. Volver"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\n--- Total de ventas ---"
            totalVentas ventas
            menuAnalisis ventas

        "2" -> do
            putStrLn "\n--- Total de ventas mensual y anual ---"
            totalVentasMensualYAnual ventas
            menuAnalisis ventas
        
        "3" -> do
            putStrLn "\n--- Promedio de ventas por categoría por año ---"
            promedioVentasPorCategoriaPorAnio ventas
            menuAnalisis ventas

        "7" -> do
            putStrLn "\n.."

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
            menuAnalisis ventas


-- Funcion total de ventas
-- Entrada: lista de ventas
-- Salida: Imprime el total de ventas
totalVentas :: [Venta] -> IO ()
totalVentas ventas = do
    let sumaTotal = sum [ total v | v <- ventas ]
    putStrLn $ "\nTotal de ventas: $" ++ show (redondear2dec sumaTotal)


-- Funcion total de ventas mensual y anual
-- Entrada: lista de ventas
-- Salida: Imprime el total de ventas por mes y por año
totalVentasMensualYAnual :: [Venta] -> IO ()
totalVentasMensualYAnual ventas = do
    let ventasPorPeriodo = [ ((take 4 (fecha v), take 7 (fecha v)), total v) | v <- ventas ]
        sumasPorMes  = Map.toList $ Map.fromListWith (+) ventasPorPeriodo
        sumasPorAnio = Map.toList $ Map.fromListWith (+)
                        [ (anio, t) | ((anio, _), t) <- ventasPorPeriodo ]

        ordenMeses = sortOn fst sumasPorMes
        ordenAnios = sortOn fst sumasPorAnio

    putStrLn "\nTotal de ventas por mes:"
    mapM_ (\((_, mes), suma) -> putStrLn $ "- " ++ mes ++ ": $" ++ show (redondear2dec suma)) ordenMeses

    putStrLn "\nTotal de ventas por año:"
    mapM_ (\(anio, suma) -> putStrLn $ "- " ++ anio ++ ": $" ++ show (redondear2dec suma)) ordenAnios


-- Funcion de promedio de ventas por categoría por año
-- Entrada: lista de ventas
-- Salida: Imprime el promedio de ventas por categoría por año
promedioVentasPorCategoriaPorAnio :: [Venta] -> IO ()
promedioVentasPorCategoriaPorAnio ventas = do
    let ventasPorCatAnio = [ ((take 4 (fecha v), categoria v), total v) | v <- ventas ]
        agrupadas = Map.fromListWith (++) [ (k, [v]) | (k, v) <- ventasPorCatAnio ]
        promedios = Map.toList $ Map.map promedio agrupadas
        ordenados = sortOn (Down . fst) promedios

    putStrLn "\nPromedio de ventas por categoría por año:"
    mapM_ (\((anio, cat), prom) ->
        putStrLn $ "- " ++ anio ++ " | " ++ cat ++ ": $" ++ show (redondear2dec prom)
        ) ordenados

-- Función auxiliar para calcular el promedio de ventas
promedio :: [Double] -> Double
promedio xs
  | null xs   = 0
  | otherwise = sum xs / fromIntegral (length xs)


redondear2dec :: Double -> Double
redondear2dec x = fromIntegral (round (x * 100)) / 100
