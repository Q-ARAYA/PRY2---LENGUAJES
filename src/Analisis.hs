{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Analisis where

import System.IO (hFlush, stdout)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as Set
import Data.List (foldl')
import System.Directory (renameFile, removeFile)
import qualified Data.Map.Strict as Map
import Data.List (maximumBy, sort)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))


data Venta = Venta
  { venta_id        :: Int
  , fecha           :: String
  , producto_id     :: Int
  , producto_nombre :: String
  , categoria       :: String
  , cantidad        :: Int
  , precio_unitario :: Double
  , total           :: Double
  } deriving (Show, Generic)

instance FromJSON Venta
instance ToJSON Venta where

menuAnalisis :: IO ()
menuAnalisis = do
    putStrLn "\n--- ANALISIS DE DATOS ---"
    putStrLn "1. Total de ventas"
    putStrLn "2. Total de ventas mensual y anual"
    putStrLn "3. Promedio de ventas por categoría por anio"
    putStrLn "6. Salir"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\n--- Total de ventas seleccionado ---"
            totalVentas "ventas.json"
            menuAnalisis 

        "2" -> do
            putStrLn "\n--- Total de ventas mensual y anual seleccionado ---"
            totalVentasMensualYAnual "ventas.json"
            menuAnalisis
        
        "3" -> do
            putStrLn "\n--- Promedio de ventas por categoría por año seleccionado ---"
            promedioVentasPorCategoriaPorAnio "ventas.json"
            menuAnalisis

        "6" -> do
            putStrLn "\n.."
            

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
            menuAnalisis 

totalVentas :: FilePath -> IO ()
totalVentas ruta = do
    contenido <- B.readFile ruta
    let ventas = case decode contenido :: Maybe [Venta] of
                    Just v  -> v
                    Nothing -> []
        sumaTotal = sum [ total v | v <- ventas ]
    putStrLn $ "\nTotal de ventas: $" ++ show sumaTotal


totalVentasMensualYAnual :: FilePath -> IO ()
totalVentasMensualYAnual ruta = do
    contenido <- B.readFile ruta
    let ventas = case decode contenido :: Maybe [Venta] of
                    Just v  -> v
                    Nothing -> []

        -- Extraer año y mes
        ventasPorPeriodo = [ ((take 4 (fecha v), take 7 (fecha v)), total v) | v <- ventas ]

        -- Sumar por año-mes
        sumasPorMes = Map.toList $ Map.fromListWith (+) ventasPorPeriodo

        -- sumas por año
        sumasPorAnio = Map.toList $
                       Map.fromListWith (+) [ (anio, t) | ((anio, _), t) <- ventasPorPeriodo ]

        -- Ordenar de forma cronológica
        ordenMeses = sortOn fst sumasPorMes
        ordenAnios = sortOn fst sumasPorAnio

    putStrLn "\nTotal de ventas por mes:"
    mapM_ (\((anio, mes), suma) -> putStrLn $ "- " ++ mes ++ ": $" ++ show suma) ordenMeses

    putStrLn "\nTotal de ventas por año:"
    mapM_ (\(anio, suma) -> putStrLn $ "- " ++ anio ++ ": $" ++ show suma) ordenAnios


promedioVentasPorCategoriaPorAnio :: FilePath -> IO ()
promedioVentasPorCategoriaPorAnio ruta = do
    contenido <- B.readFile ruta
    let ventas = case decode contenido :: Maybe [Venta] of
                    Just v  -> v
                    Nothing -> []

        -- Extraer año y categoría
        ventasPorCatAnio = [ ((take 4 (fecha v), categoria v), total v) | v <- ventas ]

        -- Agrupa las ventas de cada (año, categoría)
        agrupadas = Map.fromListWith (++) [ (k, [v]) | (k, v) <- ventasPorCatAnio ]

        -- Calcular promedios
        promedios = Map.toList $ Map.map promedio agrupadas

        -- Ordenar por año descendente
        ordenados = sortOn (Down . fst) promedios

    putStrLn "\nPromedio de ventas por categoría por año:"
    mapM_ (\((anio, cat), prom) ->
        putStrLn $ "- " ++ anio ++ " | " ++ cat ++ ": $" ++ show (redondear2dec prom)
        ) ordenados


promedio :: [Double] -> Double
promedio xs = sum xs / fromIntegral (length xs)


redondear2dec :: Double -> Double
redondear2dec x = fromIntegral (round (x * 100)) / 100
