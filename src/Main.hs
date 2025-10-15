{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)

-- import Archivos
import Importacion (solicitarNombreArchivo)
-- import Procesamiento
-- import Analisis
-- import Temporal
-- import Estadisticasss


type Venta = String

main :: IO ()
main = do
    putStrLn "=========================================="
    putStrLn "   SISTEMA DE ANALISIS DE DATOS DE VENTAS"
    putStrLn "=========================================="
    menuLoop [] 

menuLoop :: [Venta] -> IO ()
menuLoop ventas = do
    putStrLn "\n--- MENÚ PRINCIPAL ---"
    putStrLn "1. Importación de datos"
    putStrLn "2. Procesamiento de datos"
    putStrLn "3. Análisis de datos"
    putStrLn "4. Análisis temporal"
    putStrLn "5. Estadísticas"
    putStrLn "6. Salir"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\n[Importación de datos seleccionadaaa]"
            solicitarNombreArchivo
            let nuevasVentas = ventas ++ ["EjemploVenta"]
            menuLoop nuevasVentas

        "2" -> do
            putStrLn "\n[Procesamiento de datos seleccionado]"
            -- procesamientoDatos ventas
            menuLoop ventas

        "3" -> do
            putStrLn "\n[Análisis de datos seleccionado]"
            -- analisisDatos ventas
            menuLoop ventas

        "4" -> do
            putStrLn "\n[Análisis temporal seleccionado]"
            -- analisisTemporal ventas
            menuLoop ventas

        "5" -> do
            putStrLn "\n[Estadísticas seleccionadas]"
            -- estadisticasMenu ventas
            menuLoop ventas

        "6" -> do
            putStrLn "\nSaliendo del sistema..."
            putStrLn "Gracias por usar el sistema de análisis de ventas."

        _   -> do
            putStrLn "\nOpción inválida. Intente de nuevo."
            menuLoop ventas
