{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import System.Directory (doesFileExist, renameFile)
import Data.Time (parseTimeM, defaultTimeLocale, Day)

import Importacion (Venta(..), importarVentas)
import Procesamiento (menuProcesamiento)
import Analisis (menuAnalisis, redondear2dec)
import Estadisticas (menuEstadisticas)

main :: IO ()
main = do
    putStrLn "=========================================="
    putStrLn "           ANALISIS DE VENTAS              "
    putStrLn "=========================================="

    ventasIniciales <- cargarVentas

    menuLoop ventasIniciales


menuLoop :: [Venta] -> IO ()
menuLoop ventas = do
    putStrLn "\n--- MENÚ PRINCIPAL ---"
    putStrLn "1. Importación de datos"
    putStrLn "2. Procesamiento de datos"
    putStrLn "3. Análisis de datos"
    putStrLn "4. Análisis temporal"
    putStrLn "5. Búsqueda de ventas"
    putStrLn "6. Estadísticas"
    putStrLn "7. Salir"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "\n--- Importación de datos ---"
            putStr "Ingrese el nombre del archivo JSON: "
            hFlush stdout
            ruta <- getLine

            -- Llamada a importarVentas
            nuevasVentas <- importarVentas ruta ventas
            imprimirVentas nuevasVentas
            menuLoop nuevasVentas

        "2" -> do
            putStrLn "\n--- Procesamiento de datos ---"
            nuevasVentas <- menuProcesamiento ventas
            imprimirVentas nuevasVentas
            menuLoop nuevasVentas

        "3" -> do
            putStrLn "\n--- Análisis de datos ---"
            menuAnalisis ventas
            menuLoop ventas

        "4" -> do
            putStrLn "\n--- Análisis temporal ---"
            -- analisisTemporal ventas
            menuLoop ventas

        "5" -> do
            putStrLn "\n--- Búsqueda de ventas ---"
            buscarVentasPorRango ventas
            menuLoop ventas

        "6" -> do
            putStrLn "\n--- Estadísticas ---"
            menuEstadisticas ventas
            menuLoop ventas

        "7" -> do
            putStrLn "\nSaliendo del sistema..."
            putStrLn "Gracias por usar el sistema de análisis de ventas."
            guardarVentas ventas
            putStrLn "--- Datos guardados correctamente. ---"

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
            menuLoop ventas


-- imprime las ventas existenetes en memoria
imprimirVentas :: [Venta] -> IO ()
imprimirVentas ventas = do
  putStrLn "\n--- Ventas en memoria ---"
  if null ventas
    then putStrLn "No hay ventas cargadas."
    else mapM_ (\v -> putStrLn $
        "ID: " ++ show (venta_id v) ++ 
        " | Fecha: " ++ fecha v ++ 
        " | Producto ID: " ++ show (producto_id v) ++ 
        " | Nombre: " ++ producto_nombre v ++ 
        " | Categoría: " ++ categoria v ++ 
        " | Cantidad: " ++ show (cantidad v) ++ 
        " | Precio Unitario: " ++ show (precio_unitario v) ++ 
        " | Total: " ++ show (total v)

      ) ventas
  putStrLn "------------------------\n"

-- guarda las ventas en un json para manejar persistencia
guardarVentas :: [Venta] -> IO ()
guardarVentas ventas = do
  let archivoTemp = "ventas_temp.json"
  B.writeFile archivoTemp (encode ventas)
  renameFile archivoTemp "ventas.json"
  putStrLn "Ventas guardadas en ventas.json"

-- carga las ventas desde el json
cargarVentas :: IO [Venta]
cargarVentas = do
  existe <- doesFileExist "ventas.json"
  if not existe
    then return []
    else do
      contenido <- B.readFile "ventas.json"
      let decoded = decode contenido :: Maybe [Venta]
      case decoded of
        Nothing -> do
          putStrLn "Error al leer ventas.json. Archivo corrupto o mal formado."
          return []
        Just ventas -> return ventas

-- Funcion para buscar ventas por rango de fechas
-- Entradas: lista de ventas
-- Salida: Imprime las ventas dentro del rango especificado y el total
-- Restricciones: Se asume que las fechas estan en formato "YYYY-MM-DD"
buscarVentasPorRango :: [Venta] -> IO ()
buscarVentasPorRango ventas = do
    putStr "Ingrese la fecha inicial (YYYY-MM-DD): "
    hFlush stdout
    fechaInicio <- getLine

    putStr "Ingrese la fecha final (YYYY-MM-DD): "
    hFlush stdout
    fechaFin <- getLine

    let entradaInicio = convertirATipoDay fechaInicio
        entradaFin    = convertirATipoDay fechaFin

    case (entradaInicio, entradaFin) of
        (Just inicio, Just fin) -> do
            let ventasFiltradas = filtrarPorRango inicio fin ventas
            if null ventasFiltradas
                then putStrLn "\nNo se encontraron ventas en el rango especificado."
                else do
                    putStrLn "\nVentas en el rango seleccionado:\n"
                    mapM_ imprimirVentaPorRango ventasFiltradas
                    let totalRango = sum [ total v | v <- ventasFiltradas ]
                    putStrLn $ "\nTotal de ventas en el rango: $" ++ show (redondear2dec totalRango)
        _ -> putStrLn "\nFormato de fecha inválido. Use el formato YYYY-MM-DD."

-- Convierte un string "YYYY-MM-DD" a tipo Day
convertirATipoDay :: String -> Maybe Day
convertirATipoDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Filtra las ventas dentro del rango
filtrarPorRango :: Day -> Day -> [Venta] -> [Venta]
filtrarPorRango inicio fin = filter (\v ->
    case convertirATipoDay (fecha v) of
        Just f  -> f >= inicio && f <= fin
        Nothing -> False
    )

imprimirVentaPorRango :: Venta -> IO ()
imprimirVentaPorRango v = putStrLn $
    "ID: " ++ show (venta_id v) ++ 
    " | Fecha: " ++ fecha v ++ 
    " | Producto ID: " ++ show (producto_id v) ++ 
    " | Nombre: " ++ producto_nombre v ++ 
    " | Categoría: " ++ categoria v ++ 
    " | Cantidad: " ++ show (cantidad v) ++ 
    " | Precio Unitario: " ++ show (precio_unitario v) ++ 
    " | Total: " ++ show (total v)

