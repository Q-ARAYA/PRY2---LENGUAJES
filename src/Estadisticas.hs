{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Estadisticas where

import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as Map
import Data.List (sortOn, group, sort)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))

import Importacion (Venta(..))
import Analisis (redondear2dec)

menuEstadisticas :: [Venta] -> IO ()
menuEstadisticas ventas = do
    putStrLn "\n--- ESTADISTICAS ---"
    putStrLn "1. Categorías con mayores ventas"
    putStrLn "2. Producto más vendido"
    putStrLn "3. Categoría con menor participación"
    putStrLn "4. Resumen general"
    putStrLn "7. Volver"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "\n--- Categorías con mayores ventas ---"
            topCategoriasVentas ventas
            menuEstadisticas ventas

        "2" -> do
            putStrLn "\n--- Producto más vendido ---"
            productoMasVendido ventas
            menuEstadisticas ventas

        "3" -> do
            putStrLn "\n--- Categoría con menor participación ---"
            categoriaMenorParticipacion ventas
            menuEstadisticas ventas

        "4" -> do
            putStrLn "\n--- Resumen general ---"
            resumenGeneral ventas
            menuEstadisticas ventas

        "7" -> do
            putStrLn "\n.."
            

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
            menuEstadisticas ventas

-- Función para obtener el top 5 de categorías con mayores ventas
-- Entradas: lista de ventas
-- Salida: Imprime el top 5 de categorías con mayores ventas
topCategoriasVentas :: [Venta] -> IO ()
topCategoriasVentas ventas = do
    -- Agrupar las ventas por categoría y sumar montos
    let ventasPorCategoria = Map.toList $
            Map.fromListWith (+) [ (categoria v, total v) | v <- ventas ]

        -- Ordenae de mayor a menor
        ordenadas = take 5 $ sortOn (Down . snd) ventasPorCategoria

    putStrLn "\n--- Top 5 de categorías con mayores ventas ---"
    mapM_ (\(cat, monto) ->
        putStrLn $ "- " ++ cat ++ ": $" ++ show (redondear2dec monto)
        ) ordenadas

-- Función para obtener el producto más vendido
-- Entradas: lista de ventas
-- Salida: Imprime el producto más vendido
productoMasVendido :: [Venta] -> IO ()
productoMasVendido ventas = do
    let ventasPorProductoMap = Map.fromListWith
            (\(nombre1, cant1) (_, cant2) -> (nombre1, cant1 + cant2))
            [ (producto_id v, (producto_nombre v, cantidad v)) | v <- ventas ]

        ventasPorProducto = Map.toList ventasPorProductoMap
        ordenadas = sortOn (Down . (\(_, (_, cant)) -> cant)) ventasPorProducto

    if null ordenadas
        then putStrLn "\nNo hay ventas registradas."
        else do
            let (pid, (pname, cantTotal)) = head ordenadas
            putStrLn "\n--- Producto más vendido ---"
            putStrLn $ "- Producto Id: " ++ show pid
            putStrLn $ "- Producto: " ++ pname
            putStrLn $ "- Cantidad total vendida: " ++ show cantTotal

-- funcion que obtiene categoria con menor participación
--Entradsas: lista de ventas
-- Salida: Imprime la categoria con menor participación
categoriaMenorParticipacion :: [Venta] -> IO ()
categoriaMenorParticipacion ventas = do
    -- Agrupar las ventas por categoría y sumar las cantidades
    let cantidadesPorCategoria = Map.toList $
            Map.fromListWith (+) [ (categoria v, cantidad v) | v <- ventas ]

    if null cantidadesPorCategoria
        then putStrLn "\nNo hay ventas registradas."
        else do
            -- Buscar la categoría con menor cantidad total
            let (cat, cant) = head $ sortOn snd cantidadesPorCategoria

            putStrLn "\n--- Categoría con menor participación ---"
            putStrLn $ "- Categoría: " ++ cat
            putStrLn $ "- Cantidad total vendida: " ++ show cant

-- Función cantidad de ventas por categoría
-- Entradas: lista de ventas
-- Salida: Imprime la cantidad de ventas por categoría
cantidadVentasPorCategoria :: [Venta] -> IO ()
cantidadVentasPorCategoria ventas = do
    let conteo = Map.toList $
            Map.fromListWith (+) [ (categoria v, 1) | v <- ventas ]
    putStrLn "\n-----Cantidad de ventas por categoría-----"
    mapM_ (\(cat, n) -> putStrLn $ "- " ++ cat ++ ": " ++ show n ++ " ventas") conteo

-- Funcion venta mas alta y mas baja
-- Entradas: lista de ventas
-- Salida: Imprime la venta mas alta y la más baja
ventaMasAltaYMasBaja :: [Venta] -> IO ()
ventaMasAltaYMasBaja ventas =
    if null ventas
        then putStrLn "\nNo hay ventas registradas."
        else do
            let ordenadas = sortOn total ventas
                menor = head ordenadas
                mayor = last ordenadas
            putStrLn "\n-----Venta más baja-----"
            putStrLn $ "- Producto: " ++ producto_nombre menor ++ " | Total: $" ++ show (redondear2dec $ total menor)
            putStrLn "\n-----Venta más alta-----"
            putStrLn $ "- Producto: " ++ producto_nombre mayor ++ " | Total: $" ++ show (redondear2dec $ total mayor)

-- Funcion categoria con mayor variedad de productos vendidos
-- Entradas: lista de ventas
-- Salida: Imprime la categoria con mayor variedad de productos vendidos
categoriaMayorVariedad :: [Venta] -> IO ()
categoriaMayorVariedad ventas = do
    let productosPorCategoria = Map.fromListWith (\a b -> a ++ b)
            [ (categoria v, [producto_id v]) | v <- ventas ]
        variedad = [ (cat, length (unique prods)) | (cat, prods) <- Map.toList productosPorCategoria ]
            where unique = map head . group . sort

    if null variedad
        then putStrLn "\nNo hay ventas registradas."
        else do
            let (catMax, cantProd) = last $ sortOn snd variedad
            putStrLn "\n-----Categoría con mayor variedad de productos vendidos-----"
            putStrLn $ "- Categoría: " ++ catMax
            putStrLn $ "- Variedad de productos: " ++ show cantProd

resumenGeneral :: [Venta] -> IO ()
resumenGeneral ventas = do
    cantidadVentasPorCategoria ventas
    ventaMasAltaYMasBaja ventas
    categoriaMayorVariedad ventas