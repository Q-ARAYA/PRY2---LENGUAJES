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

pausa :: IO ()
pausa = do
    putStrLn "\nPresione Enter para continuar..."
    _ <- getLine
    return ()

limpiarTerminal :: IO ()
limpiarTerminal = putStr "\ESC[2J\ESC[H"

menuEstadisticas :: [Venta] -> IO ()
menuEstadisticas ventas = do
    limpiarTerminal
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
            pausa
            menuEstadisticas ventas

        "2" -> do
            putStrLn "\n--- Producto más vendido ---"
            productoMasVendido ventas
            pausa
            menuEstadisticas ventas

        "3" -> do
            putStrLn "\n--- Categoría con menor participación ---"
            categoriaMenorParticipacion ventas
            pausa
            menuEstadisticas ventas

        "4" -> do
            putStrLn "\n--- Resumen general ---"
            resumenGeneral ventas
            pausa
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
    let ventasPorCategoria = Map.toList $
            Map.fromListWith (+) [ (categoria v, total v) | v <- ventas ]
        ordenadas = take 5 $ sortOn (Down . snd) ventasPorCategoria
        lineas = [cat ++ " | " ++ show (redondear2dec monto) | (cat, monto) <- ordenadas]

    mapM_ (\(cat, monto) ->
        putStrLn $ "    - " ++ cat ++ ": $" ++ show (redondear2dec monto)
        ) ordenadas

    guardarCSV "Top Categorias con mayores ventas" ("Categoria | Monto" : lineas)

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
            let lineas = [show pid ++ " | " ++ pname ++ " | " ++ show cantTotal]
            putStrLn $ "    - Producto Id: " ++ show pid
            putStrLn $ "    - Producto: " ++ pname
            putStrLn $ "    - Cantidad total vendida: " ++ show cantTotal
            guardarCSV "Producto mas vendido" ("Id | Nombre | Cantidad" : lineas)

-- funcion que obtiene categoria con menor participación
--Entradsas: lista de ventas
-- Salida: Imprime la categoria con menor participación
categoriaMenorParticipacion :: [Venta] -> IO ()
categoriaMenorParticipacion ventas = do
    let cantidadesPorCategoria = Map.toList $
            Map.fromListWith (+) [ (categoria v, cantidad v) | v <- ventas ]
    if null cantidadesPorCategoria
        then putStrLn "\nNo hay ventas registradas."
        else do
            let (cat, cant) = head $ sortOn snd cantidadesPorCategoria
            let lineas = [cat ++ " | " ++ show cant]
            putStrLn $ "    - Categoria: " ++ cat
            putStrLn $ "    - Cantidad total vendida: " ++ show cant
            guardarCSV "Categoria con menor participacion" ("Categoria | Cantidad" : lineas)


-- Función cantidad de ventas por categoría
-- Entradas: lista de ventas
-- Salida: Imprime la cantidad de ventas por categoría
cantidadVentasPorCategoria :: [Venta] -> IO [String]
cantidadVentasPorCategoria ventas = do
    putStrLn "\nCantidad de ventas por categoría:"
    let conteo = Map.toList $
            Map.fromListWith (+) [ (categoria v, 1) | v <- ventas ]
        lineas = [cat ++ " | " ++ show n | (cat, n) <- conteo]
    mapM_ (\(cat, n) -> putStrLn $ "    - " ++ cat ++ ": " ++ show n ++ " ventas") conteo
    return lineas


-- Funcion venta mas alta y mas baja
-- Entradas: lista de ventas
-- Salida: Imprime la venta mas alta y la más baja
ventaMasAltaYMasBaja :: [Venta] -> IO [String]
ventaMasAltaYMasBaja ventas =
    if null ventas
        then do
            putStrLn "\nNo hay ventas registradas."
            return []
        else do
            putStrLn "\nVenta más alta y más baja:"
            let ordenadas = sortOn total ventas
                menor = head ordenadas
                mayor = last ordenadas
                lineas = [ "Mas baja," ++ producto_nombre menor ++ " | " ++ show (redondear2dec $ total menor)
                         , "Mas alta," ++ producto_nombre mayor ++ " | " ++ show (redondear2dec $ total mayor)
                         ]
            putStrLn $ "    - Producto mas bajo: " ++ producto_nombre menor ++ " | Total: $" ++ show (redondear2dec $ total menor)
            putStrLn $ "    - Producto mas alto: " ++ producto_nombre mayor ++ " | Total: $" ++ show (redondear2dec $ total mayor)
            return lineas

-- Funcion categoria con mayor variedad de productos vendidos
-- Entradas: lista de ventas
-- Salida: Imprime la categoria con mayor variedad de productos vendidos
categoriaMayorVariedad :: [Venta] -> IO [String]
categoriaMayorVariedad ventas = do
    let productosPorCategoria = Map.fromListWith (\a b -> a ++ b)
            [ (categoria v, [producto_id v]) | v <- ventas ]
        variedad = [ (cat, length (unique prods)) | (cat, prods) <- Map.toList productosPorCategoria ]
            where unique = map head . group . sort
    if null variedad
        then do
            putStrLn "\nNo hay ventas registradas."
            return []
        else do
            putStrLn "\nCategoría con mayor variedad de productos vendidos:"
            let (catMax, cantProd) = last $ sortOn snd variedad
            putStrLn $ "    - Categoria: " ++ catMax
            putStrLn $ "    - Variedad de productos: " ++ show cantProd
            return [catMax ++ " | " ++ show cantProd]

resumenGeneral :: [Venta] -> IO ()
resumenGeneral ventas = do
    lineas1 <- cantidadVentasPorCategoria ventas
    lineas2 <- ventaMasAltaYMasBaja ventas
    lineas3 <- categoriaMayorVariedad ventas
    let todas = ["Tipo de reporte: Resumen general",
                 "",
                 "Cantidad de ventas por categoria:",
                 "Categoria,Cantidad"]
                 ++ lineas1 ++
                 ["", "Venta mas alta y mas baja:", "Tipo,Producto,Total"]
                 ++ lineas2 ++
                 ["", "Categoria con mayor variedad:", "Categoría,Variedad"]
                 ++ lineas3
    guardarCSV "Resumen general" todas

guardarCSV :: String -> [String] -> IO ()
guardarCSV tipo lineas = do
    let contenido = tipo : lineas  -- primera línea: tipo de estadística
        csv = unlines contenido
    writeFile "estadisticas.csv" csv   -- reescribe el archivo
    putStrLn "\n[Guardado en 'estadisticas.csv']"