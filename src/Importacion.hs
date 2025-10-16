{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Importacion
  ( Venta(..)
  , importarVentas
  ) where

import System.IO
import System.Directory (doesFileExist)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Control.Monad (when)

-- se define el tipo de venta
data Venta = Venta
  { venta_id :: Int
  , fecha :: String
  , producto_id :: Int
  , producto_nombre :: String
  , categoria :: String
  , cantidad :: Int
  , precio_unitario :: Double
  , total :: Double
  } deriving (Show, Generic)

instance FromJSON Venta
instance ToJSON Venta

-- Funcion principal de importación de datos
-- Entradas: ruta del archivo json, lista actual de ventas en memoria
-- Salida: lista nueva de ventas en memoria
-- Restricciones: el archivo debe existir y tener formato JSON valido,
--               ademas tiene se implementaron funciones auxiliares para validar los datos
importarVentas :: FilePath -> [Venta] -> IO [Venta]
importarVentas archivoEntrada ventasExistentes = do
  existe <- doesFileExist archivoEntrada
  if not existe
    then do
      putStrLn "El archivo especificado no existe."
      return ventasExistentes
    else do
      contenido <- B.readFile archivoEntrada
      let ventasNuevas = decode contenido :: Maybe [Venta]

      case ventasNuevas of
        Nothing -> do
          putStrLn "Error: el archivo no tiene formato JSON válido o la estructura no coincide."
          return ventasExistentes
        Just nuevas -> do
          let (validas, invalidas) = validarVentas nuevas
              ventasActualizadas = ventasExistentes ++ validas

          putStrLn $ "\nSe agregaron " ++ show (length validas) ++ " ventas válidas."
          when (not (null invalidas)) $ do
            putStrLn $ "\nSe encontraron " ++ show (length invalidas) ++ " registros inválidos:\n"
            mapM_ reportarErrorVenta invalidas

          return ventasActualizadas

-- separa las ventas válidas de las inválidas
-- ademas de generar una lista de errores para cada venta inválida
validarVentas :: [Venta] -> ([Venta], [(Venta, [String])])
validarVentas = foldr
  (\v (ok, err) ->
      let errores = erroresDeVenta v
      in if null errores
            then (v : ok, err)
            else (ok, (v, errores) : err))
  ([], [])


erroresDeVenta :: Venta -> [String]
erroresDeVenta v = concat
  [ if null (fecha v) then ["Campo 'fecha' vacío"] else []
  , if null (producto_nombre v) then ["Campo 'producto_nombre' vacío"] else []
  , if null (categoria v) then ["Campo 'categoria' vacío"] else []
  , if cantidad v < 0 then ["'cantidad' debe ser mayor que 0"] else []
  , if precio_unitario v < 0 then ["'precio_unitario' no puede ser negativo"] else []
  , if total v < 0 then ["'total' no puede ser negativo"] else []
  ]


reportarErrorVenta :: (Venta, [String]) -> IO ()
reportarErrorVenta (v, errores) = do
  putStrLn $ "Venta inválida (ID: " ++ show (venta_id v) ++ "):"
  mapM_ (\e -> putStrLn $ "  - " ++ e) errores
  putStrLn ""
