{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Importacion (solicitarNombreArchivo) where

import System.IO
import System.Directory (doesFileExist, renameFile)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Control.Monad (when)


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

-- Función principal
solicitarNombreArchivo :: IO ()
solicitarNombreArchivo = do
  putStr "Nombre del archivo JSON de entrada: "
  hFlush stdout
  nombreArchivo <- getLine
  procesarVentas nombreArchivo

procesarVentas :: FilePath -> IO ()
procesarVentas archivoEntrada = do
  -- Verificar que el archivo de entrada exista
  existe <- doesFileExist archivoEntrada
  if not existe
    then putStrLn "El archivo especificado no existe."
    else do
      -- Leer archivo JSON de entrada
      contenido <- B.readFile archivoEntrada
      let ventasNuevas = decode contenido :: Maybe [Venta]

      case ventasNuevas of
        Nothing -> putStrLn "Error: el archivo no tiene formato JSON válido o la estructura no coincide."
        Just nuevas -> do
          let (validas, invalidas) = validarVentas nuevas

          -- Leer ventas existentes
          existeVentas <- doesFileExist "ventas.json"
          ventasExistentes <- if existeVentas
            then do
              contenidoViejo <- B.readFile "ventas.json"
              let decoded = decode contenidoViejo :: Maybe [Venta]
              return (maybe [] id decoded)
            else return []

          let ventasActualizadas = ventasExistentes ++ validas

          -- Escribir en archivo temporal y luego renombrar
          let archivoTemp = "ventas_temp.json"
          B.writeFile archivoTemp (encode ventasActualizadas)
          renameFile archivoTemp "ventas.json"

          putStrLn $ "Se agregaron " ++ show (length validas) ++ " ventas correctamente."
          when (not (null invalidas)) $ do
            putStrLn "Se encontraron registros inválidos (faltan atributos o valores vacíos):"
            mapM_ print invalidas


-- Validar que tengan valores válidos
validarVentas :: [Venta] -> ([Venta], [Venta])
validarVentas = foldr (\v (ok, err) ->
  if camposCompletos v
     then (v:ok, err)
     else (ok, v:err)) ([], [])

camposCompletos :: Venta -> Bool
camposCompletos v =
  not (null (fecha v)
    || null (producto_nombre v)
    || null (categoria v))
  && venta_id v > 0
  && producto_id v > 0
  && cantidad v > 0
  && precio_unitario v > 0
  && total v >= 0
