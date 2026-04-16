{-# LANGUAGE ScopedTypeVariables #-}
-- | Haskell driver for the SHACL demo.
--
-- Generates SHACL shapes from the hydra.module.Module type, encodes kernel
-- modules from JSON as conforming RDF data, and writes both to N-Triples files.
--
-- Usage: runhaskell Demo.hs <json-dir> <output-dir>
--
-- Where:
--   <json-dir>    is the path to dist/json/hydra-kernel/src/main/json/
--   <output-dir>  is the directory for output N-Triples files

module Hydra.Demos.Shacl.Demo where

import Hydra.Kernel
import Hydra.Generation (modulesToGraph, loadModulesFromJson, readManifestField)
import Hydra.Sources.All (kernelModules)
import Hydra.Module.Compat (moduleBindings)

import qualified Hydra.Shacl.Coder as ShaclCoder
import qualified Hydra.Rdf.Syntax as Rdf
import qualified Hydra.Shacl.Model as Shacl
import qualified Hydra.Rdf.Utils as RdfUtils
import qualified Hydra.Rdf.Serde as Serde
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Packaging as EncodePackaging
import qualified Hydra.Context as Context
import qualified Hydra.Lexical as Lexical

import Hydra.Demos.Shacl.ShaclRdf (shapesGraphToTriples)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Exception (catch, SomeException, evaluate)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, hFlush, stdout)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonDir, outDir] -> runDemo jsonDir outDir
    _ -> do
      hPutStrLn stderr "Usage: ShaclDemo <json-dir> <output-dir>"
      exitFailure

runDemo :: FilePath -> FilePath -> IO ()
runDemo jsonDir outDir = do
  createDirectoryIfMissing True outDir

  -- Step 1: Build a graph from the kernel type modules
  putStrLn "Step 1: Building graph from kernel type modules..."
  let graph = modulesToGraph kernelModules kernelModules
  let cx = Lexical.emptyContext

  -- Step 2: Generate SHACL shapes from kernel type elements, skipping unsupported types
  putStrLn "Step 2: Generating SHACL shapes from kernel type modules..."
  startTime <- getCPUTime
  let allTypeEls = concatMap (\m -> filter isNativeType (moduleBindings m)) kernelModules
  let (shapes, nSkipped) = encodeShapes cx graph allTypeEls
  let allTriples = shapesGraphToTriples shapes
  let shapesNt = triplesToNtriples allTriples
  let shapesFile = outDir </> "shapes.nt"
  writeFile shapesFile shapesNt
  shapesEnd <- getCPUTime
  putStrLn $ "  Generated " ++ show (S.size (Shacl.unShapesGraph shapes)) ++ " shapes (" ++ show nSkipped ++ " types skipped)"
  putStrLn $ "  Wrote " ++ show (length allTriples) ++ " triples to " ++ shapesFile
  putStrLn $ "  Time: " ++ showTimeMs (shapesEnd - startTime)

  -- Step 3: Load kernel modules from JSON and encode as RDF
  putStrLn "Step 3: Loading kernel modules from JSON and encoding as RDF..."
  dataStart <- getCPUTime
  kernelNamespaces <- readManifestField jsonDir "mainModules"
  -- Load the first type-defining modules (those whose types we encoded as shapes)
  let typeNs = take 15 kernelNamespaces
  loadedModules <- loadModulesFromJson jsonDir kernelModules typeNs

  -- Encode each loaded module *as a Module term* into RDF
  (allDataDescs, nDataSkipped) <- encodeModulesAsRdf cx graph loadedModules
  let dataNt = Serde.rdfGraphToNtriples $ RdfUtils.descriptionsToGraph allDataDescs
  let dataFile = outDir </> "data.nt"
  writeFile dataFile dataNt
  dataEnd <- getCPUTime
  putStrLn $ "  Encoded " ++ show (length allDataDescs) ++ " descriptions (" ++ show nDataSkipped ++ " elements skipped)"
  putStrLn $ "  Wrote to " ++ dataFile
  putStrLn $ "  Time: " ++ showTimeMs (dataEnd - dataStart)

  -- Step 4: Generate intentionally non-conforming RDF
  putStrLn "Step 4: Generating non-conforming RDF data..."
  let invalidNt = generateInvalidData
  let invalidFile = outDir </> "invalid.nt"
  writeFile invalidFile invalidNt
  putStrLn $ "  Wrote to " ++ invalidFile

  -- Summary
  let totalMs = showTimeMs (dataEnd - startTime)
  putStrLn ""
  putStrLn $ "Done. Output files in " ++ outDir
  putStrLn $ "  shapes.nt:  SHACL shapes graph (" ++ show (S.size (Shacl.unShapesGraph shapes)) ++ " shapes, " ++ show (length allTriples) ++ " triples)"
  putStrLn $ "  data.nt:    conforming RDF data (" ++ show (length allDataDescs) ++ " descriptions)"
  putStrLn $ "  invalid.nt: non-conforming RDF data"
  hPutStrLn stderr $ "HYDRA_TIME_MS=" ++ totalMs

-- | Encode type elements as SHACL shapes, skipping types that the SHACL language
-- doesn't support (function types, type variables, etc.). Returns the shapes graph
-- and the count of skipped elements.
encodeShapes :: Context.Context -> Graph -> [Binding] -> (Shacl.ShapesGraph, Int)
encodeShapes cx graph elements =
  let results = map (encodeOneShape cx graph) elements
      successes = [d | Right d <- results]
      failures = length [() | Left _ <- results]
  in (Shacl.ShapesGraph (S.fromList successes), failures)

-- | Try to encode a single type element as a SHACL Definition<Shape>.
-- Replicates the toShape logic from shaclCoder but returns Either for error recovery.
encodeOneShape :: Context.Context -> Graph -> Binding -> Either String (Shacl.Definition Shacl.Shape)
encodeOneShape cx graph el =
  case DecodeCore.type_ graph (bindingTerm el) of
    Left de -> Left $ "decode error: " ++ unDecodingError de
    Right typ ->
      case ShaclCoder.encodeType (bindingName el) typ cx of
        Left _ic -> Left $ "encode error for " ++ unName (bindingName el)
        Right cp -> Right $ Shacl.Definition {
          Shacl.definitionIri = RdfUtils.nameToIri (bindingName el),
          Shacl.definitionTarget = Shacl.ShapeNode (Shacl.NodeShape cp) }

-- | Encode loaded modules as RDF. Each Module is simplified (element terms stripped)
-- then converted to a term and encoded as RDF via the SHACL term encoder.
-- This is necessary because Module elements can contain arbitrary Hydra terms
-- (functions, type definitions) that the SHACL term encoder doesn't support.
encodeModulesAsRdf :: Context.Context -> Graph -> [Module] -> IO ([Rdf.Description], Int)
encodeModulesAsRdf cx graph mods = go cx mods [] 0
  where
    go _ [] descs nSkip = return (descs, nSkip)
    go cx' (m:ms) descs nSkip = do
      let ns = unNamespace (moduleNamespace m)
          subject = Rdf.ResourceIri $ Rdf.Iri ("urn:hydra:module:" ++ ns)
          simplified = simplifyModule m
          moduleTerm = EncodePackaging.module_ simplified
      result <- tryEncode subject moduleTerm cx' graph
      case result of
        Nothing -> do
          hPutStrLn stderr $ "  Skipped: " ++ ns
          go cx' ms descs (nSkip + 1)
        Just (mDescs, cx'') -> do
          putStrLn $ "  Encoded: " ++ ns ++ " (" ++ show (length mDescs) ++ " descriptions)"
          go cx'' ms (descs ++ mDescs) nSkip

-- | Simplify a module for RDF encoding by replacing each element's term body
-- with a string literal of the element name. This preserves the module structure
-- (namespace, element names, type schemes, dependencies, description) while
-- avoiding unsupported term variants (functions, type applications, etc.).
simplifyModule :: Module -> Module
simplifyModule m = m { moduleDefinitions = map simplifyDef (moduleDefinitions m) }
  where
    simplifyDef (DefinitionTerm td) = DefinitionTerm $ td {
      termDefinitionTerm = TermLiteral (LiteralString (unName (termDefinitionName td))) }
    simplifyDef d = d

-- | Try to encode a term as RDF, catching both Either errors and exceptions.
tryEncode :: Rdf.Resource -> Term -> Context.Context -> Graph
          -> IO (Maybe ([Rdf.Description], Context.Context))
tryEncode subject term cx graph =
  catch
    (case ShaclCoder.encodeTerm subject term cx graph of
      Left _ic -> return Nothing
      Right (descs, cx') -> do
        -- Force full evaluation to catch lazy exceptions
        let nt = Serde.rdfGraphToNtriples $ RdfUtils.descriptionsToGraph descs
        _ <- evaluate (length nt)
        return (Just (descs, cx')))
    (\(_e :: SomeException) -> return Nothing)

-- | Generate non-conforming RDF data that should fail SHACL validation.
-- Creates instances typed as kernel types but with missing required fields,
-- wrong value types, etc.
generateInvalidData :: String
generateInvalidData = triplesToNtriples $
  -- A Module missing its required "namespace" field
  [ triple "urn:invalid:module1" (rdfType) "urn:hydra.module.Module"
  , tripleL "urn:invalid:module1" "urn:hydra.module#moduleDescription" "A module with no namespace"
  ] ++
  -- A Namespace with a non-string value (integer instead of string)
  [ triple "urn:invalid:ns1" (rdfType) "urn:hydra.module.Namespace"
  , tripleInt "urn:invalid:ns1" "urn:hydra.module#namespace" 42
  ] ++
  -- A QualifiedName missing its required "local" field
  [ triple "urn:invalid:qn1" (rdfType) "urn:hydra.module.QualifiedName"
  ]
  where
    rdfType = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

    triple s p o = Rdf.Triple
      (Rdf.ResourceIri (Rdf.Iri s))
      (Rdf.Iri p)
      (Rdf.NodeIri (Rdf.Iri o))

    tripleL s p v = Rdf.Triple
      (Rdf.ResourceIri (Rdf.Iri s))
      (Rdf.Iri p)
      (Rdf.NodeLiteral (Rdf.Literal v (Rdf.Iri "http://www.w3.org/2001/XMLSchema#string") Nothing))

    tripleInt s p n = Rdf.Triple
      (Rdf.ResourceIri (Rdf.Iri s))
      (Rdf.Iri p)
      (Rdf.NodeLiteral (Rdf.Literal (show n) (Rdf.Iri "http://www.w3.org/2001/XMLSchema#integer") Nothing))

-- | Convert triples to N-Triples string via Description intermediary
triplesToNtriples :: [Rdf.Triple] -> String
triplesToNtriples triples =
  Serde.rdfGraphToNtriples $ Rdf.Graph (S.fromList triples)

-- | Format picoseconds as milliseconds string
showTimeMs :: Integer -> String
showTimeMs ps = show (fromIntegral ps / 1e9 :: Double)
