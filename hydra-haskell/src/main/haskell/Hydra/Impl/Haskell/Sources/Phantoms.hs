{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Phantoms where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraPhantomsModule :: Module Meta
hydraPhantomsModule = Module hydraPhantoms [hydraCoreModule]

hydraPhantomsName :: GraphName
hydraPhantomsName = GraphName "hydra/phantoms"

hydraPhantoms :: Graph Meta
hydraPhantoms = Graph hydraPhantomsName elements hydraCoreName
  where
    core = nsref hydraCoreName
    phantoms = nsref hydraPhantomsName
    def = datatype hydraPhantomsName
    
    elements = [
      def "Case" $
        lambda "a" $ core "FieldName",
        
     def "Datum" $
       lambda "a" $ (core "Term") @@ (core "Meta"),
       
      def "Definition" $
        lambda "a" $ record [
          "name">: core "Name",
          "datum">: phantoms "Datum" @@ "a"],
          
     def "Reference" $
       lambda "a" $ unit]
