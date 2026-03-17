(ns user
  "Bootstrap file that defines make-xxx constructor functions for all Hydra defrecords.
   Clojure's defrecord creates ->RecordName constructors, but the generated Hydra code
   uses make-recordName style (matching the Scheme convention). This file bridges the gap
   by interning each make-xxx function into clojure.core so it's available in all namespaces.")

(defn- register-make
  "Register a make-xxx function in clojure.core that constructs an instance of the given
   defrecord. ns-name is a symbol like 'hydra.core, class-name is a string like \"Application\",
   make-sym is a symbol like 'make-application.
   Also registers the PascalCase version (make-ClassName) for generated code compatibility."
  [ns-name class-name make-sym]
  (try
    (require ns-name)
    (let [;; The ->ClassName positional factory fn is created by defrecord in the namespace
          factory-sym (symbol (str ns-name) (str "->" class-name))
          factory-fn (resolve factory-sym)]
      (when factory-fn
        (let [f (deref factory-fn)]
          (intern 'clojure.core make-sym f)
          ;; Also register PascalCase version: make-ClassName
          (let [pascal-sym (symbol (str "make-" class-name))]
            (when (not= pascal-sym make-sym)
              (intern 'clojure.core pascal-sym f))))))
    (catch Exception e
      (binding [*out* *err*]
        (println (str "Warning: failed to define " make-sym ": " (.getMessage e)))))))

;; ---- hydra.core ----
(register-make 'hydra.core "AnnotatedTerm" 'make-annotatedTerm)
(register-make 'hydra.core "AnnotatedType" 'make-annotatedType)
(register-make 'hydra.core "Application" 'make-application)
(register-make 'hydra.core "ApplicationType" 'make-applicationType)
(register-make 'hydra.core "Binding" 'make-binding)
(register-make 'hydra.core "CaseStatement" 'make-caseStatement)
(register-make 'hydra.core "EitherType" 'make-eitherType)
(register-make 'hydra.core "Field" 'make-field)
(register-make 'hydra.core "FieldType" 'make-fieldType)
(register-make 'hydra.core "ForallType" 'make-forallType)
(register-make 'hydra.core "FunctionType" 'make-functionType)
(register-make 'hydra.core "Injection" 'make-injection)
(register-make 'hydra.core "Lambda" 'make-lambda)
(register-make 'hydra.core "Let" 'make-let)
(register-make 'hydra.core "MapType" 'make-mapType)
(register-make 'hydra.core "Name" 'make-name)
(register-make 'hydra.core "PairType" 'make-pairType)
(register-make 'hydra.core "Projection" 'make-projection)
(register-make 'hydra.core "Record" 'make-record)
(register-make 'hydra.core "RowType" 'make-rowType)
(register-make 'hydra.core "TypeApplicationTerm" 'make-typeApplicationTerm)
(register-make 'hydra.core "TypeLambda" 'make-typeLambda)
(register-make 'hydra.core "TypeScheme" 'make-typeScheme)
(register-make 'hydra.core "TypeVariableMetadata" 'make-typeVariableMetadata)
(register-make 'hydra.core "WrappedTerm" 'make-wrappedTerm)
(register-make 'hydra.core "WrappedType" 'make-wrappedType)

;; ---- hydra.context ----
(register-make 'hydra.context "Context" 'make-context)
(register-make 'hydra.context "InContext" 'make-inContext)

;; ---- hydra.error ----
(register-make 'hydra.error "DecodingError" 'make-decodingError)
(register-make 'hydra.error "OtherError" 'make-otherError)
(register-make 'hydra.error "UnificationError" 'make-unificationError)

;; ---- hydra.graph ----
(register-make 'hydra.graph "Graph" 'make-graph)
(register-make 'hydra.graph "Primitive" 'make-primitive)
(register-make 'hydra.graph "TermCoder" 'make-termCoder)

;; ---- hydra.compute ----
(register-make 'hydra.compute "Adapter" 'make-adapter)
(register-make 'hydra.compute "Bicoder" 'make-bicoder)
(register-make 'hydra.compute "Coder" 'make-coder)

;; ---- hydra.coders ----
(register-make 'hydra.coders "AdapterContext" 'make-adapterContext)
(register-make 'hydra.coders "Language" 'make-language)
(register-make 'hydra.coders "LanguageConstraints" 'make-languageConstraints)
(register-make 'hydra.coders "LanguageName" 'make-languageName)

;; ---- hydra.module ----
(register-make 'hydra.module "FileExtension" 'make-fileExtension)
(register-make 'hydra.module "Library" 'make-library)
(register-make 'hydra.module "Module" 'make-module)
(register-make 'hydra.module "Namespace" 'make-namespace)
(register-make 'hydra.module "Namespaces" 'make-namespaces)
(register-make 'hydra.module "QualifiedName" 'make-qualifiedName)
(register-make 'hydra.module "TermDefinition" 'make-termDefinition)
(register-make 'hydra.module "TypeDefinition" 'make-typeDefinition)

;; ---- hydra.ast ----
(register-make 'hydra.ast "BlockStyle" 'make-blockStyle)
(register-make 'hydra.ast "BracketExpr" 'make-bracketExpr)
(register-make 'hydra.ast "Brackets" 'make-brackets)
(register-make 'hydra.ast "IndentedExpression" 'make-indentedExpression)
(register-make 'hydra.ast "Op" 'make-op)
(register-make 'hydra.ast "OpExpr" 'make-opExpr)
(register-make 'hydra.ast "Padding" 'make-padding)
(register-make 'hydra.ast "Precedence" 'make-precedence)
(register-make 'hydra.ast "Symbol" 'make-symbol)

;; ---- hydra.phantoms ----
(register-make 'hydra.phantoms "TBinding" 'make-tBinding)
(register-make 'hydra.phantoms "TTerm" 'make-tTerm)

;; ---- hydra.topology ----
(register-make 'hydra.topology "OrderingIsomorphism" 'make-orderingIsomorphism)
(register-make 'hydra.topology "TarjanState" 'make-tarjanState)

;; ---- hydra.typing ----
(register-make 'hydra.typing "FunctionStructure" 'make-functionStructure)
(register-make 'hydra.typing "InferenceResult" 'make-inferenceResult)
(register-make 'hydra.typing "TermSubst" 'make-termSubst)
(register-make 'hydra.typing "TypeConstraint" 'make-typeConstraint)
(register-make 'hydra.typing "TypeSubst" 'make-typeSubst)

;; ---- hydra.parsing ----
(register-make 'hydra.parsing "ParseError" 'make-parseError)
(register-make 'hydra.parsing "ParseSuccess" 'make-parseSuccess)
(register-make 'hydra.parsing "Parser" 'make-parser)

;; ---- hydra.workflow ----
(register-make 'hydra.workflow "HydraSchemaSpec" 'make-hydraSchemaSpec)
(register-make 'hydra.workflow "TransformWorkflow" 'make-transformWorkflow)

;; ---- hydra.query ----
(register-make 'hydra.query "Edge" 'make-edge)
(register-make 'hydra.query "GraphPattern" 'make-graphPattern)
(register-make 'hydra.query "PathEquation" 'make-pathEquation)
(register-make 'hydra.query "PatternImplication" 'make-patternImplication)
(register-make 'hydra.query "Query" 'make-query)
(register-make 'hydra.query "Range" 'make-range)
(register-make 'hydra.query "RegexSequence" 'make-regexSequence)
(register-make 'hydra.query "TriplePattern" 'make-triplePattern)
(register-make 'hydra.query "Variable" 'make-variable)

;; ---- hydra.accessors ----
(register-make 'hydra.accessors "AccessorEdge" 'make-accessorEdge)
(register-make 'hydra.accessors "AccessorGraph" 'make-accessorGraph)
(register-make 'hydra.accessors "AccessorNode" 'make-accessorNode)
(register-make 'hydra.accessors "AccessorPath" 'make-accessorPath)

;; ---- hydra.grammar ----
(register-make 'hydra.grammar "Constant" 'make-constant)
(register-make 'hydra.grammar "Grammar" 'make-grammar)
(register-make 'hydra.grammar "Label" 'make-label)
(register-make 'hydra.grammar "LabeledPattern" 'make-labeledPattern)
(register-make 'hydra.grammar "Production" 'make-production)
(register-make 'hydra.grammar "Regex" 'make-regex)

;; ---- hydra.relational ----
(register-make 'hydra.relational "ColumnName" 'make-columnName)
(register-make 'hydra.relational "ColumnSchema" 'make-columnSchema)
(register-make 'hydra.relational "ForeignKey" 'make-foreignKey)
(register-make 'hydra.relational "PrimaryKey" 'make-primaryKey)
(register-make 'hydra.relational "Relation" 'make-relation)
(register-make 'hydra.relational "RelationName" 'make-relationName)
(register-make 'hydra.relational "RelationSchema" 'make-relationSchema)
(register-make 'hydra.relational "Relationship" 'make-relationship)
(register-make 'hydra.relational "Row" 'make-row)

;; ---- hydra.tabular ----
(register-make 'hydra.tabular "ColumnType" 'make-columnType)
(register-make 'hydra.tabular "DataRow" 'make-dataRow)
(register-make 'hydra.tabular "HeaderRow" 'make-headerRow)
(register-make 'hydra.tabular "Table" 'make-table)
(register-make 'hydra.tabular "TableType" 'make-tableType)

;; ---- hydra.testing ----
(register-make 'hydra.testing "AlphaConversionTestCase" 'make-alphaConversionTestCase)
(register-make 'hydra.testing "CaseConversionTestCase" 'make-caseConversionTestCase)
(register-make 'hydra.testing "DeannotateTermTestCase" 'make-deannotateTermTestCase)
(register-make 'hydra.testing "DeannotateTypeTestCase" 'make-deannotateTypeTestCase)
(register-make 'hydra.testing "DelegatedEvaluationTestCase" 'make-delegatedEvaluationTestCase)
(register-make 'hydra.testing "EtaExpansionTestCase" 'make-etaExpansionTestCase)
(register-make 'hydra.testing "EvaluationTestCase" 'make-evaluationTestCase)
(register-make 'hydra.testing "FlattenLetTermsTestCase" 'make-flattenLetTermsTestCase)
(register-make 'hydra.testing "FoldOverTermTestCase" 'make-foldOverTermTestCase)
(register-make 'hydra.testing "FreeVariablesTestCase" 'make-freeVariablesTestCase)
(register-make 'hydra.testing "HoistCaseStatementsTestCase" 'make-hoistCaseStatementsTestCase)
(register-make 'hydra.testing "HoistLetBindingsTestCase" 'make-hoistLetBindingsTestCase)
(register-make 'hydra.testing "HoistPolymorphicLetBindingsTestCase" 'make-hoistPolymorphicLetBindingsTestCase)
(register-make 'hydra.testing "HoistSubtermsTestCase" 'make-hoistSubtermsTestCase)
(register-make 'hydra.testing "InferenceFailureTestCase" 'make-inferenceFailureTestCase)
(register-make 'hydra.testing "InferenceTestCase" 'make-inferenceTestCase)
(register-make 'hydra.testing "JoinTypesTestCase" 'make-joinTypesTestCase)
(register-make 'hydra.testing "JsonCoderTestCase" 'make-jsonCoderTestCase)
(register-make 'hydra.testing "JsonDecodeTestCase" 'make-jsonDecodeTestCase)
(register-make 'hydra.testing "JsonEncodeTestCase" 'make-jsonEncodeTestCase)
(register-make 'hydra.testing "JsonRoundtripTestCase" 'make-jsonRoundtripTestCase)
(register-make 'hydra.testing "LiftLambdaAboveLetTestCase" 'make-liftLambdaAboveLetTestCase)
(register-make 'hydra.testing "NormalizeTypeVariablesTestCase" 'make-normalizeTypeVariablesTestCase)
(register-make 'hydra.testing "ParserTestCase" 'make-parserTestCase)
(register-make 'hydra.testing "RewriteTermTestCase" 'make-rewriteTermTestCase)
(register-make 'hydra.testing "RewriteTypeTestCase" 'make-rewriteTypeTestCase)
(register-make 'hydra.testing "SerializationTestCase" 'make-serializationTestCase)
(register-make 'hydra.testing "SimplifyTermTestCase" 'make-simplifyTermTestCase)
(register-make 'hydra.testing "SubstInTypeTestCase" 'make-substInTypeTestCase)
(register-make 'hydra.testing "Tag" 'make-tag)
(register-make 'hydra.testing "TestCaseWithMetadata" 'make-testCaseWithMetadata)
(register-make 'hydra.testing "TestCodec" 'make-testCodec)
(register-make 'hydra.testing "TestGroup" 'make-testGroup)
(register-make 'hydra.testing "TopologicalSortBindingsTestCase" 'make-topologicalSortBindingsTestCase)
(register-make 'hydra.testing "TopologicalSortSCCTestCase" 'make-topologicalSortSCCTestCase)
(register-make 'hydra.testing "TopologicalSortTestCase" 'make-topologicalSortTestCase)
(register-make 'hydra.testing "TypeCheckingFailureTestCase" 'make-typeCheckingFailureTestCase)
(register-make 'hydra.testing "TypeCheckingTestCase" 'make-typeCheckingTestCase)
(register-make 'hydra.testing "TypeReductionTestCase" 'make-typeReductionTestCase)
(register-make 'hydra.testing "UnifyTypesTestCase" 'make-unifyTypesTestCase)
(register-make 'hydra.testing "UnshadowVariablesTestCase" 'make-unshadowVariablesTestCase)
(register-make 'hydra.testing "VariableOccursInTypeTestCase" 'make-variableOccursInTypeTestCase)
(register-make 'hydra.testing "WriterTestCase" 'make-writerTestCase)

;; Note: If you see "Unable to resolve symbol: make-xxx", add the corresponding
;; register-make entry here using the actual defrecord definition from the generated code.
;; Use: grep -rn 'defrecord Xxx' src/gen-main/ src/gen-test/ to find the right namespace.
