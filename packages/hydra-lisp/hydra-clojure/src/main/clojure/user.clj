(ns user
  "Bootstrap file that defines make-xxx constructor functions for all Hydra defrecords.
   Clojure's defrecord creates ->RecordName constructors, but the generated Hydra code
   uses make-recordName style (matching the Scheme convention). This file bridges the gap
   by interning each make-xxx function into clojure.core so it's available in all namespaces.")

(defn- register-make
  "Register a make-xxx function in clojure.core that constructs an instance of the given
   defrecord. ns-name is a symbol like 'hydra.core.model, class-name is a string like \"Application\",
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

;; ---- hydra.core.model ----
(register-make 'hydra.core.model "AnnotatedTerm" 'make-annotatedTerm)
(register-make 'hydra.core.model "AnnotatedType" 'make-annotatedType)
(register-make 'hydra.core.model "Application" 'make-application)
(register-make 'hydra.core.model "ApplicationType" 'make-applicationType)
(register-make 'hydra.core.model "Binding" 'make-binding)
(register-make 'hydra.core.model "CaseStatement" 'make-caseStatement)
(register-make 'hydra.core.model "EitherType" 'make-eitherType)
(register-make 'hydra.core.model "Field" 'make-field)
(register-make 'hydra.core.model "FieldType" 'make-fieldType)
(register-make 'hydra.core.model "ForallType" 'make-forallType)
(register-make 'hydra.core.model "FunctionType" 'make-functionType)
(register-make 'hydra.core.model "Injection" 'make-injection)
(register-make 'hydra.core.model "Lambda" 'make-lambda)
(register-make 'hydra.core.model "Let" 'make-let)
(register-make 'hydra.core.model "MapType" 'make-mapType)
(register-make 'hydra.core.model "Name" 'make-name)
(register-make 'hydra.core.model "PairType" 'make-pairType)
(register-make 'hydra.core.model "Projection" 'make-projection)
(register-make 'hydra.core.model "Record" 'make-record)
(register-make 'hydra.core.model "RowType" 'make-rowType)
(register-make 'hydra.core.model "TypeApplicationTerm" 'make-typeApplicationTerm)
(register-make 'hydra.core.model "TypeLambda" 'make-typeLambda)
(register-make 'hydra.core.model "TypeScheme" 'make-typeScheme)
(register-make 'hydra.core.model "TypeVariableConstraints" 'make-typeVariableConstraints)
(register-make 'hydra.core.model "WrappedTerm" 'make-wrappedTerm)
(register-make 'hydra.core.model "WrappedType" 'make-wrappedType)

;; ---- hydra.core.error ----
(register-make 'hydra.core.error "DecodingError" 'make-decodingError)
(register-make 'hydra.core.error "OtherError" 'make-otherError)
(register-make 'hydra.core.error "UnificationError" 'make-unificationError)

;; ---- hydra.core.graph ----
(register-make 'hydra.core.graph "Graph" 'make-graph)
(register-make 'hydra.core.graph "Primitive" 'make-primitive)
(register-make 'hydra.core.graph "TermCoder" 'make-termCoder)

;; ---- hydra.core.coders ----
(register-make 'hydra.core.coders "AdapterContext" 'make-adapterContext)
(register-make 'hydra.core.coders "Language" 'make-language)
(register-make 'hydra.core.coders "LanguageConstraints" 'make-languageConstraints)
(register-make 'hydra.core.coders "LanguageName" 'make-languageName)

;; ---- hydra.core.file ----
(register-make 'hydra.core.file "FileExtension" 'make-fileExtension)

;; ---- hydra.module ----
(register-make 'hydra.module "Library" 'make-library)
(register-make 'hydra.module "Module" 'make-module)
(register-make 'hydra.module "Namespace" 'make-namespace)
(register-make 'hydra.module "ModuleNames" 'make-moduleNames)
(register-make 'hydra.module "QualifiedName" 'make-qualifiedName)
(register-make 'hydra.module "TermDefinition" 'make-termDefinition)
(register-make 'hydra.module "TypeDefinition" 'make-typeDefinition)

;; ---- hydra.core.ast ----
(register-make 'hydra.core.ast "BlockStyle" 'make-blockStyle)
(register-make 'hydra.core.ast "BracketExpr" 'make-bracketExpr)
(register-make 'hydra.core.ast "Brackets" 'make-brackets)
(register-make 'hydra.core.ast "IndentedExpression" 'make-indentedExpression)
(register-make 'hydra.core.ast "Op" 'make-op)
(register-make 'hydra.core.ast "OpExpr" 'make-opExpr)
(register-make 'hydra.core.ast "Padding" 'make-padding)
(register-make 'hydra.core.ast "Precedence" 'make-precedence)
(register-make 'hydra.core.ast "Symbol" 'make-symbol)

;; ---- hydra.core.typed ----
(register-make 'hydra.core.typed "TypedBinding" 'make-typedBinding)
(register-make 'hydra.core.typed "TypedTerm" 'make-typedTerm)

;; ---- hydra.core.topology ----
(register-make 'hydra.core.topology "OrderingIsomorphism" 'make-orderingIsomorphism)
(register-make 'hydra.core.topology "TarjanState" 'make-tarjanState)

;; ---- hydra.core.typing ----
(register-make 'hydra.core.typing "FunctionStructure" 'make-functionStructure)
(register-make 'hydra.core.typing "InferenceResult" 'make-inferenceResult)
(register-make 'hydra.core.typing "TermSubst" 'make-termSubst)
(register-make 'hydra.core.typing "TypeConstraint" 'make-typeConstraint)
(register-make 'hydra.core.typing "TypeSubst" 'make-typeSubst)

;; ---- hydra.core.parsing ----
(register-make 'hydra.core.parsing "ParseError" 'make-parseError)
(register-make 'hydra.core.parsing "ParseSuccess" 'make-parseSuccess)
(register-make 'hydra.core.parsing "Parser" 'make-parser)

;; ---- hydra.core.query ----
(register-make 'hydra.core.query "Edge" 'make-edge)
(register-make 'hydra.core.query "GraphPattern" 'make-graphPattern)
(register-make 'hydra.core.query "PathEquation" 'make-pathEquation)
(register-make 'hydra.core.query "PatternImplication" 'make-patternImplication)
(register-make 'hydra.core.query "Query" 'make-query)
(register-make 'hydra.core.query "Range" 'make-range)
(register-make 'hydra.core.query "RegexSequence" 'make-regexSequence)
(register-make 'hydra.core.query "TriplePattern" 'make-triplePattern)
(register-make 'hydra.core.query "Variable" 'make-variable)

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

;; ---- hydra.core.relational ----
(register-make 'hydra.core.relational "ColumnName" 'make-columnName)
(register-make 'hydra.core.relational "ColumnSchema" 'make-columnSchema)
(register-make 'hydra.core.relational "ForeignKey" 'make-foreignKey)
(register-make 'hydra.core.relational "PrimaryKey" 'make-primaryKey)
(register-make 'hydra.core.relational "Relation" 'make-relation)
(register-make 'hydra.core.relational "RelationName" 'make-relationName)
(register-make 'hydra.core.relational "RelationSchema" 'make-relationSchema)
(register-make 'hydra.core.relational "Relationship" 'make-relationship)
(register-make 'hydra.core.relational "Row" 'make-row)

;; ---- hydra.core.tabular ----
(register-make 'hydra.core.tabular "ColumnType" 'make-columnType)
(register-make 'hydra.core.tabular "DataRow" 'make-dataRow)
(register-make 'hydra.core.tabular "HeaderRow" 'make-headerRow)
(register-make 'hydra.core.tabular "Table" 'make-table)
(register-make 'hydra.core.tabular "TableType" 'make-tableType)

;; ---- hydra.core.testing ----
(register-make 'hydra.core.testing "AlphaConversionTestCase" 'make-alphaConversionTestCase)
(register-make 'hydra.core.testing "CaseConversionTestCase" 'make-caseConversionTestCase)
(register-make 'hydra.core.testing "DeannotateTermTestCase" 'make-deannotateTermTestCase)
(register-make 'hydra.core.testing "DeannotateTypeTestCase" 'make-deannotateTypeTestCase)
(register-make 'hydra.core.testing "DelegatedEvaluationTestCase" 'make-delegatedEvaluationTestCase)
(register-make 'hydra.core.testing "EtaExpansionTestCase" 'make-etaExpansionTestCase)
(register-make 'hydra.core.testing "EvaluationTestCase" 'make-evaluationTestCase)
(register-make 'hydra.core.testing "FlattenLetTermsTestCase" 'make-flattenLetTermsTestCase)
(register-make 'hydra.core.testing "FoldOverTermTestCase" 'make-foldOverTermTestCase)
(register-make 'hydra.core.testing "FreeVariablesTestCase" 'make-freeVariablesTestCase)
(register-make 'hydra.core.testing "HoistCaseStatementsTestCase" 'make-hoistCaseStatementsTestCase)
(register-make 'hydra.core.testing "HoistLetBindingsTestCase" 'make-hoistLetBindingsTestCase)
(register-make 'hydra.core.testing "HoistPolymorphicLetBindingsTestCase" 'make-hoistPolymorphicLetBindingsTestCase)
(register-make 'hydra.core.testing "HoistSubtermsTestCase" 'make-hoistSubtermsTestCase)
(register-make 'hydra.core.testing "InferenceFailureTestCase" 'make-inferenceFailureTestCase)
(register-make 'hydra.core.testing "InferenceTestCase" 'make-inferenceTestCase)
(register-make 'hydra.core.testing "JoinTypesTestCase" 'make-joinTypesTestCase)
(register-make 'hydra.core.testing "JsonCoderTestCase" 'make-jsonCoderTestCase)
(register-make 'hydra.core.testing "JsonDecodeTestCase" 'make-jsonDecodeTestCase)
(register-make 'hydra.core.testing "JsonEncodeTestCase" 'make-jsonEncodeTestCase)
(register-make 'hydra.core.testing "JsonRoundtripTestCase" 'make-jsonRoundtripTestCase)
(register-make 'hydra.core.testing "LiftLambdaAboveLetTestCase" 'make-liftLambdaAboveLetTestCase)
(register-make 'hydra.core.testing "NormalizeTypeVariablesTestCase" 'make-normalizeTypeVariablesTestCase)
(register-make 'hydra.core.testing "ParserTestCase" 'make-parserTestCase)
(register-make 'hydra.core.testing "RewriteTermTestCase" 'make-rewriteTermTestCase)
(register-make 'hydra.core.testing "RewriteTypeTestCase" 'make-rewriteTypeTestCase)
(register-make 'hydra.core.testing "SerializationTestCase" 'make-serializationTestCase)
(register-make 'hydra.core.testing "SimplifyTermTestCase" 'make-simplifyTermTestCase)
(register-make 'hydra.core.testing "SubstInTypeTestCase" 'make-substInTypeTestCase)
(register-make 'hydra.core.testing "Tag" 'make-tag)
(register-make 'hydra.core.testing "TestCaseWithMetadata" 'make-testCaseWithMetadata)
(register-make 'hydra.core.testing "TestCodec" 'make-testCodec)
(register-make 'hydra.core.testing "TestGroup" 'make-testGroup)
(register-make 'hydra.core.testing "TopologicalSortBindingsTestCase" 'make-topologicalSortBindingsTestCase)
(register-make 'hydra.core.testing "TopologicalSortSCCTestCase" 'make-topologicalSortSCCTestCase)
(register-make 'hydra.core.testing "TopologicalSortTestCase" 'make-topologicalSortTestCase)
(register-make 'hydra.core.testing "TypeCheckingFailureTestCase" 'make-typeCheckingFailureTestCase)
(register-make 'hydra.core.testing "TypeCheckingTestCase" 'make-typeCheckingTestCase)
(register-make 'hydra.core.testing "TypeReductionTestCase" 'make-typeReductionTestCase)
(register-make 'hydra.core.testing "UnifyTypesTestCase" 'make-unifyTypesTestCase)
(register-make 'hydra.core.testing "UnshadowVariablesTestCase" 'make-unshadowVariablesTestCase)
(register-make 'hydra.core.testing "VariableOccursInTypeTestCase" 'make-variableOccursInTypeTestCase)
(register-make 'hydra.core.testing "WriterTestCase" 'make-writerTestCase)

;; Note: If you see "Unable to resolve symbol: make-xxx", add the corresponding
;; register-make entry here using the actual defrecord definition from the generated code.
;; Use: grep -rn 'defrecord Xxx' src/gen-main/ src/gen-test/ to find the right namespace.
