;;; Auto-generated alist-compatible struct accessors
;;; Do not edit manually
(in-package :cl-user)

#+sbcl (sb-ext:unlock-package :cl)

;; AccessorEdge -> accessor_edge
(defun make-accessor_edge (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:source :path :target) args)))
(defun accessor_edge-source (rec) (cdr (assoc :source rec)))
(defun accessor_edge-path (rec) (cdr (assoc :path rec)))
(defun accessor_edge-target (rec) (cdr (assoc :target rec)))

;; AccessorGraph -> accessor_graph
(defun make-accessor_graph (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:nodes :edges) args)))
(defun accessor_graph-nodes (rec) (cdr (assoc :nodes rec)))
(defun accessor_graph-edges (rec) (cdr (assoc :edges rec)))

;; AccessorNode -> accessor_node
(defun make-accessor_node (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :label :id) args)))
(defun accessor_node-name (rec) (cdr (assoc :name rec)))
(defun accessor_node-label (rec) (cdr (assoc :label rec)))
(defun accessor_node-id (rec) (cdr (assoc :id rec)))

;; AccessorPath -> accessor_path
(defun make-accessor_path (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun accessor_path-value (rec) (cdr (assoc :value rec)))

;; Adapter -> adapter
(defun make-adapter (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:is_lossy :source :target :coder) args)))
(defun adapter-is_lossy (rec) (cdr (assoc :is_lossy rec)))
(defun adapter-source (rec) (cdr (assoc :source rec)))
(defun adapter-target (rec) (cdr (assoc :target rec)))
(defun adapter-coder (rec) (cdr (assoc :coder rec)))

;; AdapterContext -> adapter_context
(defun make-adapter_context (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:graph :language :adapters) args)))
(defun adapter_context-graph (rec) (cdr (assoc :graph rec)))
(defun adapter_context-language (rec) (cdr (assoc :language rec)))
(defun adapter_context-adapters (rec) (cdr (assoc :adapters rec)))

;; AlphaConversionTestCase -> alpha_conversion_test_case
(defun make-alpha_conversion_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:term :old_variable :new_variable :result) args)))
(defun alpha_conversion_test_case-term (rec) (cdr (assoc :term rec)))
(defun alpha_conversion_test_case-old_variable (rec) (cdr (assoc :old_variable rec)))
(defun alpha_conversion_test_case-new_variable (rec) (cdr (assoc :new_variable rec)))
(defun alpha_conversion_test_case-result (rec) (cdr (assoc :result rec)))

;; AnnotatedTerm -> annotated_term
(defun make-annotated_term (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:body :annotation) args)))
(defun annotated_term-body (rec) (cdr (assoc :body rec)))
(defun annotated_term-annotation (rec) (cdr (assoc :annotation rec)))

;; AnnotatedType -> annotated_type
(defun make-annotated_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:body :annotation) args)))
(defun annotated_type-body (rec) (cdr (assoc :body rec)))
(defun annotated_type-annotation (rec) (cdr (assoc :annotation rec)))

;; Application -> application
(defun make-application (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:function :argument) args)))
(defun application-function (rec) (cdr (assoc :function rec)))
(defun application-argument (rec) (cdr (assoc :argument rec)))

;; ApplicationType -> application_type
(defun make-application_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:function :argument) args)))
(defun application_type-function (rec) (cdr (assoc :function rec)))
(defun application_type-argument (rec) (cdr (assoc :argument rec)))

;; Bicoder -> bicoder
(defun make-bicoder (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:encode :decode) args)))
(defun bicoder-encode (rec) (cdr (assoc :encode rec)))
(defun bicoder-decode (rec) (cdr (assoc :decode rec)))

;; Binding -> binding
(defun make-binding (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :term :type) args)))
(defun binding-name (rec) (cdr (assoc :name rec)))
(defun binding-term (rec) (cdr (assoc :term rec)))
(defun binding-type (rec) (cdr (assoc :type rec)))

;; BlockStyle -> block_style
(defun make-block_style (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:indent :newline_before_content :newline_after_content) args)))
(defun block_style-indent (rec) (cdr (assoc :indent rec)))
(defun block_style-newline_before_content (rec) (cdr (assoc :newline_before_content rec)))
(defun block_style-newline_after_content (rec) (cdr (assoc :newline_after_content rec)))

;; BracketExpr -> bracket_expr
(defun make-bracket_expr (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:brackets :enclosed :style) args)))
(defun bracket_expr-brackets (rec) (cdr (assoc :brackets rec)))
(defun bracket_expr-enclosed (rec) (cdr (assoc :enclosed rec)))
(defun bracket_expr-style (rec) (cdr (assoc :style rec)))

;; Brackets -> brackets
(defun make-brackets (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:open :close) args)))
(defun brackets-open (rec) (cdr (assoc :open rec)))
(defun brackets-close (rec) (cdr (assoc :close rec)))

;; CaseConversionTestCase -> case_conversion_test_case
(defun make-case_conversion_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:from_convention :to_convention :from_string :to_string) args)))
(defun case_conversion_test_case-from_convention (rec) (cdr (assoc :from_convention rec)))
(defun case_conversion_test_case-to_convention (rec) (cdr (assoc :to_convention rec)))
(defun case_conversion_test_case-from_string (rec) (cdr (assoc :from_string rec)))
(defun case_conversion_test_case-to_string (rec) (cdr (assoc :to_string rec)))

;; CaseStatement -> case_statement
(defun make-case_statement (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type_name :default :cases) args)))
(defun case_statement-type_name (rec) (cdr (assoc :type_name rec)))
(defun case_statement-default (rec) (cdr (assoc :default rec)))
(defun case_statement-cases (rec) (cdr (assoc :cases rec)))

;; Coder -> coder
(defun make-coder (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:encode :decode) args)))
(defun coder-encode (rec) (cdr (assoc :encode rec)))
(defun coder-decode (rec) (cdr (assoc :decode rec)))

;; ColumnName -> column_name
(defun make-column_name (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun column_name-value (rec) (cdr (assoc :value rec)))

;; ColumnSchema -> column_schema
(defun make-column_schema (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :domain) args)))
(defun column_schema-name (rec) (cdr (assoc :name rec)))
(defun column_schema-domain (rec) (cdr (assoc :domain rec)))

;; ColumnType -> column_type
(defun make-column_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :type) args)))
(defun column_type-name (rec) (cdr (assoc :name rec)))
(defun column_type-type (rec) (cdr (assoc :type rec)))

;; Constant -> constant
(defun make-constant (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun constant-value (rec) (cdr (assoc :value rec)))

;; Context -> context
(defun make-context (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:trace :messages :other) args)))
(defun context-trace (rec) (cdr (assoc :trace rec)))
(defun context-messages (rec) (cdr (assoc :messages rec)))
(defun context-other (rec) (cdr (assoc :other rec)))

;; DataRow -> data_row
(defun make-data_row (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun data_row-value (rec) (cdr (assoc :value rec)))

;; DeannotateTermTestCase -> deannotate_term_test_case
(defun make-deannotate_term_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun deannotate_term_test_case-input (rec) (cdr (assoc :input rec)))
(defun deannotate_term_test_case-output (rec) (cdr (assoc :output rec)))

;; DeannotateTypeTestCase -> deannotate_type_test_case
(defun make-deannotate_type_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun deannotate_type_test_case-input (rec) (cdr (assoc :input rec)))
(defun deannotate_type_test_case-output (rec) (cdr (assoc :output rec)))

;; DecodingError -> decoding_error
(defun make-decoding_error (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun decoding_error-value (rec) (cdr (assoc :value rec)))

;; DelegatedEvaluationTestCase -> delegated_evaluation_test_case
(defun make-delegated_evaluation_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun delegated_evaluation_test_case-input (rec) (cdr (assoc :input rec)))
(defun delegated_evaluation_test_case-output (rec) (cdr (assoc :output rec)))

;; Edge -> edge
(defun make-edge (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type :out :in) args)))
(defun edge-type (rec) (cdr (assoc :type rec)))
(defun edge-out (rec) (cdr (assoc :out rec)))
(defun edge-in (rec) (cdr (assoc :in rec)))

;; EitherType -> either_type
(defun make-either_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:left :right) args)))
(defun either_type-left (rec) (cdr (assoc :left rec)))
(defun either_type-right (rec) (cdr (assoc :right rec)))

;; EtaExpansionTestCase -> eta_expansion_test_case
(defun make-eta_expansion_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun eta_expansion_test_case-input (rec) (cdr (assoc :input rec)))
(defun eta_expansion_test_case-output (rec) (cdr (assoc :output rec)))

;; EvaluationTestCase -> evaluation_test_case
(defun make-evaluation_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:evaluation_style :input :output) args)))
(defun evaluation_test_case-evaluation_style (rec) (cdr (assoc :evaluation_style rec)))
(defun evaluation_test_case-input (rec) (cdr (assoc :input rec)))
(defun evaluation_test_case-output (rec) (cdr (assoc :output rec)))

;; Field -> field
(defun make-field (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :term) args)))
(defun field-name (rec) (cdr (assoc :name rec)))
(defun field-term (rec) (cdr (assoc :term rec)))

;; FieldType -> field_type
(defun make-field_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :type) args)))
(defun field_type-name (rec) (cdr (assoc :name rec)))
(defun field_type-type (rec) (cdr (assoc :type rec)))

;; FileExtension -> file_extension
(defun make-file_extension (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun file_extension-value (rec) (cdr (assoc :value rec)))

;; FlattenLetTermsTestCase -> flatten_let_terms_test_case
(defun make-flatten_let_terms_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun flatten_let_terms_test_case-input (rec) (cdr (assoc :input rec)))
(defun flatten_let_terms_test_case-output (rec) (cdr (assoc :output rec)))

;; FoldOverTermTestCase -> fold_over_term_test_case
(defun make-fold_over_term_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :traversal_order :operation :output) args)))
(defun fold_over_term_test_case-input (rec) (cdr (assoc :input rec)))
(defun fold_over_term_test_case-traversal_order (rec) (cdr (assoc :traversal_order rec)))
(defun fold_over_term_test_case-operation (rec) (cdr (assoc :operation rec)))
(defun fold_over_term_test_case-output (rec) (cdr (assoc :output rec)))

;; ForallType -> forall_type
(defun make-forall_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:parameter :body) args)))
(defun forall_type-parameter (rec) (cdr (assoc :parameter rec)))
(defun forall_type-body (rec) (cdr (assoc :body rec)))

;; ForeignKey -> foreign_key
(defun make-foreign_key (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:foreign_relation :keys) args)))
(defun foreign_key-foreign_relation (rec) (cdr (assoc :foreign_relation rec)))
(defun foreign_key-keys (rec) (cdr (assoc :keys rec)))

;; FreeVariablesTestCase -> free_variables_test_case
(defun make-free_variables_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun free_variables_test_case-input (rec) (cdr (assoc :input rec)))
(defun free_variables_test_case-output (rec) (cdr (assoc :output rec)))

;; FunctionStructure -> function_structure
(defun make-function_structure (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type_params :params :bindings :body :domains :codomain :environment) args)))
(defun function_structure-type_params (rec) (cdr (assoc :type_params rec)))
(defun function_structure-params (rec) (cdr (assoc :params rec)))
(defun function_structure-bindings (rec) (cdr (assoc :bindings rec)))
(defun function_structure-body (rec) (cdr (assoc :body rec)))
(defun function_structure-domains (rec) (cdr (assoc :domains rec)))
(defun function_structure-codomain (rec) (cdr (assoc :codomain rec)))
(defun function_structure-environment (rec) (cdr (assoc :environment rec)))

;; FunctionType -> function_type
(defun make-function_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:domain :codomain) args)))
(defun function_type-domain (rec) (cdr (assoc :domain rec)))
(defun function_type-codomain (rec) (cdr (assoc :codomain rec)))

;; Grammar -> grammar
(defun make-grammar (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun grammar-value (rec) (cdr (assoc :value rec)))

;; Graph -> graph
(defun make-graph (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:bound_terms :bound_types :class_constraints :lambda_variables :metadata :primitives :schema_types :type_variables) args)))
(defun graph-bound_terms (rec) (cdr (assoc :bound_terms rec)))
(defun graph-bound_types (rec) (cdr (assoc :bound_types rec)))
(defun graph-class_constraints (rec) (cdr (assoc :class_constraints rec)))
(defun graph-lambda_variables (rec) (cdr (assoc :lambda_variables rec)))
(defun graph-metadata (rec) (cdr (assoc :metadata rec)))
(defun graph-primitives (rec) (cdr (assoc :primitives rec)))
(defun graph-schema_types (rec) (cdr (assoc :schema_types rec)))
(defun graph-type_variables (rec) (cdr (assoc :type_variables rec)))

;; GraphPattern -> graph_pattern
(defun make-graph_pattern (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:graph :patterns) args)))
(defun graph_pattern-graph (rec) (cdr (assoc :graph rec)))
(defun graph_pattern-patterns (rec) (cdr (assoc :patterns rec)))

;; HeaderRow -> header_row
(defun make-header_row (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun header_row-value (rec) (cdr (assoc :value rec)))

;; HoistCaseStatementsTestCase -> hoist_case_statements_test_case
(defun make-hoist_case_statements_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun hoist_case_statements_test_case-input (rec) (cdr (assoc :input rec)))
(defun hoist_case_statements_test_case-output (rec) (cdr (assoc :output rec)))

;; HoistLetBindingsTestCase -> hoist_let_bindings_test_case
(defun make-hoist_let_bindings_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun hoist_let_bindings_test_case-input (rec) (cdr (assoc :input rec)))
(defun hoist_let_bindings_test_case-output (rec) (cdr (assoc :output rec)))

;; HoistPolymorphicLetBindingsTestCase -> hoist_polymorphic_let_bindings_test_case
(defun make-hoist_polymorphic_let_bindings_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun hoist_polymorphic_let_bindings_test_case-input (rec) (cdr (assoc :input rec)))
(defun hoist_polymorphic_let_bindings_test_case-output (rec) (cdr (assoc :output rec)))

;; HoistSubtermsTestCase -> hoist_subterms_test_case
(defun make-hoist_subterms_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:predicate :input :output) args)))
(defun hoist_subterms_test_case-predicate (rec) (cdr (assoc :predicate rec)))
(defun hoist_subterms_test_case-input (rec) (cdr (assoc :input rec)))
(defun hoist_subterms_test_case-output (rec) (cdr (assoc :output rec)))

;; HydraSchemaSpec -> hydra_schema_spec
(defun make-hydra_schema_spec (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:modules :type_name) args)))
(defun hydra_schema_spec-modules (rec) (cdr (assoc :modules rec)))
(defun hydra_schema_spec-type_name (rec) (cdr (assoc :type_name rec)))

;; InContext -> in_context
(defun make-in_context (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:object :context) args)))
(defun in_context-object (rec) (cdr (assoc :object rec)))
(defun in_context-context (rec) (cdr (assoc :context rec)))

;; IndentedExpression -> indented_expression
(defun make-indented_expression (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:style :expr) args)))
(defun indented_expression-style (rec) (cdr (assoc :style rec)))
(defun indented_expression-expr (rec) (cdr (assoc :expr rec)))

;; InferenceFailureTestCase -> inference_failure_test_case
(defun make-inference_failure_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input) args)))
(defun inference_failure_test_case-input (rec) (cdr (assoc :input rec)))

;; InferenceResult -> inference_result
(defun make-inference_result (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:term :type :subst :class_constraints :context) args)))
(defun inference_result-term (rec) (cdr (assoc :term rec)))
(defun inference_result-type (rec) (cdr (assoc :type rec)))
(defun inference_result-subst (rec) (cdr (assoc :subst rec)))
(defun inference_result-class_constraints (rec) (cdr (assoc :class_constraints rec)))
(defun inference_result-context (rec) (cdr (assoc :context rec)))

;; InferenceTestCase -> inference_test_case
(defun make-inference_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun inference_test_case-input (rec) (cdr (assoc :input rec)))
(defun inference_test_case-output (rec) (cdr (assoc :output rec)))

;; Injection -> injection
(defun make-injection (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type_name :field) args)))
(defun injection-type_name (rec) (cdr (assoc :type_name rec)))
(defun injection-field (rec) (cdr (assoc :field rec)))

;; JoinTypesTestCase -> join_types_test_case
(defun make-join_types_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:left :right :expected) args)))
(defun join_types_test_case-left (rec) (cdr (assoc :left rec)))
(defun join_types_test_case-right (rec) (cdr (assoc :right rec)))
(defun join_types_test_case-expected (rec) (cdr (assoc :expected rec)))

;; JsonCoderTestCase -> json_coder_test_case
(defun make-json_coder_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type :term :json) args)))
(defun json_coder_test_case-type (rec) (cdr (assoc :type rec)))
(defun json_coder_test_case-term (rec) (cdr (assoc :term rec)))
(defun json_coder_test_case-json (rec) (cdr (assoc :json rec)))

;; JsonDecodeTestCase -> json_decode_test_case
(defun make-json_decode_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type :json :expected) args)))
(defun json_decode_test_case-type (rec) (cdr (assoc :type rec)))
(defun json_decode_test_case-json (rec) (cdr (assoc :json rec)))
(defun json_decode_test_case-expected (rec) (cdr (assoc :expected rec)))

;; JsonEncodeTestCase -> json_encode_test_case
(defun make-json_encode_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:term :expected) args)))
(defun json_encode_test_case-term (rec) (cdr (assoc :term rec)))
(defun json_encode_test_case-expected (rec) (cdr (assoc :expected rec)))

;; JsonRoundtripTestCase -> json_roundtrip_test_case
(defun make-json_roundtrip_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type :term) args)))
(defun json_roundtrip_test_case-type (rec) (cdr (assoc :type rec)))
(defun json_roundtrip_test_case-term (rec) (cdr (assoc :term rec)))

;; Label -> label
(defun make-label (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun label-value (rec) (cdr (assoc :value rec)))

;; LabeledPattern -> labeled_pattern
(defun make-labeled_pattern (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:label :pattern) args)))
(defun labeled_pattern-label (rec) (cdr (assoc :label rec)))
(defun labeled_pattern-pattern (rec) (cdr (assoc :pattern rec)))

;; Lambda -> lambda
(defun make-lambda (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:parameter :domain :body) args)))
(defun lambda-parameter (rec) (cdr (assoc :parameter rec)))
(defun lambda-domain (rec) (cdr (assoc :domain rec)))
(defun lambda-body (rec) (cdr (assoc :body rec)))

;; Language -> language
(defun make-language (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :constraints) args)))
(defun language-name (rec) (cdr (assoc :name rec)))
(defun language-constraints (rec) (cdr (assoc :constraints rec)))

;; LanguageConstraints -> language_constraints
(defun make-language_constraints (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:elimination_variants :literal_variants :float_types :function_variants :integer_types :term_variants :type_variants :types) args)))
(defun language_constraints-elimination_variants (rec) (cdr (assoc :elimination_variants rec)))
(defun language_constraints-literal_variants (rec) (cdr (assoc :literal_variants rec)))
(defun language_constraints-float_types (rec) (cdr (assoc :float_types rec)))
(defun language_constraints-function_variants (rec) (cdr (assoc :function_variants rec)))
(defun language_constraints-integer_types (rec) (cdr (assoc :integer_types rec)))
(defun language_constraints-term_variants (rec) (cdr (assoc :term_variants rec)))
(defun language_constraints-type_variants (rec) (cdr (assoc :type_variants rec)))
(defun language_constraints-types (rec) (cdr (assoc :types rec)))

;; LanguageName -> language_name
(defun make-language_name (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun language_name-value (rec) (cdr (assoc :value rec)))

;; Let -> let
(defun make-let (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:bindings :body) args)))
(defun let-bindings (rec) (cdr (assoc :bindings rec)))
(defun let-body (rec) (cdr (assoc :body rec)))

;; Library -> library
(defun make-library (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:namespace :prefix :primitives) args)))
(defun library-namespace (rec) (cdr (assoc :namespace rec)))
(defun library-prefix (rec) (cdr (assoc :prefix rec)))
(defun library-primitives (rec) (cdr (assoc :primitives rec)))

;; LiftLambdaAboveLetTestCase -> lift_lambda_above_let_test_case
(defun make-lift_lambda_above_let_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun lift_lambda_above_let_test_case-input (rec) (cdr (assoc :input rec)))
(defun lift_lambda_above_let_test_case-output (rec) (cdr (assoc :output rec)))

;; MapType -> map_type
(defun make-map_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:keys :values) args)))
(defun map_type-keys (rec) (cdr (assoc :keys rec)))
(defun map_type-values (rec) (cdr (assoc :values rec)))

;; Module -> module
(defun make-module (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:namespace :definitions :term_dependencies :type_dependencies :description) args)))
(defun module-namespace (rec) (cdr (assoc :namespace rec)))
(defun module-elements (rec) (cdr (assoc :definitions rec)))
(defun module-term_dependencies (rec) (cdr (assoc :term_dependencies rec)))
(defun module-type_dependencies (rec) (cdr (assoc :type_dependencies rec)))
(defun module-description (rec) (cdr (assoc :description rec)))

;; Name -> name
(defun make-name (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun name-value (rec) (cdr (assoc :value rec)))

;; Namespace -> namespace
(defun make-namespace (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun namespace-value (rec) (cdr (assoc :value rec)))

;; Namespaces -> namespaces
(defun make-namespaces (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:focus :mapping) args)))
(defun namespaces-focus (rec) (cdr (assoc :focus rec)))
(defun namespaces-mapping (rec) (cdr (assoc :mapping rec)))

;; NormalizeTypeVariablesTestCase -> normalize_type_variables_test_case
(defun make-normalize_type_variables_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun normalize_type_variables_test_case-input (rec) (cdr (assoc :input rec)))
(defun normalize_type_variables_test_case-output (rec) (cdr (assoc :output rec)))

;; Op -> op
(defun make-op (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:symbol :padding :precedence :associativity) args)))
(defun op-symbol (rec) (cdr (assoc :symbol rec)))
(defun op-padding (rec) (cdr (assoc :padding rec)))
(defun op-precedence (rec) (cdr (assoc :precedence rec)))
(defun op-associativity (rec) (cdr (assoc :associativity rec)))

;; OpExpr -> op_expr
(defun make-op_expr (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:op :lhs :rhs) args)))
(defun op_expr-op (rec) (cdr (assoc :op rec)))
(defun op_expr-lhs (rec) (cdr (assoc :lhs rec)))
(defun op_expr-rhs (rec) (cdr (assoc :rhs rec)))

;; OrderingIsomorphism -> ordering_isomorphism
(defun make-ordering_isomorphism (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:encode :decode) args)))
(defun ordering_isomorphism-encode (rec) (cdr (assoc :encode rec)))
(defun ordering_isomorphism-decode (rec) (cdr (assoc :decode rec)))

;; OtherError -> other_error
(defun make-other_error (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun other_error-value (rec) (cdr (assoc :value rec)))

;; Padding -> padding
(defun make-padding (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:left :right) args)))
(defun padding-left (rec) (cdr (assoc :left rec)))
(defun padding-right (rec) (cdr (assoc :right rec)))

;; PairType -> pair_type
(defun make-pair_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:first :second) args)))
(defun pair_type-first (rec) (cdr (assoc :first rec)))
(defun pair_type-second (rec) (cdr (assoc :second rec)))

;; ParseError -> parse_error
(defun make-parse_error (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:message :remainder) args)))
(defun parse_error-message (rec) (cdr (assoc :message rec)))
(defun parse_error-remainder (rec) (cdr (assoc :remainder rec)))

;; ParseSuccess -> parse_success
(defun make-parse_success (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value :remainder) args)))
(defun parse_success-value (rec) (cdr (assoc :value rec)))
(defun parse_success-remainder (rec) (cdr (assoc :remainder rec)))

;; Parser -> parser
(defun make-parser (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun parser-value (rec) (cdr (assoc :value rec)))

;; ParserTestCase -> parser_test_case
(defun make-parser_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun parser_test_case-input (rec) (cdr (assoc :input rec)))
(defun parser_test_case-output (rec) (cdr (assoc :output rec)))

;; PathEquation -> path_equation
(defun make-path_equation (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:left :right) args)))
(defun path_equation-left (rec) (cdr (assoc :left rec)))
(defun path_equation-right (rec) (cdr (assoc :right rec)))

;; PatternImplication -> pattern_implication
(defun make-pattern_implication (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:antecedent :consequent) args)))
(defun pattern_implication-antecedent (rec) (cdr (assoc :antecedent rec)))
(defun pattern_implication-consequent (rec) (cdr (assoc :consequent rec)))

;; Precedence -> precedence
(defun make-precedence (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun precedence-value (rec) (cdr (assoc :value rec)))

;; PrimaryKey -> primary_key
(defun make-primary_key (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun primary_key-value (rec) (cdr (assoc :value rec)))

;; Primitive -> primitive
(defun make-primitive (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :type :implementation) args)))
(defun primitive-name (rec) (cdr (assoc :name rec)))
(defun primitive-type (rec) (cdr (assoc :type rec)))
(defun primitive-implementation (rec) (cdr (assoc :implementation rec)))

;; Production -> production
(defun make-production (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:symbol :pattern) args)))
(defun production-symbol (rec) (cdr (assoc :symbol rec)))
(defun production-pattern (rec) (cdr (assoc :pattern rec)))

;; Projection -> projection
(defun make-projection (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type_name :field) args)))
(defun projection-type_name (rec) (cdr (assoc :type_name rec)))
(defun projection-field (rec) (cdr (assoc :field rec)))

;; QualifiedName -> qualified_name
(defun make-qualified_name (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:namespace :local) args)))
(defun qualified_name-namespace (rec) (cdr (assoc :namespace rec)))
(defun qualified_name-local (rec) (cdr (assoc :local rec)))

;; Query -> query
(defun make-query (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:variables :patterns) args)))
(defun query-variables (rec) (cdr (assoc :variables rec)))
(defun query-patterns (rec) (cdr (assoc :patterns rec)))

;; Range -> range
(defun make-range (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:min :max) args)))
(defun range-min (rec) (cdr (assoc :min rec)))
(defun range-max (rec) (cdr (assoc :max rec)))

;; Record -> record
(defun make-record (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type_name :fields) args)))
(defun record-type_name (rec) (cdr (assoc :type_name rec)))
(defun record-fields (rec) (cdr (assoc :fields rec)))

;; Regex -> regex
(defun make-regex (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun regex-value (rec) (cdr (assoc :value rec)))

;; RegexSequence -> regex_sequence
(defun make-regex_sequence (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:path :quantifier) args)))
(defun regex_sequence-path (rec) (cdr (assoc :path rec)))
(defun regex_sequence-quantifier (rec) (cdr (assoc :quantifier rec)))

;; Relation -> relation
(defun make-relation (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun relation-value (rec) (cdr (assoc :value rec)))

;; RelationName -> relation_name
(defun make-relation_name (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun relation_name-value (rec) (cdr (assoc :value rec)))

;; RelationSchema -> relation_schema
(defun make-relation_schema (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :columns :primary_keys :foreign_keys) args)))
(defun relation_schema-name (rec) (cdr (assoc :name rec)))
(defun relation_schema-columns (rec) (cdr (assoc :columns rec)))
(defun relation_schema-primary_keys (rec) (cdr (assoc :primary_keys rec)))
(defun relation_schema-foreign_keys (rec) (cdr (assoc :foreign_keys rec)))

;; Relationship -> relationship
(defun make-relationship (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun relationship-value (rec) (cdr (assoc :value rec)))

;; RewriteTermTestCase -> rewrite_term_test_case
(defun make-rewrite_term_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :rewriter :output) args)))
(defun rewrite_term_test_case-input (rec) (cdr (assoc :input rec)))
(defun rewrite_term_test_case-rewriter (rec) (cdr (assoc :rewriter rec)))
(defun rewrite_term_test_case-output (rec) (cdr (assoc :output rec)))

;; RewriteTypeTestCase -> rewrite_type_test_case
(defun make-rewrite_type_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :rewriter :output) args)))
(defun rewrite_type_test_case-input (rec) (cdr (assoc :input rec)))
(defun rewrite_type_test_case-rewriter (rec) (cdr (assoc :rewriter rec)))
(defun rewrite_type_test_case-output (rec) (cdr (assoc :output rec)))

;; Row -> row
(defun make-row (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun row-value (rec) (cdr (assoc :value rec)))

;; RowType -> row_type
(defun make-row_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type_name :fields) args)))
(defun row_type-type_name (rec) (cdr (assoc :type_name rec)))
(defun row_type-fields (rec) (cdr (assoc :fields rec)))

;; SerializationTestCase -> serialization_test_case
(defun make-serialization_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun serialization_test_case-input (rec) (cdr (assoc :input rec)))
(defun serialization_test_case-output (rec) (cdr (assoc :output rec)))

;; SimplifyTermTestCase -> simplify_term_test_case
(defun make-simplify_term_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun simplify_term_test_case-input (rec) (cdr (assoc :input rec)))
(defun simplify_term_test_case-output (rec) (cdr (assoc :output rec)))

;; SubstInTypeTestCase -> subst_in_type_test_case
(defun make-subst_in_type_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:substitution :input :output) args)))
(defun subst_in_type_test_case-substitution (rec) (cdr (assoc :substitution rec)))
(defun subst_in_type_test_case-input (rec) (cdr (assoc :input rec)))
(defun subst_in_type_test_case-output (rec) (cdr (assoc :output rec)))

;; Symbol -> symbol
(defun make-symbol (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun symbol-value (rec) (cdr (assoc :value rec)))

;; TBinding -> t_binding
(defun make-t_binding (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :term) args)))
(defun t_binding-name (rec) (cdr (assoc :name rec)))
(defun t_binding-term (rec) (cdr (assoc :term rec)))

;; TTerm -> t_term
(defun make-t_term (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun t_term-value (rec) (cdr (assoc :value rec)))

;; Table -> table
(defun make-table (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:header :data) args)))
(defun table-header (rec) (cdr (assoc :header rec)))
(defun table-data (rec) (cdr (assoc :data rec)))

;; TableType -> table_type
(defun make-table_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :columns) args)))
(defun table_type-name (rec) (cdr (assoc :name rec)))
(defun table_type-columns (rec) (cdr (assoc :columns rec)))

;; Tag -> tag
(defun make-tag (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun tag-value (rec) (cdr (assoc :value rec)))

;; TarjanState -> tarjan_state
(defun make-tarjan_state (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:counter :indices :low_links :stack :on_stack :sccs) args)))
(defun tarjan_state-counter (rec) (cdr (assoc :counter rec)))
(defun tarjan_state-indices (rec) (cdr (assoc :indices rec)))
(defun tarjan_state-low_links (rec) (cdr (assoc :low_links rec)))
(defun tarjan_state-stack (rec) (cdr (assoc :stack rec)))
(defun tarjan_state-on_stack (rec) (cdr (assoc :on_stack rec)))
(defun tarjan_state-sccs (rec) (cdr (assoc :sccs rec)))

;; TermCoder -> term_coder
(defun make-term_coder (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type :encode :decode) args)))
(defun term_coder-type (rec) (cdr (assoc :type rec)))
(defun term_coder-encode (rec) (cdr (assoc :encode rec)))
(defun term_coder-decode (rec) (cdr (assoc :decode rec)))

;; TermDefinition -> term_definition
(defun make-term_definition (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :term :type) args)))
(defun term_definition-name (rec) (cdr (assoc :name rec)))
(defun term_definition-term (rec) (cdr (assoc :term rec)))
(defun term_definition-type (rec) (cdr (assoc :type rec)))

;; TermSubst -> term_subst
(defun make-term_subst (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun term_subst-value (rec) (cdr (assoc :value rec)))

;; TestCaseWithMetadata -> test_case_with_metadata
(defun make-test_case_with_metadata (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :case :description :tags) args)))
(defun test_case_with_metadata-name (rec) (cdr (assoc :name rec)))
(defun test_case_with_metadata-case (rec) (cdr (assoc :case rec)))
(defun test_case_with_metadata-description (rec) (cdr (assoc :description rec)))
(defun test_case_with_metadata-tags (rec) (cdr (assoc :tags rec)))

;; TestCodec -> test_codec
(defun make-test_codec (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:language :file_extension :encode_term :encode_type :format_test_name :format_module_name :test_case_template :test_group_template :module_template :import_template :find_imports) args)))
(defun test_codec-language (rec) (cdr (assoc :language rec)))
(defun test_codec-file_extension (rec) (cdr (assoc :file_extension rec)))
(defun test_codec-encode_term (rec) (cdr (assoc :encode_term rec)))
(defun test_codec-encode_type (rec) (cdr (assoc :encode_type rec)))
(defun test_codec-format_test_name (rec) (cdr (assoc :format_test_name rec)))
(defun test_codec-format_module_name (rec) (cdr (assoc :format_module_name rec)))
(defun test_codec-test_case_template (rec) (cdr (assoc :test_case_template rec)))
(defun test_codec-test_group_template (rec) (cdr (assoc :test_group_template rec)))
(defun test_codec-module_template (rec) (cdr (assoc :module_template rec)))
(defun test_codec-import_template (rec) (cdr (assoc :import_template rec)))
(defun test_codec-find_imports (rec) (cdr (assoc :find_imports rec)))

;; TestGroup -> test_group
(defun make-test_group (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :description :subgroups :cases) args)))
(defun test_group-name (rec) (cdr (assoc :name rec)))
(defun test_group-description (rec) (cdr (assoc :description rec)))
(defun test_group-subgroups (rec) (cdr (assoc :subgroups rec)))
(defun test_group-cases (rec) (cdr (assoc :cases rec)))

;; TopologicalSortBindingsTestCase -> topological_sort_bindings_test_case
(defun make-topological_sort_bindings_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:bindings :expected) args)))
(defun topological_sort_bindings_test_case-bindings (rec) (cdr (assoc :bindings rec)))
(defun topological_sort_bindings_test_case-expected (rec) (cdr (assoc :expected rec)))

;; TopologicalSortSCCTestCase -> topological_sort_scc_test_case
(defun make-topological_sort_scc_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:adjacency_list :expected) args)))
(defun topological_sort_scc_test_case-adjacency_list (rec) (cdr (assoc :adjacency_list rec)))
(defun topological_sort_scc_test_case-expected (rec) (cdr (assoc :expected rec)))

;; TopologicalSortTestCase -> topological_sort_test_case
(defun make-topological_sort_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:adjacency_list :expected) args)))
(defun topological_sort_test_case-adjacency_list (rec) (cdr (assoc :adjacency_list rec)))
(defun topological_sort_test_case-expected (rec) (cdr (assoc :expected rec)))

;; TransformWorkflow -> transform_workflow
(defun make-transform_workflow (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :schema_spec :src_dir :dest_dir) args)))
(defun transform_workflow-name (rec) (cdr (assoc :name rec)))
(defun transform_workflow-schema_spec (rec) (cdr (assoc :schema_spec rec)))
(defun transform_workflow-src_dir (rec) (cdr (assoc :src_dir rec)))
(defun transform_workflow-dest_dir (rec) (cdr (assoc :dest_dir rec)))

;; TriplePattern -> triple_pattern
(defun make-triple_pattern (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:subject :predicate :object) args)))
(defun triple_pattern-subject (rec) (cdr (assoc :subject rec)))
(defun triple_pattern-predicate (rec) (cdr (assoc :predicate rec)))
(defun triple_pattern-object (rec) (cdr (assoc :object rec)))

;; TypeApplicationTerm -> type_application_term
(defun make-type_application_term (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:body :type) args)))
(defun type_application_term-body (rec) (cdr (assoc :body rec)))
(defun type_application_term-type (rec) (cdr (assoc :type rec)))

;; TypeCheckingFailureTestCase -> type_checking_failure_test_case
(defun make-type_checking_failure_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input) args)))
(defun type_checking_failure_test_case-input (rec) (cdr (assoc :input rec)))

;; TypeCheckingTestCase -> type_checking_test_case
(defun make-type_checking_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output_term :output_type) args)))
(defun type_checking_test_case-input (rec) (cdr (assoc :input rec)))
(defun type_checking_test_case-output_term (rec) (cdr (assoc :output_term rec)))
(defun type_checking_test_case-output_type (rec) (cdr (assoc :output_type rec)))

;; TypeConstraint -> type_constraint
(defun make-type_constraint (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:left :right :comment) args)))
(defun type_constraint-left (rec) (cdr (assoc :left rec)))
(defun type_constraint-right (rec) (cdr (assoc :right rec)))
(defun type_constraint-comment (rec) (cdr (assoc :comment rec)))

;; TypeDefinition -> type_definition
(defun make-type_definition (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:name :type) args)))
(defun type_definition-name (rec) (cdr (assoc :name rec)))
(defun type_definition-type (rec) (cdr (assoc :type rec)))

;; TypeLambda -> type_lambda
(defun make-type_lambda (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:parameter :body) args)))
(defun type_lambda-parameter (rec) (cdr (assoc :parameter rec)))
(defun type_lambda-body (rec) (cdr (assoc :body rec)))

;; TypeReductionTestCase -> type_reduction_test_case
(defun make-type_reduction_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun type_reduction_test_case-input (rec) (cdr (assoc :input rec)))
(defun type_reduction_test_case-output (rec) (cdr (assoc :output rec)))

;; TypeScheme -> type_scheme
(defun make-type_scheme (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:variables :type :constraints) args)))
(defun type_scheme-variables (rec) (cdr (assoc :variables rec)))
(defun type_scheme-type (rec) (cdr (assoc :type rec)))
(defun type_scheme-constraints (rec) (cdr (assoc :constraints rec)))

;; TypeSubst -> type_subst
(defun make-type_subst (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun type_subst-value (rec) (cdr (assoc :value rec)))

;; TypeVariableMetadata -> type_variable_metadata
(defun make-type_variable_metadata (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:classes) args)))
(defun type_variable_metadata-classes (rec) (cdr (assoc :classes rec)))

;; UnificationError -> unification_error
(defun make-unification_error (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:left_type :right_type :message) args)))
(defun unification_error-left_type (rec) (cdr (assoc :left_type rec)))
(defun unification_error-right_type (rec) (cdr (assoc :right_type rec)))
(defun unification_error-message (rec) (cdr (assoc :message rec)))

;; UnifyTypesTestCase -> unify_types_test_case
(defun make-unify_types_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:schema_types :left :right :expected) args)))
(defun unify_types_test_case-schema_types (rec) (cdr (assoc :schema_types rec)))
(defun unify_types_test_case-left (rec) (cdr (assoc :left rec)))
(defun unify_types_test_case-right (rec) (cdr (assoc :right rec)))
(defun unify_types_test_case-expected (rec) (cdr (assoc :expected rec)))

;; UnshadowVariablesTestCase -> unshadow_variables_test_case
(defun make-unshadow_variables_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun unshadow_variables_test_case-input (rec) (cdr (assoc :input rec)))
(defun unshadow_variables_test_case-output (rec) (cdr (assoc :output rec)))

;; Variable -> variable
(defun make-variable (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:value) args)))
(defun variable-value (rec) (cdr (assoc :value rec)))

;; VariableOccursInTypeTestCase -> variable_occurs_in_type_test_case
(defun make-variable_occurs_in_type_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:variable :type :expected) args)))
(defun variable_occurs_in_type_test_case-variable (rec) (cdr (assoc :variable rec)))
(defun variable_occurs_in_type_test_case-type (rec) (cdr (assoc :type rec)))
(defun variable_occurs_in_type_test_case-expected (rec) (cdr (assoc :expected rec)))

;; WrappedTerm -> wrapped_term
(defun make-wrapped_term (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type_name :body) args)))
(defun wrapped_term-type_name (rec) (cdr (assoc :type_name rec)))
(defun wrapped_term-body (rec) (cdr (assoc :body rec)))

;; WrappedType -> wrapped_type
(defun make-wrapped_type (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:type_name :body) args)))
(defun wrapped_type-type_name (rec) (cdr (assoc :type_name rec)))
(defun wrapped_type-body (rec) (cdr (assoc :body rec)))

;; WriterTestCase -> writer_test_case
(defun make-writer_test_case (&rest args)
  (if (and args (keywordp (first args)))
    (loop for (k v) on args by #'cddr collect (cons k v))
    (mapcar #'cons '(:input :output) args)))
(defun writer_test_case-input (rec) (cdr (assoc :input rec)))
(defun writer_test_case-output (rec) (cdr (assoc :output rec)))

#+sbcl (sb-ext:lock-package :cl)
;;; End of generated compatibility definitions
