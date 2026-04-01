(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.constants)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.ext.haskell.coder)

(require 'hydra.ext.haskell.serde)

(require 'hydra.ext.haskell.syntax)

(require 'hydra.ext.haskell.utils)

(require 'hydra.formatting)

(require 'hydra.graph)

(require 'hydra.inference)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.names)

(require 'hydra.rewriting)

(require 'hydra.schemas)

(require 'hydra.serialization)

(require 'hydra.show.errors)

(require 'hydra.substitution)

(require 'hydra.testing)

(require 'hydra.typing)

(require 'hydra.util)

(defvar hydra_ext_haskell_testing_add_namespaces_to_namespaces (lambda (ns0) (lambda (names) (let* ((new_namespaces (hydra_lib_sets_from_list (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map hydra_names_namespace_of) (hydra_lib_sets_to_list names))))) (to_module_name (lambda (namespace) (hydra_formatting_capitalize (hydra_lib_lists_last (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) namespace)))))) (new_mappings (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (ns_) (list ns_ (to_module_name ns_)))) (hydra_lib_sets_to_list new_namespaces))))) (make-hydra_module_namespaces (funcall (lambda (v) (hydra_module_namespaces-focus v)) ns0) (funcall (hydra_lib_maps_union (funcall (lambda (v) (hydra_module_namespaces-mapping v)) ns0)) new_mappings))))))

(defvar hydra_ext_haskell_testing_collect_test_cases (lambda (tg) (funcall (hydra_lib_lists_concat2 (funcall (lambda (v) (hydra_testing_test_group-cases v)) tg)) (hydra_lib_lists_concat (funcall (hydra_lib_lists_map hydra_ext_haskell_testing_collect_test_cases) (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) tg))))))

(defvar hydra_ext_haskell_testing_collect_names (lambda (graf) (lambda (names) (lambda (t_) (if (hydra_schemas_is_encoded_term (hydra_rewriting_deannotate_term t_)) (funcall (funcall (hydra_lib_eithers_either (lambda (_) names)) (lambda (decoded_term) (funcall (hydra_lib_sets_union names) (funcall (funcall (funcall (hydra_rewriting_term_dependency_names t) t) t) decoded_term)))) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) _e)) (lambda (_a) _a)) (funcall (hydra_decode_core_term graf) t_))) names)))))

(defvar hydra_ext_haskell_testing_extract_encoded_term_variable_names (lambda (graf) (lambda (term) (funcall (funcall (funcall (hydra_rewriting_fold_over_term (list :pre nil)) (hydra_ext_haskell_testing_collect_names graf)) hydra_lib_sets_empty) term))))

(defvar hydra_ext_haskell_testing_extract_test_terms (lambda (tcm) (list)))

(defvar hydra_ext_haskell_testing_build_namespaces_for_test_group (lambda (mod) (lambda (tgroup) (lambda (graph_) (let* ((test_cases_ (hydra_ext_haskell_testing_collect_test_cases tgroup)) (test_terms (hydra_lib_lists_concat (funcall (hydra_lib_lists_map hydra_ext_haskell_testing_extract_test_terms) test_cases_))) (test_bindings (funcall (hydra_lib_lists_map (lambda (term) (make-hydra_core_binding "_test_" term (list :nothing)))) test_terms)) (temp_module (make-hydra_module_module (funcall (lambda (v) (hydra_module_module-namespace v)) mod) (funcall (hydra_lib_lists_map (lambda (b) (list :term (make-hydra_module_term_definition (funcall (lambda (v) (hydra_core_binding-name v)) b) (funcall (lambda (v) (hydra_core_binding-term v)) b) (funcall (lambda (v) (hydra_core_binding-type v)) b))))) test_bindings) (funcall (lambda (v) (hydra_module_module-term_dependencies v)) mod) (funcall (lambda (v) (hydra_module_module-type_dependencies v)) mod) (funcall (lambda (v) (hydra_module_module-description v)) mod)))) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (ic) (hydra_show_errors_error (funcall (lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (a) a)) (funcall (funcall (hydra_ext_haskell_utils_namespaces_for_module temp_module) hydra_lexical_empty_context) graph_))) (lambda (base_namespaces) (let ((encoded_names (hydra_lib_sets_unions (funcall (hydra_lib_lists_map (lambda (t_) (funcall (hydra_ext_haskell_testing_extract_encoded_term_variable_names graph_) t_))) test_terms)))) (list :right (funcall (hydra_ext_haskell_testing_add_namespaces_to_namespaces base_namespaces) encoded_names))))))))))

(defvar hydra_ext_haskell_testing_build_test_module_with_codec (lambda (codec) (lambda (test_module) (lambda (test_group) (lambda (test_body) (lambda (namespaces) (let* ((domain_imports (funcall (funcall (lambda (v) (hydra_testing_test_codec-find_imports v)) codec) hydra_lib_sets_empty)) (standard_imports (list "import Hydra.Kernel" "import qualified Test.Hspec as H" "import qualified Data.List as L" "import qualified Data.Map as M" "import qualified Data.Set as S" "import qualified Data.Maybe as Y")) (all_imports (funcall (hydra_lib_lists_concat2 standard_imports) domain_imports)) (debug_comments (list "-- DEBUG: Focus namespace = (see generated module)" "-- DEBUG: Namespace mappings: (see generated module)")) (group_name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) test_group)) (ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) test_module)) (spec_ns (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) ns_)) "Spec")) (module_name_string (funcall (funcall (lambda (v) (hydra_testing_test_codec-format_module_name v)) codec) spec_ns)) (header (funcall (hydra_lib_strings_intercalate "\n") (hydra_lib_lists_concat (list (list (funcall (hydra_lib_strings_cat2 "-- ") hydra_constants_warning_auto_generated_file) "") debug_comments (list "" (hydra_lib_strings_cat (list "module " module_name_string " where")) "") all_imports (list "" "spec :: H.Spec" (hydra_lib_strings_cat (list "spec = H.describe " (hydra_lib_literals_show_string group_name_) " $ do")))))))) (hydra_lib_strings_cat (list header "\n" test_body "\n")))))))))

(defvar hydra_ext_haskell_testing_contains_trivially_polymorphic (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :list) (funcall (lambda (xs) (funcall (hydra_lib_logic_or (hydra_lib_lists_null xs)) (funcall (funcall (hydra_lib_lists_foldl hydra_lib_logic_or) nil) (funcall (hydra_lib_lists_map hydra_ext_haskell_testing_contains_trivially_polymorphic) xs)))) match_value)) ((equal (car match_target) :set) (funcall (lambda (s) (funcall (hydra_lib_logic_or (hydra_lib_sets_null s)) (funcall (funcall (hydra_lib_lists_foldl hydra_lib_logic_or) nil) (funcall (hydra_lib_lists_map hydra_ext_haskell_testing_contains_trivially_polymorphic) (hydra_lib_sets_to_list s))))) match_value)) ((equal (car match_target) :map) (funcall (lambda (m) (funcall (hydra_lib_logic_or (hydra_lib_maps_null m)) (funcall (hydra_lib_logic_or (funcall (funcall (hydra_lib_lists_foldl hydra_lib_logic_or) nil) (funcall (hydra_lib_lists_map hydra_ext_haskell_testing_contains_trivially_polymorphic) (hydra_lib_maps_keys m)))) (funcall (funcall (hydra_lib_lists_foldl hydra_lib_logic_or) nil) (funcall (hydra_lib_lists_map hydra_ext_haskell_testing_contains_trivially_polymorphic) (funcall (hydra_lib_lists_map (lambda (p) (hydra_lib_pairs_second p))) (hydra_lib_maps_to_list m))))))) match_value)) ((equal (car match_target) :maybe) (funcall (lambda (mx) (funcall (funcall (hydra_lib_maybes_maybe (lambda () t)) hydra_ext_haskell_testing_contains_trivially_polymorphic) mx)) match_value)) ((equal (car match_target) :either) (funcall (lambda (_) t) match_value)) ((equal (car match_target) :union) (funcall (lambda (inj) (hydra_ext_haskell_testing_contains_trivially_polymorphic (funcall (lambda (v) (hydra_core_field-term v)) (funcall (lambda (v) (hydra_core_injection-field v)) inj)))) match_value)) ((equal (car match_target) :pair) (funcall (lambda (p) (funcall (hydra_lib_logic_or (hydra_ext_haskell_testing_contains_trivially_polymorphic (hydra_lib_pairs_first p))) (hydra_ext_haskell_testing_contains_trivially_polymorphic (hydra_lib_pairs_second p)))) match_value)) ((equal (car match_target) :record) (funcall (lambda (rec) (funcall (funcall (hydra_lib_lists_foldl hydra_lib_logic_or) nil) (funcall (hydra_lib_lists_map (lambda (f) (hydra_ext_haskell_testing_contains_trivially_polymorphic (funcall (lambda (v) (hydra_core_field-term v)) f)))) (funcall (lambda (v) (hydra_core_record-fields v)) rec)))) match_value)) ((equal (car match_target) :application) (funcall (lambda (app) (funcall (hydra_lib_logic_or (hydra_ext_haskell_testing_contains_trivially_polymorphic (funcall (lambda (v) (hydra_core_application-function v)) app))) (hydra_ext_haskell_testing_contains_trivially_polymorphic (funcall (lambda (v) (hydra_core_application-argument v)) app)))) match_value)) (t nil))) (cadr match_target))) term)))

(defvar hydra_ext_haskell_testing_find_haskell_imports (lambda (namespaces) (lambda (names_) (let* ((mapping_ (funcall (lambda (v) (hydra_module_namespaces-mapping v)) namespaces)) (filtered (funcall (hydra_lib_maps_filter_with_key (lambda (ns_) (lambda (_v) (hydra_lib_logic_not (funcall (hydra_lib_equality_equal (hydra_lib_lists_head (funcall (hydra_lib_strings_split_on "hydra.test.") (funcall (lambda (v) v) ns_)))) ""))))) mapping_))) (funcall (hydra_lib_lists_map (lambda (entry) (hydra_lib_strings_cat (list "import qualified " (funcall (hydra_lib_strings_intercalate ".") (funcall (hydra_lib_lists_map hydra_formatting_capitalize) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) (hydra_lib_pairs_first entry))))) " as " (funcall (lambda (v) v) (hydra_lib_pairs_second entry)))))) (hydra_lib_maps_to_list filtered))))))

(defvar hydra_ext_haskell_testing_generate_test_case_with_codec (lambda (g) (lambda (namespaces) (lambda (codec) (lambda (depth) (lambda (tcm) (let* ((name_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tcase (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) (list :right (list)))))))))

(defvar hydra_ext_haskell_testing_generate_test_group_hierarchy (lambda (g) (lambda (namespaces) (lambda (codec) (lambda (depth) (lambda (test_group) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) test_group)) (indent (hydra_lib_strings_from_list (funcall (hydra_lib_lists_replicate (funcall (hydra_lib_math_mul depth) 2)) 32))) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) test_group))) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map_list (lambda (tc) (funcall (funcall (funcall (funcall (hydra_ext_haskell_testing_generate_test_case_with_codec g) namespaces) codec) depth) tc))) cases_)) (lambda (test_case_lines_raw) (let* ((test_case_lines (funcall (hydra_lib_lists_map (lambda (lines_) (funcall (hydra_lib_lists_map (lambda (line) (funcall (hydra_lib_strings_cat2 indent) line))) lines_))) test_case_lines_raw)) (test_cases_str (funcall (hydra_lib_strings_intercalate "\n") (hydra_lib_lists_concat test_case_lines)))) (funcall (hydra_lib_eithers_map (lambda (subgroups_str) (hydra_lib_strings_cat (list test_cases_str (if (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_equal test_cases_str) "")) (funcall (hydra_lib_equality_equal subgroups_str) "")) "" "\n") subgroups_str)))) (funcall (hydra_lib_eithers_map (lambda (blocks) (funcall (hydra_lib_strings_intercalate "\n") blocks))) (funcall (hydra_lib_eithers_map_list (lambda (subgroup) (let ((group_name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) subgroup))) (funcall (hydra_lib_eithers_map (lambda (content) (hydra_lib_strings_cat (list indent "H.describe " (hydra_lib_literals_show_string group_name_) " $ do\n" content)))) (funcall (funcall (funcall (funcall (hydra_ext_haskell_testing_generate_test_group_hierarchy g) namespaces) codec) (funcall (hydra_lib_math_add depth) 1)) subgroup))))) subgroups)))))))))))))

(defvar hydra_ext_haskell_testing_generate_test_file_with_codec (lambda (codec) (lambda (test_module) (lambda (test_group) (lambda (namespaces) (lambda (g) (funcall (hydra_lib_eithers_map (lambda (test_body) (let* ((ext (funcall (lambda (v) v) (funcall (lambda (v) (hydra_testing_test_codec-file_extension v)) codec))) (ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) test_module)) (spec_ns (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) ns_)) "Spec")) (file_path (funcall (funcall (hydra_names_namespace_to_file_path (list :pascal nil)) ext) spec_ns)) (test_module_content (funcall (funcall (funcall (funcall (hydra_ext_haskell_testing_build_test_module_with_codec codec) test_module) test_group) test_body) namespaces))) (list file_path test_module_content)))) (funcall (funcall (funcall (funcall (hydra_ext_haskell_testing_generate_test_group_hierarchy g) namespaces) codec) 1) test_group))))))))

(defvar hydra_ext_haskell_testing_haskell_import_template "import qualified {namespace} as {alias}")

(defvar hydra_ext_haskell_testing_haskell_module_template (funcall (hydra_lib_strings_intercalate "\n") (list (funcall (hydra_lib_strings_cat2 "-- ") hydra_constants_warning_auto_generated_file) "" "module {moduleName} where" "" "{imports}" "" "spec :: H.Spec" "{testGroup}" "{testCases}" "")))

(defvar hydra_ext_haskell_testing_haskell_test_case_template (funcall (hydra_lib_strings_intercalate "\n") (list "  H.it {name} $ H.shouldBe" "    ({input})" "    ({output})" "")))

(defvar hydra_ext_haskell_testing_haskell_test_group_template "spec = H.describe {groupName} $ do")

(defvar hydra_ext_haskell_testing_namespace_to_module_name (lambda (ns_) (funcall (hydra_lib_strings_intercalate ".") (funcall (hydra_lib_lists_map hydra_formatting_capitalize) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))))))

(defvar hydra_ext_haskell_testing_term_to_haskell (lambda (namespaces) (lambda (term) (lambda (g) (funcall (funcall (hydra_lib_eithers_bimap (lambda (ic) (hydra_show_errors_error (funcall (lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (arg_) (funcall (lambda (arg_) (hydra_serialization_print_expr (hydra_serialization_parenthesize arg_))) (hydra_ext_haskell_serde_expression_to_expr arg_)))) (funcall (funcall (funcall (funcall (hydra_ext_haskell_coder_encode_term 0) namespaces) term) hydra_lexical_empty_context) g))))))

(defvar hydra_ext_haskell_testing_type_to_haskell (lambda (namespaces) (lambda (typ) (lambda (g) (funcall (funcall (hydra_lib_eithers_bimap (lambda (ic) (hydra_show_errors_error (funcall (lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (arg_) (funcall (lambda (arg_) (hydra_serialization_print_expr (hydra_serialization_parenthesize arg_))) (hydra_ext_haskell_serde_type_to_expr arg_)))) (funcall (funcall (funcall (hydra_ext_haskell_coder_encode_type namespaces) typ) hydra_lexical_empty_context) g))))))

(defvar hydra_ext_haskell_testing_haskell_test_codec (lambda (namespaces) (make-hydra_testing_test_codec "haskell" "hs" (hydra_ext_haskell_testing_term_to_haskell namespaces) (hydra_ext_haskell_testing_type_to_haskell namespaces) (lambda (n) n) hydra_ext_haskell_testing_namespace_to_module_name hydra_ext_haskell_testing_haskell_test_case_template hydra_ext_haskell_testing_haskell_test_group_template hydra_ext_haskell_testing_haskell_module_template hydra_ext_haskell_testing_haskell_import_template (hydra_ext_haskell_testing_find_haskell_imports namespaces))))

(defvar hydra_ext_haskell_testing_generate_haskell_test_file (lambda (test_module) (lambda (test_group) (lambda (g) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_ext_haskell_testing_build_namespaces_for_test_group test_module) test_group) g)) (lambda (namespaces) (funcall (funcall (funcall (funcall (hydra_ext_haskell_testing_generate_test_file_with_codec (hydra_ext_haskell_testing_haskell_test_codec namespaces)) test_module) test_group) namespaces) g)))))))

(defvar hydra_ext_haskell_testing_try_infer_type_of (lambda (g) (lambda (term) (funcall (funcall (hydra_lib_eithers_either (lambda (_) (list :nothing))) (lambda (result) (list :just (hydra_lib_pairs_first result)))) (funcall (funcall (hydra_inference_infer_type_of hydra_lexical_empty_context) g) term)))))

(defvar hydra_ext_haskell_testing_generate_type_annotation_for (lambda (g) (lambda (namespaces) (lambda (input_term) (lambda (output_term) (if (hydra_lib_logic_not (hydra_ext_haskell_testing_contains_trivially_polymorphic output_term)) (list :right (list :nothing)) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right (list :nothing)))) (lambda (result) (let* ((schema_vars (hydra_lib_sets_from_list (hydra_lib_maps_keys (funcall (lambda (v) (hydra_graph_graph-schema_types v)) g)))) (type_scheme (hydra_lib_pairs_second result)) (typ (funcall (lambda (v) (hydra_core_type_scheme-type v)) type_scheme)) (free_vars (hydra_lib_sets_to_list (funcall (hydra_lib_sets_difference (hydra_rewriting_free_variables_in_type typ)) schema_vars))) (is_either (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :either) (funcall (lambda (_) t) match_value)) (t nil))) (cadr match_target))) (hydra_rewriting_deannotate_term output_term)))) (if (funcall (hydra_lib_logic_or is_either) (hydra_lib_logic_not (hydra_lib_lists_null free_vars))) (let* ((int32_type (list :literal (list :integer (list :int32 nil)))) (subst (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (v) (list v int32_type))) free_vars))) (grounded_type (funcall (hydra_substitution_subst_in_type subst) typ))) (funcall (hydra_lib_eithers_map (lambda (type_str) (list :just (funcall (hydra_lib_strings_cat2 " :: ") type_str)))) (funcall (funcall (hydra_ext_haskell_testing_type_to_haskell namespaces) grounded_type) g))) (list :right (list :nothing)))))) (funcall (hydra_ext_haskell_testing_try_infer_type_of g) input_term))))))))

(defvar hydra_ext_haskell_testing_indent_continuation_lines (lambda (n) (lambda (s) (funcall (hydra_lib_strings_intercalate (funcall (hydra_lib_strings_cat2 "\n") (hydra_lib_strings_from_list (funcall (hydra_lib_lists_replicate n) 32)))) (funcall (hydra_lib_strings_split_on "\n") s)))))

(provide 'hydra.ext.haskell.testing)
