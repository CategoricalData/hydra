(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.constants)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.dependencies)

(require 'hydra.formatting)

(require 'hydra.haskell.syntax)

(require 'hydra.haskell.utils)

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

(require 'hydra.names)

(require 'hydra.packaging)

(require 'hydra.predicates)

(require 'hydra.rewriting)

(require 'hydra.show.errors)

(require 'hydra.strip)

(require 'hydra.testing)

(require 'hydra.util)

(defvar hydra_haskell_testing_add_namespaces_to_namespaces (lambda (ns0) (lambda (names) (let* ((new_namespaces (hydra_lib_sets_from_list (hydra_lib_maybes_cat (funcall (hydra_lib_lists_map hydra_names_namespace_of) (hydra_lib_sets_to_list names))))) (to_module_name (lambda (namespace) (hydra_formatting_capitalize (funcall (hydra_lib_maybes_from_maybe (lambda () (funcall (lambda (v) v) namespace))) (hydra_lib_lists_maybe_last (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) namespace))))))) (new_mappings (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (ns_) (list ns_ (to_module_name ns_)))) (hydra_lib_sets_to_list new_namespaces))))) (make-hydra_packaging_namespaces (funcall (lambda (v) (hydra_packaging_namespaces-focus v)) ns0) (funcall (hydra_lib_maps_union (funcall (lambda (v) (hydra_packaging_namespaces-mapping v)) ns0)) new_mappings))))))

(defvar hydra_haskell_testing_collect_test_cases (lambda (tg) (funcall (hydra_lib_lists_concat2 (funcall (lambda (v) (hydra_testing_test_group-cases v)) tg)) (hydra_lib_lists_concat (funcall (hydra_lib_lists_map hydra_haskell_testing_collect_test_cases) (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) tg))))))

(defvar hydra_haskell_testing_collect_names (lambda (graf) (lambda (names) (lambda (t_) (if (hydra_predicates_is_encoded_term (hydra_strip_deannotate_term t_)) (funcall (funcall (hydra_lib_eithers_either (lambda (_) names)) (lambda (decoded_term) (funcall (hydra_lib_sets_union names) (funcall (funcall (funcall (hydra_dependencies_term_dependency_names t) t) t) decoded_term)))) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) _e)) (lambda (_a) _a)) (funcall (hydra_decode_core_term graf) t_))) names)))))

(defvar hydra_haskell_testing_extract_encoded_term_variable_names (lambda (graf) (lambda (term) (funcall (funcall (funcall (hydra_rewriting_fold_over_term (list :pre nil)) (hydra_haskell_testing_collect_names graf)) hydra_lib_sets_empty) term))))

(defvar hydra_haskell_testing_extract_test_terms (lambda (tcm) (list)))

(defvar hydra_haskell_testing_build_namespaces_for_test_group (lambda (mod) (lambda (tgroup) (lambda (graph_) (let* ((test_cases_ (hydra_haskell_testing_collect_test_cases tgroup)) (test_terms (hydra_lib_lists_concat (funcall (hydra_lib_lists_map hydra_haskell_testing_extract_test_terms) test_cases_))) (test_bindings (funcall (hydra_lib_lists_map (lambda (term) (make-hydra_core_binding "_test_" term (list :nothing)))) test_terms)) (temp_module (make-hydra_packaging_module (funcall (lambda (v) (hydra_packaging_module-namespace v)) mod) (funcall (hydra_lib_lists_map (lambda (b) (list :term (make-hydra_packaging_term_definition (funcall (lambda (v) (hydra_core_binding-name v)) b) (funcall (lambda (v) (hydra_core_binding-term v)) b) (funcall (lambda (v) (hydra_core_binding-type v)) b))))) test_bindings) (funcall (lambda (v) (hydra_packaging_module-term_dependencies v)) mod) (funcall (lambda (v) (hydra_packaging_module-type_dependencies v)) mod) (funcall (lambda (v) (hydra_packaging_module-description v)) mod)))) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (e) (hydra_show_errors_error e))) (lambda (a) a)) (funcall (funcall (hydra_haskell_utils_namespaces_for_module temp_module) hydra_lexical_empty_context) graph_))) (lambda (base_namespaces) (let ((encoded_names (hydra_lib_sets_unions (funcall (hydra_lib_lists_map (lambda (t_) (funcall (hydra_haskell_testing_extract_encoded_term_variable_names graph_) t_))) test_terms)))) (list :right (funcall (hydra_haskell_testing_add_namespaces_to_namespaces base_namespaces) encoded_names))))))))))

(defvar hydra_haskell_testing_find_haskell_imports (lambda (namespaces) (lambda (names_) (let* ((mapping_ (funcall (lambda (v) (hydra_packaging_namespaces-mapping v)) namespaces)) (filtered (funcall (hydra_lib_maps_filter_with_key (lambda (ns_) (lambda (_v) (hydra_lib_logic_not (funcall (hydra_lib_equality_equal (funcall (hydra_lib_maybes_from_maybe (lambda () "")) (hydra_lib_lists_maybe_head (funcall (hydra_lib_strings_split_on "hydra.test.") (funcall (lambda (v) v) ns_))))) ""))))) mapping_))) (funcall (hydra_lib_lists_map (lambda (entry) (hydra_lib_strings_cat (list "import qualified " (funcall (hydra_lib_strings_intercalate ".") (funcall (hydra_lib_lists_map hydra_formatting_capitalize) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) (hydra_lib_pairs_first entry))))) " as " (funcall (lambda (v) v) (hydra_lib_pairs_second entry)))))) (hydra_lib_maps_to_list filtered))))))

(defvar hydra_haskell_testing_namespace_to_module_name (lambda (ns_) (funcall (hydra_lib_strings_intercalate ".") (funcall (hydra_lib_lists_map hydra_formatting_capitalize) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))))))

(defvar hydra_haskell_testing_build_test_module (lambda (test_module) (lambda (test_group) (lambda (test_body) (lambda (namespaces) (let* ((domain_imports (funcall (hydra_haskell_testing_find_haskell_imports namespaces) hydra_lib_sets_empty)) (standard_imports (list "import Hydra.Kernel" "import qualified Test.Hspec as H" "import qualified Data.List as L" "import qualified Data.Map as M" "import qualified Data.Set as S" "import qualified Data.Maybe as Y")) (all_imports (funcall (hydra_lib_lists_concat2 standard_imports) domain_imports)) (group_name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) test_group)) (ns_ (funcall (lambda (v) (hydra_packaging_module-namespace v)) test_module)) (spec_ns (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) ns_)) "Spec")) (module_name_string (hydra_haskell_testing_namespace_to_module_name spec_ns)) (header (funcall (hydra_lib_strings_intercalate "\n") (hydra_lib_lists_concat (list (list (funcall (hydra_lib_strings_cat2 "-- ") hydra_constants_warning_auto_generated_file) "") (list "" (hydra_lib_strings_cat (list "module " module_name_string " where")) "") all_imports (list "" "spec :: H.Spec" (hydra_lib_strings_cat (list "spec = H.describe " (hydra_lib_literals_show_string group_name_) " $ do")))))))) (hydra_lib_strings_cat (list header "\n" test_body "\n"))))))))

(defvar hydra_haskell_testing_generate_test_case (lambda (depth) (lambda (tcm) (let* ((tcase (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm)) (universal (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :universal) (funcall (lambda (u) u) match_value)))) (cadr match_target))) tcase)) (actual_ (funcall (lambda (v) (hydra_testing_universal_test_case-actual v)) universal)) (expected_ (funcall (lambda (v) (hydra_testing_universal_test_case-expected v)) universal)) (name_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm))) (list :right (list (hydra_lib_strings_cat (list "H.it " (hydra_lib_literals_show_string name_) " $ H.shouldBe")) (hydra_lib_strings_cat (list "  (" actual_ ")")) (hydra_lib_strings_cat (list "  (" expected_ ")"))))))))

(defvar hydra_haskell_testing_generate_test_group_hierarchy (lambda (depth) (lambda (test_group) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) test_group)) (indent (hydra_lib_strings_from_list (funcall (hydra_lib_lists_replicate (funcall (hydra_lib_math_mul depth) 2)) 32))) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) test_group))) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map_list (lambda (tc) (funcall (hydra_haskell_testing_generate_test_case depth) tc))) cases_)) (lambda (test_case_lines_raw) (let* ((test_case_lines (funcall (hydra_lib_lists_map (lambda (lines_) (funcall (hydra_lib_lists_map (lambda (line) (funcall (hydra_lib_strings_cat2 indent) line))) lines_))) test_case_lines_raw)) (test_cases_str (funcall (hydra_lib_strings_intercalate "\n") (hydra_lib_lists_concat test_case_lines)))) (funcall (hydra_lib_eithers_map (lambda (subgroups_str) (hydra_lib_strings_cat (list test_cases_str (if (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_equal test_cases_str) "")) (funcall (hydra_lib_equality_equal subgroups_str) "")) "" "\n") subgroups_str)))) (funcall (hydra_lib_eithers_map (lambda (blocks) (funcall (hydra_lib_strings_intercalate "\n") blocks))) (funcall (hydra_lib_eithers_map_list (lambda (subgroup) (let ((group_name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) subgroup))) (funcall (hydra_lib_eithers_map (lambda (content) (hydra_lib_strings_cat (list indent "H.describe " (hydra_lib_literals_show_string group_name_) " $ do\n" content)))) (funcall (hydra_haskell_testing_generate_test_group_hierarchy (funcall (hydra_lib_math_add depth) 1)) subgroup))))) subgroups))))))))))

(defvar hydra_haskell_testing_generate_test_file (lambda (test_module) (lambda (test_group) (lambda (namespaces) (funcall (hydra_lib_eithers_map (lambda (test_body) (let* ((ns_ (funcall (lambda (v) (hydra_packaging_module-namespace v)) test_module)) (spec_ns (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) ns_)) "Spec")) (file_path (funcall (funcall (hydra_names_namespace_to_file_path (list :pascal nil)) "hs") spec_ns)) (test_module_content (funcall (funcall (funcall (hydra_haskell_testing_build_test_module test_module) test_group) test_body) namespaces))) (list file_path test_module_content)))) (funcall (hydra_haskell_testing_generate_test_group_hierarchy 1) test_group))))))

(defvar hydra_haskell_testing_generate_haskell_test_file (lambda (test_module) (lambda (test_group) (lambda (g) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_haskell_testing_build_namespaces_for_test_group test_module) test_group) g)) (lambda (namespaces) (funcall (funcall (hydra_haskell_testing_generate_test_file test_module) test_group) namespaces)))))))

(provide 'hydra.haskell.testing)
