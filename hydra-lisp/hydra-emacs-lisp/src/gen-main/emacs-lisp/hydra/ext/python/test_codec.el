(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.constants)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.ext.python.coder)

(require 'hydra.ext.python.helpers)

(require 'hydra.ext.python.names)

(require 'hydra.ext.python.serde)

(require 'hydra.ext.python.utils)

(require 'hydra.lexical)

(require 'hydra.lib.chars)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.serialization)

(require 'hydra.show.error)

(require 'hydra.test.utils)

(require 'hydra.testing)

(defvar hydra_ext_python_test_codec_build_python_test_module (lambda (codec) (lambda (test_module) (lambda (test_group) (lambda (test_body) (lambda (namespaces_) (let* ((domain_imports (funcall (funcall (lambda (v) (hydra_testing_test_codec-find_imports v)) codec) hydra_lib_sets_empty)) (standard_imports (list "from __future__ import annotations" "from typing import cast" "from decimal import Decimal" "from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing")) (all_imports (funcall (hydra_lib_lists_concat2 standard_imports) domain_imports)) (group_name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) test_group)) (header (hydra_lib_strings_cat (list (funcall (hydra_lib_strings_cat2 "# ") hydra_constants_warning_auto_generated_file) "\n" (funcall (hydra_lib_strings_cat2 "# ") group_name_) "\n\n" (funcall (hydra_lib_strings_intercalate "\n") all_imports) "\n\n")))) (hydra_lib_strings_cat (list header test_body "\n")))))))))

(defvar hydra_ext_python_test_codec_empty_python_module_metadata (lambda (ns_) (make-hydra_ext_python_helpers_python_module_metadata (make-hydra_module_namespaces (list ns_ (hydra_ext_python_names_encode_namespace ns_)) hydra_lib_maps_empty) hydra_lib_sets_empty nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)))

(defvar hydra_ext_python_test_codec_find_python_imports (lambda (namespaces_) (lambda (names_) (let* ((mapping_ (funcall (lambda (v) (hydra_module_namespaces-mapping v)) namespaces_)) (filtered (funcall (hydra_lib_maps_filter_with_key (lambda (ns_) (lambda (_v) (hydra_lib_logic_not (funcall (hydra_lib_equality_equal (hydra_lib_lists_head (funcall (hydra_lib_strings_split_on "hydra.test.") (funcall (lambda (v) v) ns_)))) ""))))) mapping_))) (funcall (hydra_lib_lists_map (lambda (entry) (funcall (hydra_lib_strings_cat2 "import ") (funcall (lambda (v) v) (hydra_lib_pairs_first entry))))) (hydra_lib_maps_to_list filtered))))))

(defvar hydra_ext_python_test_codec_format_python_test_name (lambda (name) (funcall (hydra_lib_strings_cat2 "test_") (hydra_lib_strings_from_list (funcall (hydra_lib_lists_map (lambda (c) (if (hydra_lib_chars_is_alpha_num c) (hydra_lib_chars_to_lower c) 95))) (hydra_lib_strings_to_list name))))))

(defvar hydra_ext_python_test_codec_generate_python_test_case (lambda (g) (lambda (namespaces_) (lambda (codec) (lambda (group_path) (lambda (tcm) (let* ((name_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tcase (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :delegated_evaluation) (funcall (lambda (del_case) (let* ((full_name (if (hydra_lib_lists_null group_path) name_ (funcall (hydra_lib_strings_intercalate "__") (funcall (hydra_lib_lists_concat2 group_path) (list name_))))) (formatted_name (funcall (funcall (lambda (v) (hydra_testing_test_codec-format_test_name v)) codec) full_name)) (input_ (funcall (lambda (v) (hydra_testing_delegated_evaluation_test_case-input v)) del_case)) (output_ (funcall (lambda (v) (hydra_testing_delegated_evaluation_test_case-output v)) del_case))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (lambda (v) (hydra_testing_test_codec-encode_term v)) codec) input_) g)) (lambda (input_code) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (lambda (v) (hydra_testing_test_codec-encode_term v)) codec) output_) g)) (lambda (output_code) (list :right (list (hydra_lib_strings_cat (list "def " formatted_name "():")) (hydra_lib_strings_cat (list "    assert (" input_code ") == (" output_code ")")))))))))) match_value)) (t (list :right (list))))) (cadr match_target))) tcase))))))))

(defvar hydra_ext_python_test_codec_generate_python_test_group_hierarchy (lambda (g) (lambda (namespaces_) (lambda (codec) (lambda (group_path) (lambda (test_group) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) test_group)) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) test_group))) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map_list (lambda (tc) (funcall (funcall (funcall (funcall (hydra_ext_python_test_codec_generate_python_test_case g) namespaces_) codec) group_path) tc))) cases_)) (lambda (test_case_lines) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map_list (lambda (subgroup) (let* ((group_name (funcall (lambda (v) (hydra_testing_test_group-name v)) subgroup)) (header (funcall (hydra_lib_strings_cat2 "# ") group_name))) (funcall (hydra_lib_eithers_map (lambda (content) (hydra_lib_strings_cat (list header "\n\n" content)))) (funcall (funcall (funcall (funcall (hydra_ext_python_test_codec_generate_python_test_group_hierarchy g) namespaces_) codec) (funcall (hydra_lib_lists_concat2 group_path) (list group_name))) subgroup))))) subgroups)) (lambda (subgroup_blocks) (let* ((subgroups_str (funcall (hydra_lib_strings_intercalate "\n\n") subgroup_blocks)) (test_cases_str (funcall (hydra_lib_strings_intercalate "\n\n") (hydra_lib_lists_concat test_case_lines)))) (list :right (hydra_lib_strings_cat (list test_cases_str (if (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_equal test_cases_str) "")) (funcall (hydra_lib_equality_equal subgroups_str) "")) "" "\n\n") subgroups_str)))))))))))))))

(defvar hydra_ext_python_test_codec_generate_test_file_with_python_codec (lambda (codec) (lambda (test_module) (lambda (test_group) (lambda (namespaces_) (lambda (g) (funcall (hydra_lib_eithers_map (lambda (test_body) (let* ((ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) test_module)) (parts (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))) (dir_parts (hydra_lib_lists_init parts)) (file_name (hydra_lib_strings_cat (list "test_" (hydra_lib_lists_last parts) ".py"))) (file_path (hydra_lib_strings_cat (list (funcall (hydra_lib_strings_intercalate "/") dir_parts) "/" file_name))) (test_module_content (funcall (funcall (funcall (funcall (hydra_ext_python_test_codec_build_python_test_module codec) test_module) test_group) test_body) namespaces_))) (list file_path test_module_content)))) (funcall (funcall (funcall (funcall (hydra_ext_python_test_codec_generate_python_test_group_hierarchy g) namespaces_) codec) (list)) test_group))))))))

(defvar hydra_ext_python_test_codec_namespaces_for_python_module (lambda (mod) (lambda (graph_) (let* ((bindings (hydra_lexical_graph_to_bindings graph_)) (defs (funcall (hydra_lib_maybes_map_maybe (lambda (b) (funcall (hydra_lib_maybes_map (lambda (ts) (list :term (make-hydra_module_term_definition (funcall (lambda (v) (hydra_core_binding-name v)) b) (funcall (lambda (v) (hydra_core_binding-term v)) b) ts)))) (funcall (lambda (v) (hydra_core_binding-type v)) b)))) bindings))) (list :right (funcall (hydra_ext_python_utils_find_namespaces (funcall (lambda (v) (hydra_module_module-namespace v)) mod)) defs))))))

(defvar hydra_ext_python_test_codec_namespace_to_python_module_name (lambda (ns_) (funcall (lambda (v) v) ns_)))

(defvar hydra_ext_python_test_codec_python_import_template "import {namespace}")

(defvar hydra_ext_python_test_codec_python_module_template (funcall (hydra_lib_strings_intercalate "\n") (list (funcall (hydra_lib_strings_cat2 "# ") hydra_constants_warning_auto_generated_file) "" "{imports}" "" "{testGroup}" "" "{testCases}")))

(defvar hydra_ext_python_test_codec_python_test_case_template (funcall (hydra_lib_strings_intercalate "\n") (list "def {name}():" "    assert ({input}) == ({output})")))

(defvar hydra_ext_python_test_codec_python_test_group_template "# {groupName}")

(defvar hydra_ext_python_test_codec_term_to_python_with_context (lambda (namespaces_) (lambda (graph0) (lambda (skip_casts) (lambda (term) (lambda (_g) (funcall (funcall (hydra_lib_eithers_bimap (lambda (ic) (hydra_show_error_error (funcall (lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (arg_) (hydra_serialization_print_expr (hydra_ext_python_serde_encode_expression arg_)))) (funcall (funcall (funcall (hydra_ext_python_coder_encode_term_inline hydra_lexical_empty_context) (make-hydra_ext_python_helpers_python_environment namespaces_ (list (list) hydra_lib_maps_empty) graph0 hydra_lib_sets_empty hydra_ext_python_utils_target_python_version skip_casts hydra_lib_sets_empty)) skip_casts) term))))))))

(defvar hydra_ext_python_test_codec_type_to_python (lambda (namespaces_) (lambda (typ) (lambda (g) (funcall (funcall (hydra_lib_eithers_bimap (lambda (ic) (hydra_show_error_error (funcall (lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (arg_) (hydra_serialization_print_expr (hydra_ext_python_serde_encode_expression arg_)))) (funcall (hydra_ext_python_coder_encode_type (make-hydra_ext_python_helpers_python_environment namespaces_ (list (list) hydra_lib_maps_empty) g hydra_lib_sets_empty hydra_ext_python_utils_target_python_version nil hydra_lib_sets_empty)) typ))))))

(defvar hydra_ext_python_test_codec_python_test_codec_with_context (lambda (namespaces_) (lambda (tcontext) (make-hydra_testing_test_codec "python" "py" (funcall (funcall (hydra_ext_python_test_codec_term_to_python_with_context namespaces_) tcontext) t) (hydra_ext_python_test_codec_type_to_python namespaces_) hydra_ext_python_test_codec_format_python_test_name hydra_ext_python_test_codec_namespace_to_python_module_name hydra_ext_python_test_codec_python_test_case_template hydra_ext_python_test_codec_python_test_group_template hydra_ext_python_test_codec_python_module_template hydra_ext_python_test_codec_python_import_template (hydra_ext_python_test_codec_find_python_imports namespaces_)))))

(defvar hydra_ext_python_test_codec_generate_python_test_file (lambda (test_module) (lambda (test_group) (lambda (g) (funcall (hydra_lib_eithers_bind (funcall (hydra_test_utils_infer_test_group_terms g) test_group)) (lambda (inferred_test_group) (funcall (hydra_lib_eithers_bind (funcall (hydra_ext_python_test_codec_namespaces_for_python_module test_module) g)) (lambda (namespaces_) (funcall (funcall (funcall (funcall (hydra_ext_python_test_codec_generate_test_file_with_python_codec (funcall (hydra_ext_python_test_codec_python_test_codec_with_context namespaces_) g)) test_module) inferred_test_group) namespaces_) g)))))))))

(defvar hydra_ext_python_test_codec_term_to_python (lambda (namespaces_) (lambda (term) (lambda (g) (funcall (funcall (funcall (funcall (hydra_ext_python_test_codec_term_to_python_with_context namespaces_) g) nil) term) g)))))

(defvar hydra_ext_python_test_codec_python_test_codec (lambda (namespaces_) (make-hydra_testing_test_codec "python" "py" (hydra_ext_python_test_codec_term_to_python namespaces_) (hydra_ext_python_test_codec_type_to_python namespaces_) hydra_ext_python_test_codec_format_python_test_name hydra_ext_python_test_codec_namespace_to_python_module_name hydra_ext_python_test_codec_python_test_case_template hydra_ext_python_test_codec_python_test_group_template hydra_ext_python_test_codec_python_module_template hydra_ext_python_test_codec_python_import_template (hydra_ext_python_test_codec_find_python_imports namespaces_))))

(provide 'hydra.ext.python.testCodec)
