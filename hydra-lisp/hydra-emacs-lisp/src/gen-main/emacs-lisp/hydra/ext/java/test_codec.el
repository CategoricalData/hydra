(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.constants)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.ext.java.coder)

(require 'hydra.ext.java.helpers)

(require 'hydra.ext.java.serde)

(require 'hydra.formatting)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.rewriting)

(require 'hydra.serialization)

(require 'hydra.show.error)

(require 'hydra.test.utils)

(require 'hydra.testing)

(require 'hydra.util)

(defvar hydra_ext_java_test_codec_build_java_test_module (lambda (codec) (lambda (test_module) (lambda (test_group) (lambda (test_body) (let* ((ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) test_module)) (parts (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))) (class_name_ (funcall (hydra_lib_strings_cat2 (hydra_formatting_capitalize (hydra_lib_lists_last parts))) "Test")) (group_name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) test_group)) (package_name (funcall (hydra_lib_strings_intercalate ".") (hydra_lib_lists_init parts))) (standard_imports (list "import org.junit.jupiter.api.Test;" "import static org.junit.jupiter.api.Assertions.*;" "import java.util.*;" "import hydra.util.*;")) (header (hydra_lib_strings_cat (list (funcall (hydra_lib_strings_cat2 "// ") hydra_constants_warning_auto_generated_file) "\n" (funcall (hydra_lib_strings_cat2 "// ") group_name_) "\n\n" (hydra_lib_strings_cat (list "package " package_name ";\n\n")) (funcall (hydra_lib_strings_intercalate "\n") standard_imports) "\n\n" (hydra_lib_strings_cat (list "public class " class_name_ " {\n\n")))))) (hydra_lib_strings_cat (list header test_body "\n}\n"))))))))

(defvar hydra_ext_java_test_codec_find_java_imports (list "import org.junit.jupiter.api.Test;" "import static org.junit.jupiter.api.Assertions.*;" "import java.util.*;"))

(defvar hydra_ext_java_test_codec_format_java_test_name (lambda (name) (let* ((replaced (funcall (hydra_lib_strings_intercalate " Neg") (funcall (hydra_lib_strings_split_on "-") (funcall (hydra_lib_strings_intercalate "Dot") (funcall (hydra_lib_strings_split_on ".") (funcall (hydra_lib_strings_intercalate " Plus") (funcall (hydra_lib_strings_split_on "+") (funcall (hydra_lib_strings_intercalate " Div") (funcall (hydra_lib_strings_split_on "/") (funcall (hydra_lib_strings_intercalate " Mul") (funcall (hydra_lib_strings_split_on "*") (funcall (hydra_lib_strings_intercalate " Num") (funcall (hydra_lib_strings_split_on "#") name))))))))))))) (sanitized (hydra_formatting_non_alnum_to_underscores replaced)) (pascal_ (funcall (funcall (hydra_formatting_convert_case (list :lower_snake nil)) (list :pascal nil)) sanitized))) (funcall (hydra_lib_strings_cat2 "test") pascal_))))

(defvar hydra_ext_java_test_codec_generate_assertion (lambda (assert_type) (lambda (output_code) (lambda (input_code) (if (funcall (hydra_lib_equality_equal assert_type) "assertArrayEquals") (list "        assertArrayEquals(" (hydra_lib_strings_cat (list "            " output_code ",")) (hydra_lib_strings_cat (list "            " input_code ");"))) (if (funcall (hydra_lib_equality_equal assert_type) "assertBigDecimalEquals") (list (hydra_lib_strings_cat (list "        assertEquals(0, (" output_code ").compareTo(" input_code "));"))) (if (funcall (hydra_lib_equality_equal assert_type) "assertDoubleEquals") (list "        assertEquals(" (hydra_lib_strings_cat (list "            " output_code ",")) (hydra_lib_strings_cat (list "            " input_code ",")) "            1e-15);") (list "        assertEquals(" (hydra_lib_strings_cat (list "            " output_code ",")) (hydra_lib_strings_cat (list "            " input_code ");"))))))))))

(defvar hydra_ext_java_test_codec_get_assertion_type (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (lit) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :binary) (funcall (lambda (_b) "assertArrayEquals") match_value)) ((equal (car match_target) :float) (funcall (lambda (fv) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :bigfloat) (funcall (lambda (_bf) "assertBigDecimalEquals") match_value)) (t "assertDoubleEquals"))) (cadr match_target))) fv)) match_value)) (t "assertEquals"))) (cadr match_target))) lit)) match_value)) (t "assertEquals"))) (cadr match_target))) (hydra_rewriting_deannotate_term term))))

(defvar hydra_ext_java_test_codec_is_inference_var (lambda (n) (let* ((s (funcall (lambda (v) v) n)) (chars (hydra_lib_strings_to_list s))) (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_equal (funcall (hydra_lib_strings_char_at 0) s)) 116)) (funcall (hydra_lib_logic_and (hydra_lib_logic_not (funcall (hydra_lib_equality_equal (hydra_lib_strings_length s)) 1))) (hydra_lib_lists_null (funcall (hydra_lib_lists_filter (lambda (c) (hydra_lib_logic_not (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gte c) 48)) (funcall (hydra_lib_equality_lte c) 57))))) (funcall (hydra_lib_lists_drop 1) chars))))))))

(defvar hydra_ext_java_test_codec_generate_java_test_case (lambda (g) (lambda (codec) (lambda (group_path) (lambda (tcm) (let* ((name_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tcase (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :delegated_evaluation) (funcall (lambda (del_case) (let* ((output_ (funcall (lambda (v) (hydra_testing_delegated_evaluation_test_case-output v)) del_case)) (assert_type (hydra_ext_java_test_codec_get_assertion_type output_)) (full_name (if (hydra_lib_lists_null group_path) name_ (funcall (hydra_lib_strings_intercalate "_") (funcall (hydra_lib_lists_concat2 group_path) (list name_))))) (formatted_name (funcall (funcall (lambda (v) (hydra_testing_test_codec-format_test_name v)) codec) full_name)) (input_ (funcall (lambda (v) (hydra_testing_delegated_evaluation_test_case-input v)) del_case))) (let* ((type_vars (hydra_lib_lists_sort (funcall (hydra_lib_lists_filter hydra_ext_java_test_codec_is_inference_var) (hydra_lib_sets_to_list (funcall (hydra_lib_sets_union (hydra_rewriting_free_type_variables_in_term input_)) (hydra_rewriting_free_type_variables_in_term output_)))))) (type_params_str (if (hydra_lib_lists_null type_vars) "" (hydra_lib_strings_cat (list "<" (funcall (hydra_lib_strings_intercalate ", ") (funcall (hydra_lib_lists_map (lambda (n_) (hydra_formatting_capitalize (funcall (lambda (v) v) n_)))) type_vars)) "> "))))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (lambda (v) (hydra_testing_test_codec-encode_term v)) codec) input_) g)) (lambda (input_code) (funcall (hydra_lib_eithers_map (lambda (output_code) (let ((assertion_lines (funcall (funcall (hydra_ext_java_test_codec_generate_assertion assert_type) output_code) input_code))) (funcall (hydra_lib_lists_concat2 (list "    @Test" (hydra_lib_strings_cat (list "    public " type_params_str "void " formatted_name "() {")))) (funcall (hydra_lib_lists_concat2 assertion_lines) (list "    }")))))) (funcall (funcall (funcall (lambda (v) (hydra_testing_test_codec-encode_term v)) codec) output_) g))))))) match_value)) (t (list :right (list))))) (cadr match_target))) tcase)))))))

(defvar hydra_ext_java_test_codec_generate_java_test_group_hierarchy (lambda (g) (lambda (codec) (lambda (group_path) (lambda (test_group) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) test_group)) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) test_group))) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map (lambda (lines_) (funcall (hydra_lib_strings_intercalate "\n\n") (hydra_lib_lists_concat lines_)))) (funcall (hydra_lib_eithers_map_list (lambda (tc) (funcall (funcall (funcall (hydra_ext_java_test_codec_generate_java_test_case g) codec) group_path) tc))) cases_))) (lambda (test_cases_str) (funcall (hydra_lib_eithers_map (lambda (subgroups_str) (hydra_lib_strings_cat (list test_cases_str (if (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_equal test_cases_str) "")) (funcall (hydra_lib_equality_equal subgroups_str) "")) "" "\n\n") subgroups_str)))) (funcall (hydra_lib_eithers_map (lambda (blocks) (funcall (hydra_lib_strings_intercalate "\n\n") blocks))) (funcall (hydra_lib_eithers_map_list (lambda (subgroup) (let* ((group_name (funcall (lambda (v) (hydra_testing_test_group-name v)) subgroup)) (header (funcall (hydra_lib_strings_cat2 "    // ") group_name))) (funcall (hydra_lib_eithers_map (lambda (content) (hydra_lib_strings_cat (list header "\n\n" content)))) (funcall (funcall (funcall (hydra_ext_java_test_codec_generate_java_test_group_hierarchy g) codec) (funcall (hydra_lib_lists_concat2 group_path) (list group_name))) subgroup))))) subgroups)))))))))))

(defvar hydra_ext_java_test_codec_generate_test_file_with_java_codec (lambda (codec) (lambda (test_module) (lambda (test_group) (lambda (g) (funcall (hydra_lib_eithers_map (lambda (test_body) (let* ((ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) test_module)) (parts (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))) (class_name_ (funcall (hydra_lib_strings_cat2 (hydra_formatting_capitalize (hydra_lib_lists_last parts))) "Test")) (dir_parts (funcall (hydra_lib_lists_drop 1) (hydra_lib_lists_init parts))) (file_name (funcall (hydra_lib_strings_cat2 class_name_) ".java")) (file_path (hydra_lib_strings_cat (list (funcall (hydra_lib_strings_intercalate "/") dir_parts) "/" file_name))) (test_module_content (funcall (funcall (funcall (hydra_ext_java_test_codec_build_java_test_module codec) test_module) test_group) test_body))) (list file_path test_module_content)))) (funcall (funcall (funcall (hydra_ext_java_test_codec_generate_java_test_group_hierarchy g) codec) (list)) test_group)))))))

(defvar hydra_ext_java_test_codec_java_import_template "import {namespace};")

(defvar hydra_ext_java_test_codec_java_module_template (funcall (hydra_lib_strings_intercalate "\n") (list (funcall (hydra_lib_strings_cat2 "// ") hydra_constants_warning_auto_generated_file) "" "package {package};" "" "{imports}" "" "public class {className} {" "    {testCases}" "}")))

(defvar hydra_ext_java_test_codec_java_test_case_template (funcall (hydra_lib_strings_intercalate "\n") (list "    @Test" "    public void {name}() {" "        assertEquals({output}, {input});" "    }")))

(defvar hydra_ext_java_test_codec_java_test_group_template "// {groupName}")

(defvar hydra_ext_java_test_codec_namespace_to_java_class_name (lambda (ns_) (funcall (hydra_lib_strings_intercalate ".") (funcall (hydra_lib_lists_map hydra_formatting_capitalize) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))))))

(defvar hydra_ext_java_test_codec_term_to_java (lambda (term) (lambda (g) (funcall (funcall (hydra_lib_eithers_bimap (lambda (ic) (hydra_show_error_error (funcall (lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (arg_) (funcall (lambda (arg_) (hydra_serialization_print_expr (hydra_serialization_parenthesize arg_))) (hydra_ext_java_serde_write_expression arg_)))) (funcall (funcall (funcall (hydra_ext_java_coder_encode_term (make-hydra_ext_java_helpers_java_environment (make-hydra_ext_java_helpers_aliases "test" hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_sets_empty (list :nothing) hydra_lib_sets_empty) g)) term) hydra_lexical_empty_context) g)))))

(defvar hydra_ext_java_test_codec_type_to_java (lambda (_t) (lambda (_g) (list :right "Object"))))

(defvar hydra_ext_java_test_codec_java_test_codec (make-hydra_testing_test_codec "java" "java" hydra_ext_java_test_codec_term_to_java hydra_ext_java_test_codec_type_to_java hydra_ext_java_test_codec_format_java_test_name hydra_ext_java_test_codec_namespace_to_java_class_name hydra_ext_java_test_codec_java_test_case_template hydra_ext_java_test_codec_java_test_group_template hydra_ext_java_test_codec_java_module_template hydra_ext_java_test_codec_java_import_template (lambda (_names) hydra_ext_java_test_codec_find_java_imports)))

(defvar hydra_ext_java_test_codec_generate_java_test_file (lambda (test_module) (lambda (test_group) (lambda (g) (funcall (hydra_lib_eithers_bind (funcall (hydra_test_utils_infer_test_group_terms g) test_group)) (lambda (inferred_test_group) (funcall (funcall (funcall (hydra_ext_java_test_codec_generate_test_file_with_java_codec hydra_ext_java_test_codec_java_test_codec) test_module) inferred_test_group) g)))))))

(provide 'hydra.ext.java.testCodec)
