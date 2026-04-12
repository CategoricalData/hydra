(require 'cl-lib)

(require 'hydra.constants)

(require 'hydra.formatting)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.strings)

(require 'hydra.packaging)

(require 'hydra.testing)

(require 'hydra.util)

(defvar hydra_ext_java_testing_build_java_test_module (lambda (test_module) (lambda (test_group) (lambda (test_body) (let* ((ns_ (funcall (lambda (v) (hydra_packaging_module-namespace v)) test_module)) (parts (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))) (class_name_ (funcall (hydra_lib_strings_cat2 (hydra_formatting_capitalize (hydra_lib_lists_last parts))) "Test")) (group_name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) test_group)) (package_name (funcall (hydra_lib_strings_intercalate ".") (hydra_lib_lists_init parts))) (standard_imports (list "import org.junit.jupiter.api.Test;" "import static org.junit.jupiter.api.Assertions.*;" "import java.util.*;" "import hydra.util.*;")) (header (hydra_lib_strings_cat (list (funcall (hydra_lib_strings_cat2 "// ") hydra_constants_warning_auto_generated_file) "\n" (funcall (hydra_lib_strings_cat2 "// ") group_name_) "\n\n" (hydra_lib_strings_cat (list "package " package_name ";\n\n")) (funcall (hydra_lib_strings_intercalate "\n") standard_imports) "\n\n" (hydra_lib_strings_cat (list "public class " class_name_ " {\n\n")))))) (hydra_lib_strings_cat (list header test_body "\n}\n")))))))

(defvar hydra_ext_java_testing_find_java_imports (list "import org.junit.jupiter.api.Test;" "import static org.junit.jupiter.api.Assertions.*;" "import java.util.*;"))

(defvar hydra_ext_java_testing_format_java_test_name (lambda (name) (let* ((replaced (funcall (hydra_lib_strings_intercalate " Neg") (funcall (hydra_lib_strings_split_on "-") (funcall (hydra_lib_strings_intercalate "Dot") (funcall (hydra_lib_strings_split_on ".") (funcall (hydra_lib_strings_intercalate " Plus") (funcall (hydra_lib_strings_split_on "+") (funcall (hydra_lib_strings_intercalate " Div") (funcall (hydra_lib_strings_split_on "/") (funcall (hydra_lib_strings_intercalate " Mul") (funcall (hydra_lib_strings_split_on "*") (funcall (hydra_lib_strings_intercalate " Num") (funcall (hydra_lib_strings_split_on "#") name))))))))))))) (sanitized (hydra_formatting_non_alnum_to_underscores replaced)) (pascal_ (funcall (funcall (hydra_formatting_convert_case (list :lower_snake nil)) (list :pascal nil)) sanitized))) (funcall (hydra_lib_strings_cat2 "test") pascal_))))

(defvar hydra_ext_java_testing_generate_java_test_case (lambda (group_path) (lambda (tcm) (let* ((name_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tcase (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :universal) (funcall (lambda (ucase) (let* ((actual_ (funcall (lambda (v) (hydra_testing_universal_test_case-actual v)) ucase)) (expected_ (funcall (lambda (v) (hydra_testing_universal_test_case-expected v)) ucase)) (full_name (if (hydra_lib_lists_null group_path) name_ (funcall (hydra_lib_strings_intercalate "_") (funcall (hydra_lib_lists_concat2 group_path) (list name_))))) (formatted_name (hydra_ext_java_testing_format_java_test_name full_name))) (list :right (list "    @Test" (hydra_lib_strings_cat (list "    public void " formatted_name "() {")) "        assertEquals(" (hydra_lib_strings_cat (list "            " expected_ ",")) (hydra_lib_strings_cat (list "            " actual_ ");")) "    }")))) match_value)))) (cadr match_target))) tcase)))))

(defvar hydra_ext_java_testing_generate_java_test_group_hierarchy (lambda (group_path) (lambda (test_group) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) test_group)) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) test_group))) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map (lambda (lines_) (funcall (hydra_lib_strings_intercalate "\n\n") (hydra_lib_lists_concat lines_)))) (funcall (hydra_lib_eithers_map_list (lambda (tc) (funcall (hydra_ext_java_testing_generate_java_test_case group_path) tc))) cases_))) (lambda (test_cases_str) (funcall (hydra_lib_eithers_map (lambda (subgroups_str) (hydra_lib_strings_cat (list test_cases_str (if (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_equal test_cases_str) "")) (funcall (hydra_lib_equality_equal subgroups_str) "")) "" "\n\n") subgroups_str)))) (funcall (hydra_lib_eithers_map (lambda (blocks) (funcall (hydra_lib_strings_intercalate "\n\n") blocks))) (funcall (hydra_lib_eithers_map_list (lambda (subgroup) (let* ((group_name (funcall (lambda (v) (hydra_testing_test_group-name v)) subgroup)) (header (funcall (hydra_lib_strings_cat2 "    // ") group_name))) (funcall (hydra_lib_eithers_map (lambda (content) (hydra_lib_strings_cat (list header "\n\n" content)))) (funcall (hydra_ext_java_testing_generate_java_test_group_hierarchy (funcall (hydra_lib_lists_concat2 group_path) (list group_name))) subgroup))))) subgroups)))))))))

(defvar hydra_ext_java_testing_generate_test_file_with_java_codec (lambda (test_module) (lambda (test_group) (funcall (hydra_lib_eithers_map (lambda (test_body) (let* ((ns_ (funcall (lambda (v) (hydra_packaging_module-namespace v)) test_module)) (parts (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))) (class_name_ (funcall (hydra_lib_strings_cat2 (hydra_formatting_capitalize (hydra_lib_lists_last parts))) "Test")) (dir_parts (funcall (hydra_lib_lists_drop 1) (hydra_lib_lists_init parts))) (file_name (funcall (hydra_lib_strings_cat2 class_name_) ".java")) (file_path (hydra_lib_strings_cat (list (funcall (hydra_lib_strings_intercalate "/") dir_parts) "/" file_name))) (test_module_content (funcall (funcall (hydra_ext_java_testing_build_java_test_module test_module) test_group) test_body))) (list file_path test_module_content)))) (funcall (hydra_ext_java_testing_generate_java_test_group_hierarchy (list)) test_group)))))

(defvar hydra_ext_java_testing_generate_java_test_file (lambda (test_module) (lambda (test_group) (lambda (_g) (funcall (hydra_ext_java_testing_generate_test_file_with_java_codec test_module) test_group)))))

(defvar hydra_ext_java_testing_namespace_to_java_class_name (lambda (ns_) (funcall (hydra_lib_strings_intercalate ".") (funcall (hydra_lib_lists_map hydra_formatting_capitalize) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))))))

(provide 'hydra.ext.java.testing)
