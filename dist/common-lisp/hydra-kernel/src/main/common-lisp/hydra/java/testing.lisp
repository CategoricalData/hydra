(defpackage :hydra.java.testing
(:use :cl :hydra.constants :hydra.formatting :hydra.lib.eithers :hydra.lib.equality :hydra.lib.lists :hydra.lib.logic :hydra.lib.maybes :hydra.lib.strings :hydra.packaging :hydra.testing :hydra.util)
(:export :hydra_java_testing_build_java_test_module :hydra_java_testing_find_java_imports :hydra_java_testing_format_java_test_name :hydra_java_testing_generate_java_test_case :hydra_java_testing_generate_java_test_group_hierarchy :hydra_java_testing_generate_test_file_with_java_codec :hydra_java_testing_generate_java_test_file :hydra_java_testing_namespace_to_java_class_name))

(in-package :hydra.java.testing)

(cl:defvar hydra_java_testing_build_java_test_module (cl:lambda (test_module) (cl:lambda (test_group) (cl:lambda (test_body) (let* ((ns_ ((cl:lambda (v) (hydra_packaging_module-namespace v)) test_module)) (parts ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) ns_))) (class_name_ ((hydra_lib_strings_cat2 (hydra_formatting_capitalize ((hydra_lib_maybes_from_maybe (cl:lambda () "")) (hydra_lib_lists_maybe_last parts)))) "Test")) (group_name_ ((cl:lambda (v) (hydra_testing_test_group-name v)) test_group)) (package_name ((hydra_lib_strings_intercalate ".") ((hydra_lib_maybes_from_maybe (cl:lambda () (cl:list))) (hydra_lib_lists_maybe_init parts)))) (standard_imports (cl:list "import org.junit.jupiter.api.Test;" "import static org.junit.jupiter.api.Assertions.*;" "import java.util.*;" "import hydra.util.*;")) (header (hydra_lib_strings_cat (cl:list ((hydra_lib_strings_cat2 "// ") hydra_constants_warning_auto_generated_file) "
" ((hydra_lib_strings_cat2 "// ") group_name_) "

" (hydra_lib_strings_cat (cl:list "package " package_name ";

")) ((hydra_lib_strings_intercalate "
") standard_imports) "

" (hydra_lib_strings_cat (cl:list "public class " class_name_ " {

")))))) (hydra_lib_strings_cat (cl:list header test_body "
}
")))))))

(cl:defvar hydra_java_testing_find_java_imports (cl:list "import org.junit.jupiter.api.Test;" "import static org.junit.jupiter.api.Assertions.*;" "import java.util.*;"))

(cl:defvar hydra_java_testing_format_java_test_name (cl:lambda (name) (let* ((replaced ((hydra_lib_strings_intercalate " Neg") ((hydra_lib_strings_split_on "-") ((hydra_lib_strings_intercalate "Dot") ((hydra_lib_strings_split_on ".") ((hydra_lib_strings_intercalate " Plus") ((hydra_lib_strings_split_on "+") ((hydra_lib_strings_intercalate " Div") ((hydra_lib_strings_split_on "/") ((hydra_lib_strings_intercalate " Mul") ((hydra_lib_strings_split_on "*") ((hydra_lib_strings_intercalate " Num") ((hydra_lib_strings_split_on "#") name))))))))))))) (sanitized (hydra_formatting_non_alnum_to_underscores replaced)) (pascal_ (((hydra_formatting_convert_case (list :lower_snake cl:nil)) (list :pascal cl:nil)) sanitized))) ((hydra_lib_strings_cat2 "test") pascal_))))

(cl:defvar hydra_java_testing_generate_java_test_case (cl:lambda (group_path) (cl:lambda (tcm) (let* ((name_ ((cl:lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tcase ((cl:lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :universal) ((cl:lambda (ucase) (let* ((actual_ ((cl:lambda (v) (hydra_testing_universal_test_case-actual v)) ucase)) (expected_ ((cl:lambda (v) (hydra_testing_universal_test_case-expected v)) ucase)) (full_name (if (hydra_lib_lists_null group_path) name_ ((hydra_lib_strings_intercalate "_") ((hydra_lib_lists_concat2 group_path) (cl:list name_))))) (formatted_name (hydra_java_testing_format_java_test_name full_name))) (list :right (cl:list "    @Test" (hydra_lib_strings_cat (cl:list "    public void " formatted_name "() {")) "        assertEquals(" (hydra_lib_strings_cat (cl:list "            " expected_ ",")) (hydra_lib_strings_cat (cl:list "            " actual_ ");")) "    }")))) match_value)))) (cadr match_target))) tcase)))))

(cl:defvar hydra_java_testing_generate_java_test_group_hierarchy (cl:lambda (group_path) (cl:lambda (test_group) (let* ((cases_ ((cl:lambda (v) (hydra_testing_test_group-cases v)) test_group)) (subgroups ((cl:lambda (v) (hydra_testing_test_group-subgroups v)) test_group))) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map (cl:lambda (lines_) ((hydra_lib_strings_intercalate "

") (hydra_lib_lists_concat lines_)))) ((hydra_lib_eithers_map_list (cl:lambda (tc) ((hydra_java_testing_generate_java_test_case group_path) tc))) cases_))) (cl:lambda (test_cases_str) ((hydra_lib_eithers_map (cl:lambda (subgroups_str) (hydra_lib_strings_cat (cl:list test_cases_str (if ((hydra_lib_logic_or ((hydra_lib_equality_equal test_cases_str) "")) ((hydra_lib_equality_equal subgroups_str) "")) "" "

") subgroups_str)))) ((hydra_lib_eithers_map (cl:lambda (blocks) ((hydra_lib_strings_intercalate "

") blocks))) ((hydra_lib_eithers_map_list (cl:lambda (subgroup) (let* ((group_name ((cl:lambda (v) (hydra_testing_test_group-name v)) subgroup)) (header ((hydra_lib_strings_cat2 "    // ") group_name))) ((hydra_lib_eithers_map (cl:lambda (content) (hydra_lib_strings_cat (cl:list header "

" content)))) ((hydra_java_testing_generate_java_test_group_hierarchy ((hydra_lib_lists_concat2 group_path) (cl:list group_name))) subgroup))))) subgroups)))))))))

(cl:defvar hydra_java_testing_generate_test_file_with_java_codec (cl:lambda (test_module) (cl:lambda (test_group) ((hydra_lib_eithers_map (cl:lambda (test_body) (let* ((ns_ ((cl:lambda (v) (hydra_packaging_module-namespace v)) test_module)) (parts ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) ns_))) (class_name_ ((hydra_lib_strings_cat2 (hydra_formatting_capitalize ((hydra_lib_maybes_from_maybe (cl:lambda () "")) (hydra_lib_lists_maybe_last parts)))) "Test")) (dir_parts ((hydra_lib_lists_drop 1) ((hydra_lib_maybes_from_maybe (cl:lambda () (cl:list))) (hydra_lib_lists_maybe_init parts)))) (file_name ((hydra_lib_strings_cat2 class_name_) ".java")) (file_path (hydra_lib_strings_cat (cl:list ((hydra_lib_strings_intercalate "/") dir_parts) "/" file_name))) (test_module_content (((hydra_java_testing_build_java_test_module test_module) test_group) test_body))) (cl:list file_path test_module_content)))) ((hydra_java_testing_generate_java_test_group_hierarchy (cl:list)) test_group)))))

(cl:defvar hydra_java_testing_generate_java_test_file (cl:lambda (test_module) (cl:lambda (test_group) (cl:lambda (_g) ((hydra_java_testing_generate_test_file_with_java_codec test_module) test_group)))))

(cl:defvar hydra_java_testing_namespace_to_java_class_name (cl:lambda (ns_) ((hydra_lib_strings_intercalate ".") ((hydra_lib_lists_map hydra_formatting_capitalize) ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) ns_))))))
