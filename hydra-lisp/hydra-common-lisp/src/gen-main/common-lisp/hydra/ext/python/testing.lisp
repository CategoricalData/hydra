(defpackage :hydra.ext.python.testing
(:use :cl :hydra.constants :hydra.lib.chars :hydra.lib.eithers :hydra.lib.equality :hydra.lib.lists :hydra.lib.logic :hydra.lib.strings :hydra.packaging :hydra.testing)
(:export :hydra_ext_python_testing_build_python_test_module :hydra_ext_python_testing_format_python_test_name :hydra_ext_python_testing_generate_python_test_case :hydra_ext_python_testing_generate_python_test_group_hierarchy :hydra_ext_python_testing_generate_test_file_with_python_codec :hydra_ext_python_testing_generate_python_test_file))

(in-package :hydra.ext.python.testing)

(cl:defvar hydra_ext_python_testing_build_python_test_module (cl:lambda (test_module) (cl:lambda (test_group) (cl:lambda (test_body) (let* ((group_name_ ((cl:lambda (v) (hydra_testing_test_group-name v)) test_group)) (header (hydra_lib_strings_cat (cl:list ((hydra_lib_strings_cat2 "# ") hydra_constants_warning_auto_generated_file) "
" ((hydra_lib_strings_cat2 "# ") group_name_) "

")))) (hydra_lib_strings_cat (cl:list header test_body "
")))))))

(cl:defvar hydra_ext_python_testing_format_python_test_name (cl:lambda (name) ((hydra_lib_strings_cat2 "test_") (hydra_lib_strings_from_list ((hydra_lib_lists_map (cl:lambda (c) (if (hydra_lib_chars_is_alpha_num c) (hydra_lib_chars_to_lower c) 95))) (hydra_lib_strings_to_list name))))))

(cl:defvar hydra_ext_python_testing_generate_python_test_case (cl:lambda (group_path) (cl:lambda (tcm) (let* ((name_ ((cl:lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tcase ((cl:lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :universal) ((cl:lambda (ucase) (let* ((actual_ ((cl:lambda (v) (hydra_testing_universal_test_case-actual v)) ucase)) (expected_ ((cl:lambda (v) (hydra_testing_universal_test_case-expected v)) ucase)) (full_name (if (hydra_lib_lists_null group_path) name_ ((hydra_lib_strings_intercalate "__") ((hydra_lib_lists_concat2 group_path) (cl:list name_))))) (formatted_name (hydra_ext_python_testing_format_python_test_name full_name))) (list :right (cl:list (hydra_lib_strings_cat (cl:list "def " formatted_name "():")) (hydra_lib_strings_cat (cl:list "    assert (" actual_ ") == (" expected_ ")")))))) match_value)))) (cadr match_target))) tcase)))))

(cl:defvar hydra_ext_python_testing_generate_python_test_group_hierarchy (cl:lambda (group_path) (cl:lambda (test_group) (let* ((cases_ ((cl:lambda (v) (hydra_testing_test_group-cases v)) test_group)) (subgroups ((cl:lambda (v) (hydra_testing_test_group-subgroups v)) test_group))) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list (cl:lambda (tc) ((hydra_ext_python_testing_generate_python_test_case group_path) tc))) cases_)) (cl:lambda (test_case_lines) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list (cl:lambda (subgroup) (let* ((group_name ((cl:lambda (v) (hydra_testing_test_group-name v)) subgroup)) (header ((hydra_lib_strings_cat2 "# ") group_name))) ((hydra_lib_eithers_map (cl:lambda (content) (hydra_lib_strings_cat (cl:list header "

" content)))) ((hydra_ext_python_testing_generate_python_test_group_hierarchy ((hydra_lib_lists_concat2 group_path) (cl:list group_name))) subgroup))))) subgroups)) (cl:lambda (subgroup_blocks) (let* ((subgroups_str ((hydra_lib_strings_intercalate "

") subgroup_blocks)) (test_cases_str ((hydra_lib_strings_intercalate "

") (hydra_lib_lists_concat test_case_lines)))) (list :right (hydra_lib_strings_cat (cl:list test_cases_str (if ((hydra_lib_logic_or ((hydra_lib_equality_equal test_cases_str) "")) ((hydra_lib_equality_equal subgroups_str) "")) "" "

") subgroups_str))))))))))))

(cl:defvar hydra_ext_python_testing_generate_test_file_with_python_codec (cl:lambda (test_module) (cl:lambda (test_group) ((hydra_lib_eithers_map (cl:lambda (test_body) (let* ((ns_ ((cl:lambda (v) (hydra_packaging_module-namespace v)) test_module)) (parts ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) ns_))) (dir_parts (hydra_lib_lists_init parts)) (file_name (hydra_lib_strings_cat (cl:list "test_" (hydra_lib_lists_last parts) ".py"))) (file_path (hydra_lib_strings_cat (cl:list ((hydra_lib_strings_intercalate "/") dir_parts) "/" file_name))) (test_module_content (((hydra_ext_python_testing_build_python_test_module test_module) test_group) test_body))) (cl:list file_path test_module_content)))) ((hydra_ext_python_testing_generate_python_test_group_hierarchy (cl:list)) test_group)))))

(cl:defvar hydra_ext_python_testing_generate_python_test_file (cl:lambda (test_module) (cl:lambda (test_group) (cl:lambda (_g) ((hydra_ext_python_testing_generate_test_file_with_python_codec test_module) test_group)))))
