(require 'cl-lib)

(require 'hydra.constants)

(require 'hydra.lib.chars)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.strings)

(require 'hydra.packaging)

(require 'hydra.testing)

(defvar hydra_python_testing_build_python_test_module (lambda (test_module) (lambda (test_group) (lambda (test_body) (let* ((group_name_ (funcall (lambda (v) (hydra_testing_test_group-name v)) test_group)) (header (hydra_lib_strings_cat (list (funcall (hydra_lib_strings_cat2 "# ") hydra_constants_warning_auto_generated_file) "\n" (funcall (hydra_lib_strings_cat2 "# ") group_name_) "\n\n")))) (hydra_lib_strings_cat (list header test_body "\n")))))))

(defvar hydra_python_testing_format_python_test_name (lambda (name) (funcall (hydra_lib_strings_cat2 "test_") (hydra_lib_strings_from_list (funcall (hydra_lib_lists_map (lambda (c) (if (hydra_lib_chars_is_alpha_num c) (hydra_lib_chars_to_lower c) 95))) (hydra_lib_strings_to_list name))))))

(defvar hydra_python_testing_generate_python_test_case (lambda (group_path) (lambda (tcm) (let* ((name_ (funcall (lambda (v) (hydra_testing_test_case_with_metadata-name v)) tcm)) (tcase (funcall (lambda (v) (hydra_testing_test_case_with_metadata-case v)) tcm))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :universal) (funcall (lambda (ucase) (let* ((actual_ (funcall (lambda (v) (hydra_testing_universal_test_case-actual v)) ucase)) (expected_ (funcall (lambda (v) (hydra_testing_universal_test_case-expected v)) ucase)) (full_name (if (hydra_lib_lists_null group_path) name_ (funcall (hydra_lib_strings_intercalate "__") (funcall (hydra_lib_lists_concat2 group_path) (list name_))))) (formatted_name (hydra_python_testing_format_python_test_name full_name))) (list :right (list (hydra_lib_strings_cat (list "def " formatted_name "():")) (hydra_lib_strings_cat (list "    assert (" actual_ ") == (" expected_ ")")))))) match_value)))) (cadr match_target))) tcase)))))

(defvar hydra_python_testing_generate_python_test_group_hierarchy (lambda (group_path) (lambda (test_group) (let* ((cases_ (funcall (lambda (v) (hydra_testing_test_group-cases v)) test_group)) (subgroups (funcall (lambda (v) (hydra_testing_test_group-subgroups v)) test_group))) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map_list (lambda (tc) (funcall (hydra_python_testing_generate_python_test_case group_path) tc))) cases_)) (lambda (test_case_lines) (funcall (hydra_lib_eithers_bind (funcall (hydra_lib_eithers_map_list (lambda (subgroup) (let* ((group_name (funcall (lambda (v) (hydra_testing_test_group-name v)) subgroup)) (header (funcall (hydra_lib_strings_cat2 "# ") group_name))) (funcall (hydra_lib_eithers_map (lambda (content) (hydra_lib_strings_cat (list header "\n\n" content)))) (funcall (hydra_python_testing_generate_python_test_group_hierarchy (funcall (hydra_lib_lists_concat2 group_path) (list group_name))) subgroup))))) subgroups)) (lambda (subgroup_blocks) (let* ((subgroups_str (funcall (hydra_lib_strings_intercalate "\n\n") subgroup_blocks)) (test_cases_str (funcall (hydra_lib_strings_intercalate "\n\n") (hydra_lib_lists_concat test_case_lines)))) (list :right (hydra_lib_strings_cat (list test_cases_str (if (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_equal test_cases_str) "")) (funcall (hydra_lib_equality_equal subgroups_str) "")) "" "\n\n") subgroups_str))))))))))))

(defvar hydra_python_testing_generate_test_file_with_python_codec (lambda (test_module) (lambda (test_group) (funcall (hydra_lib_eithers_map (lambda (test_body) (let* ((ns_ (funcall (lambda (v) (hydra_packaging_module-namespace v)) test_module)) (parts (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))) (dir_parts (hydra_lib_lists_init parts)) (file_name (hydra_lib_strings_cat (list "test_" (hydra_lib_lists_last parts) ".py"))) (file_path (hydra_lib_strings_cat (list (funcall (hydra_lib_strings_intercalate "/") dir_parts) "/" file_name))) (test_module_content (funcall (funcall (hydra_python_testing_build_python_test_module test_module) test_group) test_body))) (list file_path test_module_content)))) (funcall (hydra_python_testing_generate_python_test_group_hierarchy (list)) test_group)))))

(defvar hydra_python_testing_generate_python_test_file (lambda (test_module) (lambda (test_group) (lambda (_g) (funcall (hydra_python_testing_generate_test_file_with_python_codec test_module) test_group)))))

(provide 'hydra.python.testing)
