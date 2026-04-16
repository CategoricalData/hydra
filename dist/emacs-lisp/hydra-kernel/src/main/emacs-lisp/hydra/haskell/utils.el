(require 'cl-lib)

(require 'hydra.analysis)

(require 'hydra.core)

(require 'hydra.formatting)

(require 'hydra.haskell.language)

(require 'hydra.haskell.syntax)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.names)

(require 'hydra.packaging)

(require 'hydra.strip)

(defvar hydra_haskell_utils_application_pattern (lambda (name) (lambda (args) (list :application (make-hydra_haskell_syntax_application_pattern name args)))))

(defvar hydra_haskell_utils_raw_name (lambda (n) (list :normal (make-hydra_haskell_syntax_qualified_name (list) n))))

(defvar hydra_haskell_utils_sanitize_haskell_name (hydra_formatting_sanitize_with_underscores hydra_haskell_language_reserved_words))

(defvar hydra_haskell_utils_simple_name (lambda (arg_) (hydra_haskell_utils_raw_name (hydra_haskell_utils_sanitize_haskell_name arg_))))

(defvar hydra_haskell_utils_element_reference (lambda (namespaces) (lambda (name) (let* ((qname (hydra_names_qualify_name name)) (local (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) qname)) (esc_local (hydra_haskell_utils_sanitize_haskell_name local)) (namespace_pair (funcall (lambda (v) (hydra_packaging_namespaces-focus v)) namespaces)) (gmod (funcall (lambda (v) v) (hydra_lib_pairs_second namespace_pair))) (gname (hydra_lib_pairs_first namespace_pair)) (mns (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qname)) (namespaces_map (funcall (lambda (v) (hydra_packaging_namespaces-mapping v)) namespaces))) (funcall (funcall (hydra_lib_maybes_cases (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qname)) (lambda () (hydra_haskell_utils_simple_name local))) (lambda (ns_) (funcall (funcall (hydra_lib_maybes_cases (funcall (hydra_lib_maps_lookup ns_) namespaces_map)) (lambda () (hydra_haskell_utils_simple_name local))) (lambda (mn) (let ((alias_str (funcall (lambda (v) v) mn))) (if (funcall (hydra_lib_equality_equal ns_) gname) (hydra_haskell_utils_simple_name esc_local) (hydra_haskell_utils_raw_name (hydra_lib_strings_cat (list alias_str "." (hydra_haskell_utils_sanitize_haskell_name local))))))))))))))

(defvar hydra_haskell_utils_hsapp (lambda (l) (lambda (r) (list :application (make-hydra_haskell_syntax_application_expression l r)))))

(defvar hydra_haskell_utils_hslambda (lambda (name) (lambda (rhs) (list :lambda (make-hydra_haskell_syntax_lambda_expression (list (list :name name)) rhs)))))

(defvar hydra_haskell_utils_hslit (lambda (lit) (list :literal lit)))

(defvar hydra_haskell_utils_hsvar (lambda (s) (list :variable (hydra_haskell_utils_raw_name s))))

(defvar hydra_haskell_utils_namespaces_for_module (lambda (mod) (lambda (cx) (lambda (g) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (funcall (funcall (funcall (hydra_analysis_module_dependency_namespaces cx) g) t) t) t) t) mod)) (lambda (nss) (let ((ns_ (funcall (lambda (v) (hydra_packaging_module-namespace v)) mod))) (let ((segments_of (lambda (namespace) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) namespace))))) (let ((alias_from_suffix (lambda (segs) (lambda (n) (let* ((drop_count (funcall (hydra_lib_math_sub (hydra_lib_lists_length segs)) n)) (suffix (funcall (hydra_lib_lists_drop drop_count) segs)) (capitalized_suffix (funcall (hydra_lib_lists_map hydra_formatting_capitalize) suffix))) (hydra_lib_strings_cat capitalized_suffix)))))) (let ((to_module_name (lambda (namespace) (funcall (alias_from_suffix (segments_of namespace)) 1)))) (let ((focus_pair (list ns_ (to_module_name ns_)))) (let ((nss_as_list (hydra_lib_sets_to_list nss))) (let ((segs_map (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (nm) (list nm (segments_of nm)))) nss_as_list)))) (let ((max_segs (funcall (funcall (hydra_lib_lists_foldl (lambda (a) (lambda (b) (if (funcall (hydra_lib_equality_gt a) b) a b)))) 1) (funcall (hydra_lib_lists_map (lambda (nm) (hydra_lib_lists_length (segments_of nm)))) nss_as_list)))) (let ((initial_state (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (nm) (list nm 1))) nss_as_list)))) (let ((segs_for (lambda (nm) (funcall (hydra_lib_maybes_from_maybe (lambda () (list))) (funcall (hydra_lib_maps_lookup nm) segs_map))))) (let ((taken_for (lambda (state) (lambda (nm) (funcall (hydra_lib_maybes_from_maybe (lambda () 1)) (funcall (hydra_lib_maps_lookup nm) state)))))) (let ((grow_step (lambda (state) (lambda (_ign) (let* ((alias_entries (funcall (hydra_lib_lists_map (lambda (nm) (let* ((n (funcall (taken_for state) nm)) (segs (segs_for nm)) (alias_str (funcall (lambda (v) v) (funcall (alias_from_suffix segs) n))) (seg_count (hydra_lib_lists_length segs))) (list nm (list n (list seg_count alias_str)))))) nss_as_list)) (alias_counts (funcall (funcall (hydra_lib_lists_foldl (lambda (m) (lambda (e) (let ((k (hydra_lib_pairs_second (hydra_lib_pairs_second (hydra_lib_pairs_second e))))) (funcall (funcall (hydra_lib_maps_insert k) (funcall (hydra_lib_math_add 1) (funcall (hydra_lib_maybes_from_maybe (lambda () 0)) (funcall (hydra_lib_maps_lookup k) m)))) m))))) hydra_lib_maps_empty) alias_entries)) (alias_min_segs (funcall (funcall (hydra_lib_lists_foldl (lambda (m) (lambda (e) (let* ((k (hydra_lib_pairs_second (hydra_lib_pairs_second (hydra_lib_pairs_second e)))) (existing (funcall (hydra_lib_maps_lookup k) m)) (seg_count (hydra_lib_pairs_first (hydra_lib_pairs_second (hydra_lib_pairs_second e))))) (funcall (funcall (hydra_lib_maps_insert k) (funcall (funcall (hydra_lib_maybes_cases existing) (lambda () seg_count)) (lambda (prev) (if (funcall (hydra_lib_equality_lt seg_count) prev) seg_count prev)))) m))))) hydra_lib_maps_empty) alias_entries))) (let ((alias_min_segs_count (funcall (funcall (hydra_lib_lists_foldl (lambda (m) (lambda (e) (let* ((k (hydra_lib_pairs_second (hydra_lib_pairs_second (hydra_lib_pairs_second e)))) (seg_count (hydra_lib_pairs_first (hydra_lib_pairs_second (hydra_lib_pairs_second e)))) (min_segs (funcall (hydra_lib_maybes_from_maybe (lambda () seg_count)) (funcall (hydra_lib_maps_lookup k) alias_min_segs)))) (if (funcall (hydra_lib_equality_equal seg_count) min_segs) (funcall (funcall (hydra_lib_maps_insert k) (funcall (hydra_lib_math_add 1) (funcall (hydra_lib_maybes_from_maybe (lambda () 0)) (funcall (hydra_lib_maps_lookup k) m)))) m) m))))) hydra_lib_maps_empty) alias_entries))) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (e) (let* ((alias_str (hydra_lib_pairs_second (hydra_lib_pairs_second (hydra_lib_pairs_second e)))) (count (funcall (hydra_lib_maybes_from_maybe (lambda () 0)) (funcall (hydra_lib_maps_lookup alias_str) alias_counts))) (seg_count (hydra_lib_pairs_first (hydra_lib_pairs_second (hydra_lib_pairs_second e)))) (min_segs (funcall (hydra_lib_maybes_from_maybe (lambda () seg_count)) (funcall (hydra_lib_maps_lookup alias_str) alias_min_segs))) (min_segs_count (funcall (hydra_lib_maybes_from_maybe (lambda () 0)) (funcall (hydra_lib_maps_lookup alias_str) alias_min_segs_count))) (n (hydra_lib_pairs_first (hydra_lib_pairs_second e))) (can_grow (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gt count) 1)) (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gt seg_count) n)) (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_gt seg_count) min_segs)) (funcall (hydra_lib_equality_gt min_segs_count) 1))))) (new_n (if can_grow (funcall (hydra_lib_math_add n) 1) n)) (nm (hydra_lib_pairs_first e))) (list nm new_n)))) alias_entries)))))))) (let ((final_state (funcall (funcall (hydra_lib_lists_foldl grow_step) initial_state) (funcall (hydra_lib_lists_replicate max_segs) nil)))) (let ((result_map (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (nm) (list nm (funcall (alias_from_suffix (segs_for nm)) (funcall (taken_for final_state) nm))))) nss_as_list)))) (list :right (make-hydra_packaging_namespaces focus_pair result_map))))))))))))))))))))))

(defvar hydra_haskell_utils_newtype_accessor_name (lambda (name) (funcall (hydra_lib_strings_cat2 "un") (hydra_names_local_name_of name))))

(defvar hydra_haskell_utils_type_name_for_record (lambda (sname) (let* ((sname_str (funcall (lambda (v) v) sname)) (parts (funcall (hydra_lib_strings_split_on ".") sname_str))) (funcall (hydra_lib_maybes_from_maybe (lambda () sname_str)) (hydra_lib_lists_maybe_last parts)))))

(defvar hydra_haskell_utils_record_field_reference (lambda (namespaces) (lambda (sname) (lambda (fname) (let* ((fname_str (funcall (lambda (v) v) fname)) (capitalized (hydra_formatting_capitalize fname_str)) (type_name_str (hydra_haskell_utils_type_name_for_record sname)) (decapitalized (hydra_formatting_decapitalize type_name_str)) (nm (funcall (hydra_lib_strings_cat2 decapitalized) capitalized)) (qname (hydra_names_qualify_name sname)) (ns_ (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qname)) (qual_name (make-hydra_packaging_qualified_name ns_ nm)) (unqual_name (hydra_names_unqualify_name qual_name))) (funcall (hydra_haskell_utils_element_reference namespaces) unqual_name))))))

(defvar hydra_haskell_utils_simple_value_binding (lambda (hname) (lambda (rhs) (lambda (bindings) (let* ((pat (list :application (make-hydra_haskell_syntax_application_pattern hname (list)))) (right_hand_side rhs)) (list :simple (make-hydra_haskell_syntax_simple_value_binding pat right_hand_side bindings)))))))

(defvar hydra_haskell_utils_to_type_application (lambda (types) (letrec ((dummy_type (list :variable (list :normal (make-hydra_haskell_syntax_qualified_name (list) "")))) (app (lambda (l) (funcall (hydra_lib_maybes_from_maybe (lambda () dummy_type)) (funcall (hydra_lib_maybes_map (lambda (p) (if (hydra_lib_lists_null (hydra_lib_pairs_second p)) (hydra_lib_pairs_first p) (list :application (make-hydra_haskell_syntax_application_type (app (hydra_lib_pairs_second p)) (hydra_lib_pairs_first p)))))) (hydra_lib_lists_uncons l)))))) (app (hydra_lib_lists_reverse types)))))

(defvar hydra_haskell_utils_union_field_reference (lambda (bound_names) (lambda (namespaces) (lambda (sname) (lambda (fname) (letrec ((fname_str (funcall (lambda (v) v) fname)) (capitalized_field_name (hydra_formatting_capitalize fname_str)) (type_name_str (hydra_haskell_utils_type_name_for_record sname)) (capitalized_type_name (hydra_formatting_capitalize type_name_str)) (qname (hydra_names_qualify_name sname)) (ns_ (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qname)) (deconflict (lambda (name) (let ((tname (hydra_names_unqualify_name (make-hydra_packaging_qualified_name ns_ name)))) (if (funcall (hydra_lib_sets_member tname) bound_names) (deconflict (funcall (hydra_lib_strings_cat2 name) "_")) name)))) (nm (deconflict (funcall (hydra_lib_strings_cat2 capitalized_type_name) capitalized_field_name))) (qual_name (make-hydra_packaging_qualified_name ns_ nm)) (unqual_name (hydra_names_unqualify_name qual_name))) (funcall (hydra_haskell_utils_element_reference namespaces) unqual_name)))))))

(defvar hydra_haskell_utils_unpack_forall_type (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :forall) (funcall (lambda (fat) (let* ((tbody (funcall (lambda (v) (hydra_core_forall_type-body v)) fat)) (recursive_result (hydra_haskell_utils_unpack_forall_type tbody)) (final_type (hydra_lib_pairs_second recursive_result)) (v (funcall (lambda (v) (hydra_core_forall_type-parameter v)) fat)) (vars (hydra_lib_pairs_first recursive_result))) (list (funcall (hydra_lib_lists_cons v) vars) final_type))) match_value)) (t (list (list) t_)))) (cadr match_target))) (hydra_strip_deannotate_type t_))))

(provide 'hydra.haskell.utils)
