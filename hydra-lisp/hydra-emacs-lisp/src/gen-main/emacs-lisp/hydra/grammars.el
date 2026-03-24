(require 'cl-lib)

(require 'hydra.annotations)

(require 'hydra.core)

(require 'hydra.formatting)

(require 'hydra.grammar)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.names)

(defvar hydra_grammars_child_name (lambda (lname) (lambda (n) (hydra_lib_strings_cat (list lname "_" (hydra_formatting_capitalize n))))))

(defvar hydra_grammars_raw_name (lambda (pat) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :alternatives) (funcall (lambda (_) "alts") match_value)) ((equal (car match_target) :constant) (funcall (lambda (c) (hydra_formatting_capitalize (hydra_formatting_with_character_aliases (funcall (lambda (v) v) c)))) match_value)) ((equal (car match_target) :ignored) (funcall (lambda (_) "ignored") match_value)) ((equal (car match_target) :labeled) (funcall (lambda (lp) (funcall (lambda (v) v) (funcall (lambda (v) (hydra_grammar_labeled_pattern-label v)) lp))) match_value)) ((equal (car match_target) :nil) (funcall (lambda (_) "none") match_value)) ((equal (car match_target) :nonterminal) (funcall (lambda (s) (hydra_formatting_capitalize (funcall (lambda (v) v) s))) match_value)) ((equal (car match_target) :option) (funcall (lambda (p) (hydra_formatting_capitalize (hydra_grammars_raw_name p))) match_value)) ((equal (car match_target) :plus) (funcall (lambda (p) (funcall (hydra_lib_strings_cat2 "listOf") (hydra_formatting_capitalize (hydra_grammars_raw_name p)))) match_value)) ((equal (car match_target) :regex) (funcall (lambda (_) "regex") match_value)) ((equal (car match_target) :sequence) (funcall (lambda (_) "sequence") match_value)) ((equal (car match_target) :star) (funcall (lambda (p) (funcall (hydra_lib_strings_cat2 "listOf") (hydra_formatting_capitalize (hydra_grammars_raw_name p)))) match_value)))) (cadr match_target))) pat)))

(defvar hydra_grammars_find_names (lambda (pats) (let ((next_name (lambda (acc) (lambda (pat) (let ((names (hydra_lib_pairs_first acc))) (let ((name_map (hydra_lib_pairs_second acc))) (let ((rn (hydra_grammars_raw_name pat))) (let ((name_and_index (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list rn 1))) (lambda (i) (list (funcall (hydra_lib_strings_cat2 rn) (hydra_lib_literals_show_int32 (funcall (hydra_lib_math_add i) 1))) (funcall (hydra_lib_math_add i) 1)))) (funcall (hydra_lib_maps_lookup rn) name_map)))) (let ((nn (hydra_lib_pairs_first name_and_index))) (let ((ni (hydra_lib_pairs_second name_and_index))) (list (funcall (hydra_lib_lists_cons nn) names) (funcall (funcall (hydra_lib_maps_insert rn) ni) name_map)))))))))))) (hydra_lib_lists_reverse (hydra_lib_pairs_first (funcall (funcall (hydra_lib_lists_foldl next_name) (list (list) hydra_lib_maps_empty)) pats))))))

(defvar hydra_grammars_simplify (lambda (is_record) (lambda (pats) (let ((is_constant (lambda (p) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :constant) (funcall (lambda (_) t) match_value)) (t nil))) (cadr match_target))) p)))) (if is_record (funcall (hydra_lib_lists_filter (lambda (p) (hydra_lib_logic_not (is_constant p)))) pats) pats)))))

(defvar hydra_grammars_is_nontrivial (lambda (is_record) (lambda (pats) (let ((min_pats (funcall (hydra_grammars_simplify is_record) pats))) (let ((is_labeled (lambda (p) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :labeled) (funcall (lambda (_) t) match_value)) (t nil))) (cadr match_target))) p)))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length min_pats)) 1) (is_labeled (hydra_lib_lists_head min_pats)) t))))))

(defvar hydra_grammars_is_complex (lambda (pat) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :labeled) (funcall (lambda (lp) (hydra_grammars_is_complex (funcall (lambda (v) (hydra_grammar_labeled_pattern-pattern v)) lp))) match_value)) ((equal (car match_target) :sequence) (funcall (lambda (pats) (funcall (hydra_grammars_is_nontrivial t) pats)) match_value)) ((equal (car match_target) :alternatives) (funcall (lambda (pats) (funcall (hydra_grammars_is_nontrivial nil) pats)) match_value)) (t nil))) (cadr match_target))) pat)))

(defvar hydra_grammars_to_name (lambda (ns_) (lambda (local) (hydra_names_unqualify_name (make-hydra_module_qualified_name (list :just ns_) local)))))

(defvar hydra_grammars_make_elements (lambda (omit_trivial) (lambda (ns_) (lambda (lname) (lambda (pat) (let ((trivial (if omit_trivial (list) (list (list lname (list :unit nil)))))) (let ((descend (lambda (n) (lambda (f) (lambda (p) (let ((cpairs (funcall (funcall (funcall (hydra_grammars_make_elements nil) ns_) (funcall (hydra_grammars_child_name lname) n)) p))) (f (if (hydra_grammars_is_complex p) (funcall (hydra_lib_lists_cons (list lname (list :variable (funcall (hydra_grammars_to_name ns_) (hydra_lib_pairs_first (hydra_lib_lists_head cpairs)))))) cpairs) (if (hydra_lib_lists_null cpairs) (list (list lname (list :unit nil))) (funcall (hydra_lib_lists_cons (list lname (hydra_lib_pairs_second (hydra_lib_lists_head cpairs)))) (hydra_lib_lists_tail cpairs))))))))))) (let ((mod (lambda (n) (lambda (f) (lambda (p) (funcall (funcall (descend n) (lambda (pairs) (funcall (hydra_lib_lists_cons (list lname (f (hydra_lib_pairs_second (hydra_lib_lists_head pairs))))) (hydra_lib_lists_tail pairs)))) p)))))) (letrec ((for_pat (lambda (pat) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :alternatives) (funcall (lambda (pats) (funcall (funcall (for_record_or_union nil) (lambda (fields) (list :union fields))) pats)) match_value)) ((equal (car match_target) :constant) (funcall (lambda (_) trivial) match_value)) ((equal (car match_target) :ignored) (funcall (lambda (_) (list)) match_value)) ((equal (car match_target) :labeled) (funcall (lambda (lp) (for_pat (funcall (lambda (v) (hydra_grammar_labeled_pattern-pattern v)) lp))) match_value)) ((equal (car match_target) :nil) (funcall (lambda (_) trivial) match_value)) ((equal (car match_target) :nonterminal) (funcall (lambda (s) (list (list lname (list :variable (funcall (hydra_grammars_to_name ns_) (funcall (lambda (v) v) s)))))) match_value)) ((equal (car match_target) :option) (funcall (lambda (p) (funcall (funcall (mod "Option") (lambda (x) (list :maybe x))) p)) match_value)) ((equal (car match_target) :plus) (funcall (lambda (p) (funcall (funcall (mod "Elmt") (lambda (x) (list :list x))) p)) match_value)) ((equal (car match_target) :regex) (funcall (lambda (_) (list (list lname (list :literal (list :string nil))))) match_value)) ((equal (car match_target) :sequence) (funcall (lambda (pats) (funcall (funcall (for_record_or_union t) (lambda (fields) (list :record fields))) pats)) match_value)) ((equal (car match_target) :star) (funcall (lambda (p) (funcall (funcall (mod "Elmt") (lambda (x) (list :list x))) p)) match_value)))) (cadr match_target))) pat))) (for_record_or_union (lambda (is_record) (lambda (construct) (lambda (pats) (let ((min_pats (funcall (hydra_grammars_simplify is_record) pats))) (let ((field_names (hydra_grammars_find_names min_pats))) (let ((to_field (lambda (n) (lambda (p) (funcall (funcall (descend n) (lambda (pairs) (list (make-hydra_core_field_type n (hydra_lib_pairs_second (hydra_lib_lists_head pairs))) (hydra_lib_lists_tail pairs)))) p))))) (let ((field_pairs (funcall (funcall (hydra_lib_lists_zip_with to_field) field_names) min_pats))) (let ((fields (funcall (hydra_lib_lists_map hydra_lib_pairs_first) field_pairs))) (let ((els (hydra_lib_lists_concat (funcall (hydra_lib_lists_map hydra_lib_pairs_second) field_pairs)))) (if (funcall (hydra_grammars_is_nontrivial is_record) pats) (funcall (hydra_lib_lists_cons (list lname (construct fields))) els) (for_pat (hydra_lib_lists_head min_pats)))))))))))))) (for_pat pat))))))))))

(defvar hydra_grammars_replace_placeholders (lambda (el_name) (lambda (typ) typ)))

(defvar hydra_grammars_wrap_type (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (_) t_) match_value)) ((equal (car match_target) :union) (funcall (lambda (_) t_) match_value)) ((equal (car match_target) :wrap) (funcall (lambda (_) t_) match_value)) (t (list :wrap t_)))) (cadr match_target))) t_)))

(defvar hydra_grammars_grammar_to_module (lambda (ns_) (lambda (grammar) (lambda (desc) (let ((prod_pairs (funcall (hydra_lib_lists_map (lambda (prod) (list (funcall (lambda (v) v) (funcall (lambda (v) (hydra_grammar_production-symbol v)) prod)) (funcall (lambda (v) (hydra_grammar_production-pattern v)) prod)))) (funcall (lambda (v) v) grammar)))) (let ((capitalized_names (funcall (hydra_lib_lists_map (lambda (pair) (hydra_formatting_capitalize (hydra_lib_pairs_first pair)))) prod_pairs))) (let ((patterns (funcall (hydra_lib_lists_map (lambda (pair) (hydra_lib_pairs_second pair))) prod_pairs))) (let ((element_pairs (hydra_lib_lists_concat (funcall (funcall (hydra_lib_lists_zip_with (funcall (hydra_grammars_make_elements nil) ns_)) capitalized_names) patterns)))) (let ((elements (funcall (hydra_lib_lists_map (lambda (pair) (let ((lname (hydra_lib_pairs_first pair))) (let ((el_name (funcall (hydra_grammars_to_name ns_) lname))) (let ((typ (funcall (hydra_grammars_replace_placeholders el_name) (hydra_grammars_wrap_type (hydra_lib_pairs_second pair))))) (funcall (hydra_annotations_type_element el_name) typ)))))) element_pairs))) (make-hydra_module_module ns_ (funcall (hydra_lib_lists_map (lambda (b) (list :term (make-hydra_module_term_definition (funcall (lambda (v) (hydra_core_binding-name v)) b) (funcall (lambda (v) (hydra_core_binding-term v)) b) (list :nothing))))) elements) (list) (list) desc))))))))))

(provide 'hydra.grammars)
