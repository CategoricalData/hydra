(define-library (hydra show typing)
(export hydra_show_typing_type_constraint hydra_show_typing_type_subst)
(import (scheme base) (hydra core) (hydra lib lists) (hydra lib maps) (hydra lib pairs) (hydra lib strings) (hydra show core) (hydra typing))
(begin
(define hydra_show_typing_type_constraint (lambda (tc) (let* ((ltyp ((lambda (v) (hydra_typing_type_constraint-left v)) tc)) (rtyp ((lambda (v) (hydra_typing_type_constraint-right v)) tc))) (hydra_lib_strings_cat (list (hydra_show_core_type ltyp) "≡" (hydra_show_core_type rtyp))))))
(define hydra_show_typing_type_subst (lambda (ts) (let* ((subst ((lambda (v) v) ts)) (pairs (hydra_lib_maps_to_list subst)) (show_pair (lambda (pair) (let* ((name ((lambda (v) v) (hydra_lib_pairs_first pair))) (typ (hydra_lib_pairs_second pair))) (hydra_lib_strings_cat (list name "↦" (hydra_show_core_type typ)))))) (pair_strs ((hydra_lib_lists_map show_pair) pairs))) (hydra_lib_strings_cat (list "{" ((hydra_lib_strings_intercalate ",") pair_strs) "}")))))))
