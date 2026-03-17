(defpackage :hydra.show.typing
(:use :cl :hydra.core :hydra.lib.lists :hydra.lib.maps :hydra.lib.pairs :hydra.lib.strings :hydra.show.core :hydra.typing)
(:export :hydra_show_typing_type_constraint :hydra_show_typing_type_subst))

(in-package :hydra.show.typing)

(cl:defvar hydra_show_typing_type_constraint (cl:lambda (tc) (let* ((ltyp ((cl:lambda (v) (hydra_typing_type_constraint-left v)) tc)) (rtyp ((cl:lambda (v) (hydra_typing_type_constraint-right v)) tc))) (hydra_lib_strings_cat (cl:list (hydra_show_core_type ltyp) "≡" (hydra_show_core_type rtyp))))))

(cl:defvar hydra_show_typing_type_subst (cl:lambda (ts) (let* ((subst ((cl:lambda (v) v) ts)) (pairs (hydra_lib_maps_to_list subst)) (show_pair (cl:lambda (pair) (let* ((name ((cl:lambda (v) v) (hydra_lib_pairs_first pair))) (typ (hydra_lib_pairs_second pair))) (hydra_lib_strings_cat (cl:list name "↦" (hydra_show_core_type typ)))))) (pair_strs ((hydra_lib_lists_map show_pair) pairs))) (hydra_lib_strings_cat (cl:list "{" ((hydra_lib_strings_intercalate ",") pair_strs) "}")))))
