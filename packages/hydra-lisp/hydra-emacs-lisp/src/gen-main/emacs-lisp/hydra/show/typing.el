(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(require 'hydra.typing)

(defvar hydra_show_typing_type_constraint (lambda (tc) (let* ((ltyp (funcall (lambda (v) (hydra_typing_type_constraint-left v)) tc)) (rtyp (funcall (lambda (v) (hydra_typing_type_constraint-right v)) tc))) (hydra_lib_strings_cat (list (hydra_show_core_type ltyp) "≡" (hydra_show_core_type rtyp))))))

(defvar hydra_show_typing_type_subst (lambda (ts) (let* ((subst (funcall (lambda (v) v) ts)) (pairs (hydra_lib_maps_to_list subst)) (show_pair (lambda (pair) (let* ((name (funcall (lambda (v) v) (hydra_lib_pairs_first pair))) (typ (hydra_lib_pairs_second pair))) (hydra_lib_strings_cat (list name "↦" (hydra_show_core_type typ)))))) (pair_strs (funcall (hydra_lib_lists_map show_pair) pairs))) (hydra_lib_strings_cat (list "{" (funcall (hydra_lib_strings_intercalate ",") pair_strs) "}")))))

(provide 'hydra.show.typing)
