(require 'cl-lib)

(require 'hydra.lib.lists)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(defvar hydra_show_graph_graph (lambda (elements) (let ((element_strs (funcall (hydra_lib_lists_map hydra_show_core_binding) elements))) (hydra_lib_strings_cat (list "{" (funcall (hydra_lib_strings_intercalate ", ") element_strs) "}")))))

(provide 'hydra.show.graph)
