(defpackage :hydra.show.graph
(:use :cl :hydra.lib.lists :hydra.lib.strings :hydra.show.core)
(:export :hydra_show_graph_graph))

(in-package :hydra.show.graph)

(cl:defvar hydra_show_graph_graph (cl:lambda (elements) (let ((element_strs ((hydra_lib_lists_map hydra_show_core_binding) elements))) (hydra_lib_strings_cat (cl:list "{" ((hydra_lib_strings_intercalate ", ") element_strs) "}")))))
