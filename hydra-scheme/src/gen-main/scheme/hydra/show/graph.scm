(define-library (hydra show graph)
(export hydra_show_graph_graph)
(import (scheme base) (hydra lib lists) (hydra lib strings) (hydra show core))
(begin
(define hydra_show_graph_graph (lambda (elements) (let ((element_strs ((hydra_lib_lists_map hydra_show_core_binding) elements))) (hydra_lib_strings_cat (list "{" ((hydra_lib_strings_intercalate ", ") element_strs) "}")))))))
