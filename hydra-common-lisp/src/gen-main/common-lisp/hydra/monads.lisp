(defpackage :hydra.monads
(:use :cl :hydra.context :hydra.lib.lists :hydra.lib.maps :hydra.lib.maybes)
(:export :hydra_monads_empty_context :hydra_monads_maybe_to_list))

(in-package :hydra.monads)

(cl:defvar hydra_monads_empty_context (make-hydra_context_context (cl:list) (cl:list) hydra_lib_maps_empty))

(cl:defvar hydra_monads_maybe_to_list (cl:lambda (mx) (((hydra_lib_maybes_maybe (cl:list)) hydra_lib_lists_pure) mx)))
