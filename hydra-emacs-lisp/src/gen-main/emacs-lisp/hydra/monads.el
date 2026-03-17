(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(defvar hydra_monads_empty_context (make-hydra_context_context (list) (list) hydra_lib_maps_empty))

(defvar hydra_monads_maybe_to_list (lambda (mx) (((hydra_lib_maybes_maybe (list)) hydra_lib_lists_pure) mx)))

(provide 'hydra.monads)
