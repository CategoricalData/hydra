(defpackage :hydra.phantoms
(:use :cl :hydra.core)
(:export :make-hydra_phantoms_t_binding :hydra_phantoms_t_binding? :hydra_phantoms_t_binding-name :hydra_phantoms_t_binding-term :make-hydra_phantoms_t_term :hydra_phantoms_t_term? :hydra_phantoms_t_term-value :make-hydra_phantoms_t_term_definition :hydra_phantoms_t_term_definition? :hydra_phantoms_t_term_definition-name :hydra_phantoms_t_term_definition-term))

(in-package :hydra.phantoms)

(cl:defstruct hydra_phantoms_t_binding name term)

(cl:defstruct hydra_phantoms_t_term value)

(cl:defstruct hydra_phantoms_t_term_definition name term)
