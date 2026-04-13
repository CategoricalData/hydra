(defpackage :hydra.context
(:use :cl :hydra.core)
(:export :make-hydra_context_context :hydra_context_context? :hydra_context_context-trace :hydra_context_context-messages :hydra_context_context-other :make-hydra_context_in_context :hydra_context_in_context? :hydra_context_in_context-object :hydra_context_in_context-context))

(in-package :hydra.context)

(cl:defstruct hydra_context_context trace messages other)

(cl:defstruct hydra_context_in_context object context)
