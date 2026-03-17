(defpackage :hydra.compute
(:use :cl :hydra.context :hydra.error)
(:export :make-hydra_compute_adapter :hydra_compute_adapter? :hydra_compute_adapter-is_lossy :hydra_compute_adapter-source :hydra_compute_adapter-target :hydra_compute_adapter-coder :make-hydra_compute_bicoder :hydra_compute_bicoder? :hydra_compute_bicoder-encode :hydra_compute_bicoder-decode :make-hydra_compute_coder :hydra_compute_coder? :hydra_compute_coder-encode :hydra_compute_coder-decode))

(in-package :hydra.compute)

(cl:defstruct hydra_compute_adapter is_lossy source target coder)

(cl:defstruct hydra_compute_bicoder encode decode)

(cl:defstruct hydra_compute_coder encode decode)
