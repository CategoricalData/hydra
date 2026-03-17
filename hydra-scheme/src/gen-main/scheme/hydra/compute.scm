(define-library (hydra compute)
(export make-hydra_compute_adapter hydra_compute_adapter? hydra_compute_adapter-is_lossy hydra_compute_adapter-source hydra_compute_adapter-target hydra_compute_adapter-coder make-hydra_compute_bicoder hydra_compute_bicoder? hydra_compute_bicoder-encode hydra_compute_bicoder-decode make-hydra_compute_coder hydra_compute_coder? hydra_compute_coder-encode hydra_compute_coder-decode)
(import (scheme base) (hydra context) (hydra error))
(begin
(define-record-type hydra_compute_adapter (make-hydra_compute_adapter is_lossy source target coder) hydra_compute_adapter? (is_lossy hydra_compute_adapter-is_lossy) (source hydra_compute_adapter-source) (target hydra_compute_adapter-target) (coder hydra_compute_adapter-coder))
(define-record-type hydra_compute_bicoder (make-hydra_compute_bicoder encode decode) hydra_compute_bicoder? (encode hydra_compute_bicoder-encode) (decode hydra_compute_bicoder-decode))
(define-record-type hydra_compute_coder (make-hydra_compute_coder encode decode) hydra_compute_coder? (encode hydra_compute_coder-encode) (decode hydra_compute_coder-decode))))
