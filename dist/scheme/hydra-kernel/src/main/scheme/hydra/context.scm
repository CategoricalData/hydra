(define-library (hydra context)
(export make-hydra_context_context hydra_context_context? hydra_context_context-trace hydra_context_context-messages hydra_context_context-other make-hydra_context_in_context hydra_context_in_context? hydra_context_in_context-object hydra_context_in_context-context)
(import (scheme base) (hydra core))
(begin
(define-record-type hydra_context_context (make-hydra_context_context trace messages other) hydra_context_context? (trace hydra_context_context-trace) (messages hydra_context_context-messages) (other hydra_context_context-other))
(define-record-type hydra_context_in_context (make-hydra_context_in_context object context) hydra_context_in_context? (object hydra_context_in_context-object) (context hydra_context_in_context-context))))
