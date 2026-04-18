(ns hydra.dsl.context
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_context_context hydra_dsl_context_context_messages hydra_dsl_context_context_other hydra_dsl_context_context_trace hydra_dsl_context_context_with_messages hydra_dsl_context_context_with_other hydra_dsl_context_context_with_trace hydra_dsl_context_in_context hydra_dsl_context_in_context_context hydra_dsl_context_in_context_object hydra_dsl_context_in_context_with_context hydra_dsl_context_in_context_with_object)

(def hydra_dsl_context_context (fn [trace] (fn [messages] (fn [other] (list :record (->hydra_core_record "hydra.context.Context" (list (->hydra_core_field "trace" ((fn [v] v) trace)) (->hydra_core_field "messages" ((fn [v] v) messages)) (->hydra_core_field "other" ((fn [v] v) other)))))))))

(def hydra_dsl_context_context_messages (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "messages")) ((fn [v] v) x)))))

(def hydra_dsl_context_context_other (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "other")) ((fn [v] v) x)))))

(def hydra_dsl_context_context_trace (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "trace")) ((fn [v] v) x)))))

(def hydra_dsl_context_context_with_messages (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.context.Context" (list (->hydra_core_field "trace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "trace")) ((fn [v] v) original)))) (->hydra_core_field "messages" ((fn [v] v) new_val)) (->hydra_core_field "other" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "other")) ((fn [v] v) original))))))))))

(def hydra_dsl_context_context_with_other (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.context.Context" (list (->hydra_core_field "trace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "trace")) ((fn [v] v) original)))) (->hydra_core_field "messages" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "messages")) ((fn [v] v) original)))) (->hydra_core_field "other" ((fn [v] v) new_val))))))))

(def hydra_dsl_context_context_with_trace (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.context.Context" (list (->hydra_core_field "trace" ((fn [v] v) new_val)) (->hydra_core_field "messages" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "messages")) ((fn [v] v) original)))) (->hydra_core_field "other" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.Context" "other")) ((fn [v] v) original))))))))))

(def hydra_dsl_context_in_context (fn [object] (fn [context] (list :record (->hydra_core_record "hydra.context.InContext" (list (->hydra_core_field "object" ((fn [v] v) object)) (->hydra_core_field "context" ((fn [v] v) context))))))))

(def hydra_dsl_context_in_context_context (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.InContext" "context")) ((fn [v] v) x)))))

(def hydra_dsl_context_in_context_object (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.InContext" "object")) ((fn [v] v) x)))))

(def hydra_dsl_context_in_context_with_context (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.context.InContext" (list (->hydra_core_field "object" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.InContext" "object")) ((fn [v] v) original)))) (->hydra_core_field "context" ((fn [v] v) new_val))))))))

(def hydra_dsl_context_in_context_with_object (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.context.InContext" (list (->hydra_core_field "object" ((fn [v] v) new_val)) (->hydra_core_field "context" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.context.InContext" "context")) ((fn [v] v) original))))))))))
