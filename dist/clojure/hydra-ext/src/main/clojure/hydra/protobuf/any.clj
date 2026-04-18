(ns hydra.protobuf.any)

(defrecord hydra_protobuf_any_any [type_url value])
(defn make-hydra_protobuf_any_any [type_url value] (->hydra_protobuf_any_any type_url value))
