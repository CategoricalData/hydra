(ns hydra.ext.haskell.environment)

(defrecord hydra_ext_haskell_environment_haskell_module_metadata [uses_byte_string uses_int uses_map uses_set])
(defn make-hydra_ext_haskell_environment_haskell_module_metadata [uses_byte_string uses_int uses_map uses_set] (->hydra_ext_haskell_environment_haskell_module_metadata uses_byte_string uses_int uses_map uses_set))
