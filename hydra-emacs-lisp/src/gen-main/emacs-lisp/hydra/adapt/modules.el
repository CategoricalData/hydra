(require 'cl-lib)

(require 'hydra.adapt.terms)

(require 'hydra.adapt.utils)

(require 'hydra.annotations)

(require 'hydra.coders)

(require 'hydra.compute)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.error)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.rewriting)

(require 'hydra.schemas)

(defvar hydra_adapt_modules_language_adapter (lambda (lang) (lambda (_cx) (lambda (g) (lambda (typ) (let ((cx0 (make-hydra_coders_adapter_context g lang hydra_lib_maps_empty))) ((hydra_adapt_terms_term_adapter cx0) typ)))))))

(defvar hydra_adapt_modules_adapt_type_to_language (lambda (lang) (lambda (cx) (lambda (g) (lambda (typ) ((hydra_lib_eithers_map (lambda (v) (hydra_compute_adapter-target v))) ((((hydra_adapt_modules_language_adapter lang) cx) g) typ)))))))

(defvar hydra_adapt_modules_adapt_type_to_language_and_encode (lambda (lang) (lambda (enc) (lambda (cx) (lambda (g) (lambda (typ) (let ((dflt ((hydra_lib_eithers_bind ((((hydra_adapt_modules_adapt_type_to_language lang) cx) g) typ)) (lambda (adapted_type) (enc adapted_type))))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :variable) ((lambda (_) (enc typ)) match_value)) (t dflt))) (cadr match_target))) (hydra_rewriting_deannotate_type typ)))))))))

(defvar hydra_adapt_modules_adapted_module_definitions (lambda (lang) (lambda (cx) (lambda (graph) (lambda (mod) (let ((els ((lambda (v) (hydra_module_module-elements v)) mod))) (let ((adapters_for (lambda (types) ((hydra_lib_eithers_map (lambda (adapters) (hydra_lib_maps_from_list ((hydra_lib_lists_zip types) adapters)))) ((hydra_lib_eithers_map_list (((hydra_adapt_modules_language_adapter lang) cx) graph)) types))))) (let ((classify (lambda (adapters) (lambda (pair) (let ((el (hydra_lib_pairs_first pair))) (let ((tt (hydra_lib_pairs_second pair))) (let ((term ((lambda (v) (hydra_core_type_application_term-body v)) tt))) (let ((typ ((lambda (v) (hydra_core_type_application_term-type v)) tt))) (let ((name ((lambda (v) (hydra_core_binding-name v)) el))) (if (hydra_annotations_is_native_type el) ((hydra_lib_eithers_bind (((hydra_lib_eithers_bimap (lambda (e) ((lambda (v) v) e))) (lambda (x) x)) ((hydra_decode_core_type graph) term))) (lambda (core_typ) ((hydra_lib_eithers_bind ((((hydra_adapt_modules_adapt_type_to_language lang) cx) graph) core_typ)) (lambda (adapted_typ) (list :right (list :type (make-hydra_module_type_definition name adapted_typ))))))) (((hydra_lib_maybes_maybe (list :left ((hydra_lib_strings_cat2 "no adapter for element ") ((lambda (v) v) name)))) (lambda (adapter) ((hydra_lib_eithers_bind (((hydra_lib_eithers_bimap (lambda (ic) ((lambda (v) v) ((lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (x) x)) ((((lambda (v) (hydra_compute_coder-encode v)) ((lambda (v) (hydra_compute_adapter-coder v)) adapter)) cx) term))) (lambda (adapted) (list :right (list :term (make-hydra_module_term_definition name adapted (hydra_schemas_type_to_type_scheme ((lambda (v) (hydra_compute_adapter-target v)) adapter))))))))) ((hydra_lib_maps_lookup typ) adapters)))))))))))) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list (lambda (_el) (((hydra_lib_eithers_bimap (lambda (ic) ((lambda (v) v) ((lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (x) x)) ((hydra_schemas_element_as_type_application_term cx) _el)))) els)) (lambda (tterms) (let ((types (hydra_lib_sets_to_list (hydra_lib_sets_from_list ((hydra_lib_lists_map (lambda (arg_) (hydra_rewriting_deannotate_type ((lambda (v) (hydra_core_type_application_term-type v)) arg_)))) tterms))))) ((hydra_lib_eithers_bind (adapters_for types)) (lambda (adapters) ((hydra_lib_eithers_map_list (classify adapters)) ((hydra_lib_lists_zip els) tterms)))))))))))))))

(defvar hydra_adapt_modules_construct_coder (lambda (lang) (lambda (encode_term) (lambda (cx) (lambda (g) (lambda (typ) ((hydra_lib_eithers_map (lambda (adapter) ((hydra_adapt_utils_compose_coders ((lambda (v) (hydra_compute_adapter-coder v)) adapter)) (hydra_adapt_utils_unidirectional_coder encode_term)))) ((((hydra_adapt_modules_language_adapter lang) cx) g) typ))))))))

(defvar hydra_adapt_modules_transform_module (lambda (lang) (lambda (encode_term) (lambda (create_module) (lambda (cx) (lambda (g) (lambda (mod) (let ((els ((lambda (v) (hydra_module_module-elements v)) mod))) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list (lambda (_el) (((hydra_lib_eithers_bimap (lambda (ic) ((lambda (v) v) ((lambda (v) (hydra_context_in_context-object v)) ic)))) (lambda (x) x)) ((hydra_schemas_element_as_type_application_term cx) _el)))) els)) (lambda (tterms) (let ((types (hydra_lib_lists_nub ((hydra_lib_lists_map (lambda (v) (hydra_core_type_application_term-type v))) tterms)))) ((hydra_lib_eithers_bind ((hydra_lib_eithers_map_list ((((hydra_adapt_modules_construct_coder lang) encode_term) cx) g)) types)) (lambda (cdrs) (let ((coders (hydra_lib_maps_from_list ((hydra_lib_lists_zip types) cdrs)))) (((create_module mod) coders) ((hydra_lib_lists_zip els) tterms))))))))))))))))

(provide 'hydra.adapt.modules)
