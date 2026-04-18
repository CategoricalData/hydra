(ns hydra.dsl.packaging
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_packaging_definition_term hydra_dsl_packaging_definition_type hydra_dsl_packaging_file_extension hydra_dsl_packaging_library hydra_dsl_packaging_library_namespace hydra_dsl_packaging_library_prefix hydra_dsl_packaging_library_primitives hydra_dsl_packaging_library_with_namespace hydra_dsl_packaging_library_with_prefix hydra_dsl_packaging_library_with_primitives hydra_dsl_packaging_module hydra_dsl_packaging_module_definitions hydra_dsl_packaging_module_description hydra_dsl_packaging_module_namespace hydra_dsl_packaging_module_term_dependencies hydra_dsl_packaging_module_type_dependencies hydra_dsl_packaging_module_with_definitions hydra_dsl_packaging_module_with_description hydra_dsl_packaging_module_with_namespace hydra_dsl_packaging_module_with_term_dependencies hydra_dsl_packaging_module_with_type_dependencies hydra_dsl_packaging_namespace hydra_dsl_packaging_namespaces hydra_dsl_packaging_namespaces_focus hydra_dsl_packaging_namespaces_mapping hydra_dsl_packaging_namespaces_with_focus hydra_dsl_packaging_namespaces_with_mapping hydra_dsl_packaging_package hydra_dsl_packaging_package_dependencies hydra_dsl_packaging_package_description hydra_dsl_packaging_package_modules hydra_dsl_packaging_package_name hydra_dsl_packaging_package_name_ hydra_dsl_packaging_package_with_dependencies hydra_dsl_packaging_package_with_description hydra_dsl_packaging_package_with_modules hydra_dsl_packaging_package_with_name hydra_dsl_packaging_qualified_name hydra_dsl_packaging_qualified_name_local hydra_dsl_packaging_qualified_name_namespace hydra_dsl_packaging_qualified_name_with_local hydra_dsl_packaging_qualified_name_with_namespace hydra_dsl_packaging_term_definition hydra_dsl_packaging_term_definition_name hydra_dsl_packaging_term_definition_term hydra_dsl_packaging_term_definition_type hydra_dsl_packaging_term_definition_with_name hydra_dsl_packaging_term_definition_with_term hydra_dsl_packaging_term_definition_with_type hydra_dsl_packaging_type_definition hydra_dsl_packaging_type_definition_name hydra_dsl_packaging_type_definition_type hydra_dsl_packaging_type_definition_with_name hydra_dsl_packaging_type_definition_with_type hydra_dsl_packaging_un_file_extension hydra_dsl_packaging_un_namespace hydra_dsl_packaging_un_package_name)

(def hydra_dsl_packaging_definition_term (fn [x] (list :inject (->hydra_core_injection "hydra.packaging.Definition" (->hydra_core_field "term" ((fn [v] v) x))))))

(def hydra_dsl_packaging_definition_type (fn [x] (list :inject (->hydra_core_injection "hydra.packaging.Definition" (->hydra_core_field "type" ((fn [v] v) x))))))

(def hydra_dsl_packaging_file_extension (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.packaging.FileExtension" ((fn [v] v) x)))))

(def hydra_dsl_packaging_library (fn [namespace] (fn [prefix] (fn [primitives] (list :record (->hydra_core_record "hydra.packaging.Library" (list (->hydra_core_field "namespace" ((fn [v] v) namespace)) (->hydra_core_field "prefix" ((fn [v] v) prefix)) (->hydra_core_field "primitives" ((fn [v] v) primitives)))))))))

(def hydra_dsl_packaging_library_namespace (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "namespace")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_library_prefix (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "prefix")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_library_primitives (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "primitives")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_library_with_namespace (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Library" (list (->hydra_core_field "namespace" ((fn [v] v) new_val)) (->hydra_core_field "prefix" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "prefix")) ((fn [v] v) original)))) (->hydra_core_field "primitives" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "primitives")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_library_with_prefix (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Library" (list (->hydra_core_field "namespace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "namespace")) ((fn [v] v) original)))) (->hydra_core_field "prefix" ((fn [v] v) new_val)) (->hydra_core_field "primitives" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "primitives")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_library_with_primitives (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Library" (list (->hydra_core_field "namespace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "namespace")) ((fn [v] v) original)))) (->hydra_core_field "prefix" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Library" "prefix")) ((fn [v] v) original)))) (->hydra_core_field "primitives" ((fn [v] v) new_val))))))))

(def hydra_dsl_packaging_module (fn [namespace] (fn [definitions] (fn [term_dependencies] (fn [type_dependencies] (fn [description] (list :record (->hydra_core_record "hydra.packaging.Module" (list (->hydra_core_field "namespace" ((fn [v] v) namespace)) (->hydra_core_field "definitions" ((fn [v] v) definitions)) (->hydra_core_field "termDependencies" ((fn [v] v) term_dependencies)) (->hydra_core_field "typeDependencies" ((fn [v] v) type_dependencies)) (->hydra_core_field "description" ((fn [v] v) description)))))))))))

(def hydra_dsl_packaging_module_definitions (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "definitions")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_module_description (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "description")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_module_namespace (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "namespace")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_module_term_dependencies (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "termDependencies")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_module_type_dependencies (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "typeDependencies")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_module_with_definitions (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Module" (list (->hydra_core_field "namespace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "namespace")) ((fn [v] v) original)))) (->hydra_core_field "definitions" ((fn [v] v) new_val)) (->hydra_core_field "termDependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "termDependencies")) ((fn [v] v) original)))) (->hydra_core_field "typeDependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "typeDependencies")) ((fn [v] v) original)))) (->hydra_core_field "description" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "description")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_module_with_description (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Module" (list (->hydra_core_field "namespace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "namespace")) ((fn [v] v) original)))) (->hydra_core_field "definitions" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "definitions")) ((fn [v] v) original)))) (->hydra_core_field "termDependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "termDependencies")) ((fn [v] v) original)))) (->hydra_core_field "typeDependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "typeDependencies")) ((fn [v] v) original)))) (->hydra_core_field "description" ((fn [v] v) new_val))))))))

(def hydra_dsl_packaging_module_with_namespace (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Module" (list (->hydra_core_field "namespace" ((fn [v] v) new_val)) (->hydra_core_field "definitions" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "definitions")) ((fn [v] v) original)))) (->hydra_core_field "termDependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "termDependencies")) ((fn [v] v) original)))) (->hydra_core_field "typeDependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "typeDependencies")) ((fn [v] v) original)))) (->hydra_core_field "description" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "description")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_module_with_term_dependencies (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Module" (list (->hydra_core_field "namespace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "namespace")) ((fn [v] v) original)))) (->hydra_core_field "definitions" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "definitions")) ((fn [v] v) original)))) (->hydra_core_field "termDependencies" ((fn [v] v) new_val)) (->hydra_core_field "typeDependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "typeDependencies")) ((fn [v] v) original)))) (->hydra_core_field "description" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "description")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_module_with_type_dependencies (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Module" (list (->hydra_core_field "namespace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "namespace")) ((fn [v] v) original)))) (->hydra_core_field "definitions" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "definitions")) ((fn [v] v) original)))) (->hydra_core_field "termDependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "termDependencies")) ((fn [v] v) original)))) (->hydra_core_field "typeDependencies" ((fn [v] v) new_val)) (->hydra_core_field "description" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Module" "description")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_namespace (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.packaging.Namespace" ((fn [v] v) x)))))

(def hydra_dsl_packaging_namespaces (fn [focus] (fn [mapping] (list :record (->hydra_core_record "hydra.packaging.Namespaces" (list (->hydra_core_field "focus" ((fn [v] v) focus)) (->hydra_core_field "mapping" ((fn [v] v) mapping))))))))

(def hydra_dsl_packaging_namespaces_focus (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Namespaces" "focus")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_namespaces_mapping (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Namespaces" "mapping")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_namespaces_with_focus (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Namespaces" (list (->hydra_core_field "focus" ((fn [v] v) new_val)) (->hydra_core_field "mapping" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Namespaces" "mapping")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_namespaces_with_mapping (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Namespaces" (list (->hydra_core_field "focus" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Namespaces" "focus")) ((fn [v] v) original)))) (->hydra_core_field "mapping" ((fn [v] v) new_val))))))))

(def hydra_dsl_packaging_package (fn [name] (fn [modules] (fn [dependencies] (fn [description] (list :record (->hydra_core_record "hydra.packaging.Package" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "modules" ((fn [v] v) modules)) (->hydra_core_field "dependencies" ((fn [v] v) dependencies)) (->hydra_core_field "description" ((fn [v] v) description))))))))))

(def hydra_dsl_packaging_package_dependencies (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "dependencies")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_package_description (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "description")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_package_modules (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "modules")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_package_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "name")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_package_name_ (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.packaging.PackageName" ((fn [v] v) x)))))

(def hydra_dsl_packaging_package_with_dependencies (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Package" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "name")) ((fn [v] v) original)))) (->hydra_core_field "modules" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "modules")) ((fn [v] v) original)))) (->hydra_core_field "dependencies" ((fn [v] v) new_val)) (->hydra_core_field "description" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "description")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_package_with_description (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Package" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "name")) ((fn [v] v) original)))) (->hydra_core_field "modules" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "modules")) ((fn [v] v) original)))) (->hydra_core_field "dependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "dependencies")) ((fn [v] v) original)))) (->hydra_core_field "description" ((fn [v] v) new_val))))))))

(def hydra_dsl_packaging_package_with_modules (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Package" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "name")) ((fn [v] v) original)))) (->hydra_core_field "modules" ((fn [v] v) new_val)) (->hydra_core_field "dependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "dependencies")) ((fn [v] v) original)))) (->hydra_core_field "description" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "description")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_package_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.Package" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "modules" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "modules")) ((fn [v] v) original)))) (->hydra_core_field "dependencies" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "dependencies")) ((fn [v] v) original)))) (->hydra_core_field "description" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.Package" "description")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_qualified_name (fn [namespace] (fn [local] (list :record (->hydra_core_record "hydra.packaging.QualifiedName" (list (->hydra_core_field "namespace" ((fn [v] v) namespace)) (->hydra_core_field "local" ((fn [v] v) local))))))))

(def hydra_dsl_packaging_qualified_name_local (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.QualifiedName" "local")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_qualified_name_namespace (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.QualifiedName" "namespace")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_qualified_name_with_local (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.QualifiedName" (list (->hydra_core_field "namespace" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.QualifiedName" "namespace")) ((fn [v] v) original)))) (->hydra_core_field "local" ((fn [v] v) new_val))))))))

(def hydra_dsl_packaging_qualified_name_with_namespace (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.QualifiedName" (list (->hydra_core_field "namespace" ((fn [v] v) new_val)) (->hydra_core_field "local" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.QualifiedName" "local")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_term_definition (fn [name] (fn [term] (fn [type] (list :record (->hydra_core_record "hydra.packaging.TermDefinition" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "term" ((fn [v] v) term)) (->hydra_core_field "type" ((fn [v] v) type)))))))))

(def hydra_dsl_packaging_term_definition_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "name")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_term_definition_term (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "term")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_term_definition_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "type")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_term_definition_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.TermDefinition" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "term" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "term")) ((fn [v] v) original)))) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_term_definition_with_term (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.TermDefinition" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "name")) ((fn [v] v) original)))) (->hydra_core_field "term" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_term_definition_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.TermDefinition" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "name")) ((fn [v] v) original)))) (->hydra_core_field "term" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TermDefinition" "term")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_packaging_type_definition (fn [name] (fn [type] (list :record (->hydra_core_record "hydra.packaging.TypeDefinition" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_packaging_type_definition_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TypeDefinition" "name")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_type_definition_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TypeDefinition" "type")) ((fn [v] v) x)))))

(def hydra_dsl_packaging_type_definition_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.TypeDefinition" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TypeDefinition" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_packaging_type_definition_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.packaging.TypeDefinition" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.packaging.TypeDefinition" "name")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_packaging_un_file_extension (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.packaging.FileExtension") ((fn [v] v) x)))))

(def hydra_dsl_packaging_un_namespace (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.packaging.Namespace") ((fn [v] v) x)))))

(def hydra_dsl_packaging_un_package_name (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.packaging.PackageName") ((fn [v] v) x)))))
