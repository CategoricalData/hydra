(defpackage :hydra.variants
(:use :cl)
(:export :hydra_variants_elimination_variant-variants :hydra_variants_function_variant-variants :hydra_variants_literal_variant-variants :hydra_variants_term_variant-variants :hydra_variants_type_variant-variants))

(in-package :hydra.variants)

(cl:defvar hydra_variants_elimination_variant-variants (cl:list :record :union :wrap))

(cl:defvar hydra_variants_function_variant-variants (cl:list :elimination :lambda :primitive))

(cl:defvar hydra_variants_literal_variant-variants (cl:list :binary :boolean :float :integer :string))

(cl:defvar hydra_variants_term_variant-variants (cl:list :annotated :application :either :function :let :list :literal :map :maybe :pair :record :set :type_application :type_lambda :union :unit :variable :wrap))

(cl:defvar hydra_variants_type_variant-variants (cl:list :annotated :application :either :forall :function :list :literal :map :maybe :pair :record :set :union :unit :variable :wrap))
