(ns hydra.variants)

(declare hydra_variants_elimination_variant-variants hydra_variants_function_variant-variants hydra_variants_literal_variant-variants hydra_variants_term_variant-variants hydra_variants_type_variant-variants)

(def hydra_variants_elimination_variant-variants (list :record :union :wrap))

(def hydra_variants_function_variant-variants (list :elimination :lambda))

(def hydra_variants_literal_variant-variants (list :binary :boolean :float :integer :string))

(def hydra_variants_term_variant-variants (list :annotated :application :cases :either :lambda :let :list :literal :map :maybe :pair :project :record :set :type_application :type_lambda :union :unit :unwrap :variable :wrap))

(def hydra_variants_type_variant-variants (list :annotated :application :either :forall :function :list :literal :map :maybe :pair :record :set :union :unit :variable :void :wrap))
