(require 'cl-lib)

(defvar hydra_variants_elimination_variant-variants (list :record :union :wrap))

(defvar hydra_variants_function_variant-variants (list :elimination :lambda))

(defvar hydra_variants_literal_variant-variants (list :binary :boolean :float :integer :string))

(defvar hydra_variants_term_variant-variants (list :annotated :application :cases :either :inject :lambda :let :list :literal :map :maybe :pair :project :record :set :type_application :type_lambda :unit :unwrap :variable :wrap))

(defvar hydra_variants_type_variant-variants (list :annotated :application :either :forall :function :list :literal :map :maybe :pair :record :set :union :unit :variable :void :wrap))

(provide 'hydra.variants)
