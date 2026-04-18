(ns hydra.kusto.kql)

(declare hydra_kusto_kql_binary_operator-variants hydra_kusto_kql_built_in_function-variants hydra_kusto_kql_columns-variants hydra_kusto_kql_command-variants hydra_kusto_kql_duration_unit-variants hydra_kusto_kql_expression-variants hydra_kusto_kql_function-variants hydra_kusto_kql_join_kind-variants hydra_kusto_kql_literal-variants hydra_kusto_kql_order-variants hydra_kusto_kql_tabular_expression-variants hydra_kusto_kql_unary_operator-variants hydra_kusto_kql_union_kind-variants)

(defrecord hydra_kusto_kql_between_expression [not expression lower_bound upper_bound])
(defn make-hydra_kusto_kql_between_expression [not expression lower_bound upper_bound] (->hydra_kusto_kql_between_expression not expression lower_bound upper_bound))

(defrecord hydra_kusto_kql_binary_expression [left operator right])
(defn make-hydra_kusto_kql_binary_expression [left operator right] (->hydra_kusto_kql_binary_expression left operator right))

(def hydra_kusto_kql_binary_operator-variants (list :case_insensitive_equal :contains :divide :ends_with :equal :greater :greater_or_equal :has :has_prefix :has_suffix :less :less_or_equal :matches_regex :minus :not_equal :plus :starts_with :times))

(def hydra_kusto_kql_built_in_function-variants (list :ago :bin :count :dcount :endofday :extract :format_datetime :materialize :now :range :startofday :strcat :todynamic))

(defrecord hydra_kusto_kql_column_alias [column alias])
(defn make-hydra_kusto_kql_column_alias [column alias] (->hydra_kusto_kql_column_alias column alias))

(defrecord hydra_kusto_kql_column_assignment [column expression])
(defn make-hydra_kusto_kql_column_assignment [column expression] (->hydra_kusto_kql_column_assignment column expression))

(defrecord hydra_kusto_kql_column_name [value])
(defn make-hydra_kusto_kql_column_name [value] (->hydra_kusto_kql_column_name value))

(def hydra_kusto_kql_columns-variants (list :all :single))

(def hydra_kusto_kql_command-variants (list :count :distinct :extend :join :limit :mvexpand :order_by :parse :print :project :project_away :project_rename :render :search :sort_by :summarize :take :top :union :where))

(defrecord hydra_kusto_kql_datetime [value])
(defn make-hydra_kusto_kql_datetime [value] (->hydra_kusto_kql_datetime value))

(defrecord hydra_kusto_kql_duration [value unit])
(defn make-hydra_kusto_kql_duration [value unit] (->hydra_kusto_kql_duration value unit))

(def hydra_kusto_kql_duration_unit-variants (list :second :minute :hour))

(def hydra_kusto_kql_expression-variants (list :and :any :between :binary :braces :column :dataset :index :list :literal :or :parentheses :property :unary))

(def hydra_kusto_kql_function-variants (list :built_in :custom))

(defrecord hydra_kusto_kql_function_expression [function arguments])
(defn make-hydra_kusto_kql_function_expression [function arguments] (->hydra_kusto_kql_function_expression function arguments))

(defrecord hydra_kusto_kql_function_name [value])
(defn make-hydra_kusto_kql_function_name [value] (->hydra_kusto_kql_function_name value))

(defrecord hydra_kusto_kql_index_expression [expression index])
(defn make-hydra_kusto_kql_index_expression [expression index] (->hydra_kusto_kql_index_expression expression index))

(defrecord hydra_kusto_kql_join_command [kind expression on])
(defn make-hydra_kusto_kql_join_command [kind expression on] (->hydra_kusto_kql_join_command kind expression on))

(def hydra_kusto_kql_join_kind-variants (list :leftouter :leftsemi :leftanti :fullouter :inner :innerunique :rightouter :rightsemi :rightanti))

(defrecord hydra_kusto_kql_key_value_pair [key value])
(defn make-hydra_kusto_kql_key_value_pair [key value] (->hydra_kusto_kql_key_value_pair key value))

(defrecord hydra_kusto_kql_let_binding [name expression])
(defn make-hydra_kusto_kql_let_binding [name expression] (->hydra_kusto_kql_let_binding name expression))

(defrecord hydra_kusto_kql_let_expression [bindings expression])
(defn make-hydra_kusto_kql_let_expression [bindings expression] (->hydra_kusto_kql_let_expression bindings expression))

(def hydra_kusto_kql_literal-variants (list :duration :datetime :string :int :long :double :boolean))

(def hydra_kusto_kql_order-variants (list :ascending :descending))

(defrecord hydra_kusto_kql_parameter [key value])
(defn make-hydra_kusto_kql_parameter [key value] (->hydra_kusto_kql_parameter key value))

(defrecord hydra_kusto_kql_parse_command [column pairs])
(defn make-hydra_kusto_kql_parse_command [column pairs] (->hydra_kusto_kql_parse_command column pairs))

(defrecord hydra_kusto_kql_pipeline_expression [value])
(defn make-hydra_kusto_kql_pipeline_expression [value] (->hydra_kusto_kql_pipeline_expression value))

(defrecord hydra_kusto_kql_print_command [column expression])
(defn make-hydra_kusto_kql_print_command [column expression] (->hydra_kusto_kql_print_command column expression))

(defrecord hydra_kusto_kql_projection [expression alias])
(defn make-hydra_kusto_kql_projection [expression alias] (->hydra_kusto_kql_projection expression alias))

(defrecord hydra_kusto_kql_property_expression [expression property])
(defn make-hydra_kusto_kql_property_expression [expression property] (->hydra_kusto_kql_property_expression expression property))

(defrecord hydra_kusto_kql_query [value])
(defn make-hydra_kusto_kql_query [value] (->hydra_kusto_kql_query value))

(defrecord hydra_kusto_kql_search_command [datasets pattern])
(defn make-hydra_kusto_kql_search_command [datasets pattern] (->hydra_kusto_kql_search_command datasets pattern))

(defrecord hydra_kusto_kql_summarize_command [columns by])
(defn make-hydra_kusto_kql_summarize_command [columns by] (->hydra_kusto_kql_summarize_command columns by))

(defrecord hydra_kusto_kql_table_name [value])
(defn make-hydra_kusto_kql_table_name [value] (->hydra_kusto_kql_table_name value))

(defrecord hydra_kusto_kql_top_command [count sort])
(defn make-hydra_kusto_kql_top_command [count sort] (->hydra_kusto_kql_top_command count sort))

(defrecord hydra_kusto_kql_sort_by [column order])
(defn make-hydra_kusto_kql_sort_by [column order] (->hydra_kusto_kql_sort_by column order))

(def hydra_kusto_kql_tabular_expression-variants (list :command :pipeline :let :table))

(defrecord hydra_kusto_kql_unary_expression [operator expression])
(defn make-hydra_kusto_kql_unary_expression [operator expression] (->hydra_kusto_kql_unary_expression operator expression))

(def hydra_kusto_kql_unary_operator-variants (list :not))

(defrecord hydra_kusto_kql_union_command [parameters kind with_source is_fuzzy tables])
(defn make-hydra_kusto_kql_union_command [parameters kind with_source is_fuzzy tables] (->hydra_kusto_kql_union_command parameters kind with_source is_fuzzy tables))

(def hydra_kusto_kql_union_kind-variants (list :inner :outer))
