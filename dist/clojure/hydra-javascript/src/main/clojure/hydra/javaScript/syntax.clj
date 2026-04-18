(ns hydra.javaScript.syntax)

(declare hydra_java_script_syntax_literal-variants hydra_java_script_syntax_numeric_literal-variants hydra_java_script_syntax_type_expression-variants hydra_java_script_syntax_expression-variants hydra_java_script_syntax_arrow_function_body-variants hydra_java_script_syntax_property_kind-variants hydra_java_script_syntax_array_element-variants hydra_java_script_syntax_pattern-variants hydra_java_script_syntax_object_pattern_property-variants hydra_java_script_syntax_statement-variants hydra_java_script_syntax_variable_kind-variants hydra_java_script_syntax_for_init-variants hydra_java_script_syntax_for_in_left-variants hydra_java_script_syntax_method_kind-variants hydra_java_script_syntax_source_type-variants hydra_java_script_syntax_module_item-variants hydra_java_script_syntax_import_clause-variants hydra_java_script_syntax_export_declaration-variants hydra_java_script_syntax_binary_operator-variants hydra_java_script_syntax_unary_operator-variants hydra_java_script_syntax_assignment_operator-variants hydra_java_script_syntax_comment-variants)

(defrecord hydra_java_script_syntax_identifier [value])
(defn make-hydra_java_script_syntax_identifier [value] (->hydra_java_script_syntax_identifier value))

(def hydra_java_script_syntax_literal-variants (list :string :number :boolean :null :undefined :big_int :template))

(defrecord hydra_java_script_syntax_string_literal [value single_quote])
(defn make-hydra_java_script_syntax_string_literal [value single_quote] (->hydra_java_script_syntax_string_literal value single_quote))

(defrecord hydra_java_script_syntax_template_literal [quasis expressions])
(defn make-hydra_java_script_syntax_template_literal [quasis expressions] (->hydra_java_script_syntax_template_literal quasis expressions))

(defrecord hydra_java_script_syntax_template_element [value tail])
(defn make-hydra_java_script_syntax_template_element [value tail] (->hydra_java_script_syntax_template_element value tail))

(def hydra_java_script_syntax_numeric_literal-variants (list :integer :float))

(defrecord hydra_java_script_syntax_type_annotation [value])
(defn make-hydra_java_script_syntax_type_annotation [value] (->hydra_java_script_syntax_type_annotation value))

(def hydra_java_script_syntax_type_expression-variants (list :identifier :literal :array :function :object :union :parameterized :optional :any :void :never))

(defrecord hydra_java_script_syntax_function_type_expression [type_parameters parameters return_type])
(defn make-hydra_java_script_syntax_function_type_expression [type_parameters parameters return_type] (->hydra_java_script_syntax_function_type_expression type_parameters parameters return_type))

(defrecord hydra_java_script_syntax_array_type_expression [value])
(defn make-hydra_java_script_syntax_array_type_expression [value] (->hydra_java_script_syntax_array_type_expression value))

(defrecord hydra_java_script_syntax_parameterized_type_expression [base arguments])
(defn make-hydra_java_script_syntax_parameterized_type_expression [base arguments] (->hydra_java_script_syntax_parameterized_type_expression base arguments))

(defrecord hydra_java_script_syntax_property_signature [name type optional readonly])
(defn make-hydra_java_script_syntax_property_signature [name type optional readonly] (->hydra_java_script_syntax_property_signature name type optional readonly))

(defrecord hydra_java_script_syntax_type_parameter [name constraint default])
(defn make-hydra_java_script_syntax_type_parameter [name constraint default] (->hydra_java_script_syntax_type_parameter name constraint default))

(def hydra_java_script_syntax_expression-variants (list :identifier :literal :array :object :function :arrow :call :member :conditional :binary :unary :assignment :sequence :this :new :yield :await :spread :parenthesized))

(defrecord hydra_java_script_syntax_function_expression [id params body async generator])
(defn make-hydra_java_script_syntax_function_expression [id params body async generator] (->hydra_java_script_syntax_function_expression id params body async generator))

(defrecord hydra_java_script_syntax_arrow_function_expression [params body async])
(defn make-hydra_java_script_syntax_arrow_function_expression [params body async] (->hydra_java_script_syntax_arrow_function_expression params body async))

(def hydra_java_script_syntax_arrow_function_body-variants (list :expression :block))

(defrecord hydra_java_script_syntax_call_expression [callee arguments optional])
(defn make-hydra_java_script_syntax_call_expression [callee arguments optional] (->hydra_java_script_syntax_call_expression callee arguments optional))

(defrecord hydra_java_script_syntax_member_expression [object property computed optional])
(defn make-hydra_java_script_syntax_member_expression [object property computed optional] (->hydra_java_script_syntax_member_expression object property computed optional))

(defrecord hydra_java_script_syntax_conditional_expression [test consequent alternate])
(defn make-hydra_java_script_syntax_conditional_expression [test consequent alternate] (->hydra_java_script_syntax_conditional_expression test consequent alternate))

(defrecord hydra_java_script_syntax_binary_expression [operator left right])
(defn make-hydra_java_script_syntax_binary_expression [operator left right] (->hydra_java_script_syntax_binary_expression operator left right))

(defrecord hydra_java_script_syntax_unary_expression [operator argument prefix])
(defn make-hydra_java_script_syntax_unary_expression [operator argument prefix] (->hydra_java_script_syntax_unary_expression operator argument prefix))

(defrecord hydra_java_script_syntax_assignment_expression [operator left right])
(defn make-hydra_java_script_syntax_assignment_expression [operator left right] (->hydra_java_script_syntax_assignment_expression operator left right))

(defrecord hydra_java_script_syntax_spread_element [value])
(defn make-hydra_java_script_syntax_spread_element [value] (->hydra_java_script_syntax_spread_element value))

(defrecord hydra_java_script_syntax_property [key value kind computed shorthand])
(defn make-hydra_java_script_syntax_property [key value kind computed shorthand] (->hydra_java_script_syntax_property key value kind computed shorthand))

(def hydra_java_script_syntax_property_kind-variants (list :init :get :set))

(def hydra_java_script_syntax_array_element-variants (list :expression :spread :hole))

(def hydra_java_script_syntax_pattern-variants (list :identifier :object :array :assignment :rest))

(defrecord hydra_java_script_syntax_object_pattern [properties])
(defn make-hydra_java_script_syntax_object_pattern [properties] (->hydra_java_script_syntax_object_pattern properties))

(def hydra_java_script_syntax_object_pattern_property-variants (list :property :rest))

(defrecord hydra_java_script_syntax_assignment_pattern [left right])
(defn make-hydra_java_script_syntax_assignment_pattern [left right] (->hydra_java_script_syntax_assignment_pattern left right))

(defrecord hydra_java_script_syntax_rest_element [value])
(defn make-hydra_java_script_syntax_rest_element [value] (->hydra_java_script_syntax_rest_element value))

(def hydra_java_script_syntax_statement-variants (list :expression :block :empty :debugger :return :break :continue :if :switch :throw :try :while :do_while :for :for_in :for_of :variable_declaration :function_declaration :class_declaration :labeled))

(defrecord hydra_java_script_syntax_labeled_statement [label body])
(defn make-hydra_java_script_syntax_labeled_statement [label body] (->hydra_java_script_syntax_labeled_statement label body))

(defrecord hydra_java_script_syntax_variable_declaration [kind declarations])
(defn make-hydra_java_script_syntax_variable_declaration [kind declarations] (->hydra_java_script_syntax_variable_declaration kind declarations))

(defrecord hydra_java_script_syntax_variable_declarator [id init])
(defn make-hydra_java_script_syntax_variable_declarator [id init] (->hydra_java_script_syntax_variable_declarator id init))

(def hydra_java_script_syntax_variable_kind-variants (list :var :let :const))

(defrecord hydra_java_script_syntax_if_statement [test consequent alternate])
(defn make-hydra_java_script_syntax_if_statement [test consequent alternate] (->hydra_java_script_syntax_if_statement test consequent alternate))

(defrecord hydra_java_script_syntax_switch_statement [discriminant cases])
(defn make-hydra_java_script_syntax_switch_statement [discriminant cases] (->hydra_java_script_syntax_switch_statement discriminant cases))

(defrecord hydra_java_script_syntax_switch_case [test consequent])
(defn make-hydra_java_script_syntax_switch_case [test consequent] (->hydra_java_script_syntax_switch_case test consequent))

(defrecord hydra_java_script_syntax_for_statement [init test update body])
(defn make-hydra_java_script_syntax_for_statement [init test update body] (->hydra_java_script_syntax_for_statement init test update body))

(def hydra_java_script_syntax_for_init-variants (list :variable :expression))

(defrecord hydra_java_script_syntax_for_in_statement [left right body])
(defn make-hydra_java_script_syntax_for_in_statement [left right body] (->hydra_java_script_syntax_for_in_statement left right body))

(def hydra_java_script_syntax_for_in_left-variants (list :variable :pattern))

(defrecord hydra_java_script_syntax_for_of_statement [await left right body])
(defn make-hydra_java_script_syntax_for_of_statement [await left right body] (->hydra_java_script_syntax_for_of_statement await left right body))

(defrecord hydra_java_script_syntax_while_statement [test body])
(defn make-hydra_java_script_syntax_while_statement [test body] (->hydra_java_script_syntax_while_statement test body))

(defrecord hydra_java_script_syntax_do_while_statement [body test])
(defn make-hydra_java_script_syntax_do_while_statement [body test] (->hydra_java_script_syntax_do_while_statement body test))

(defrecord hydra_java_script_syntax_try_statement [block handler finalizer])
(defn make-hydra_java_script_syntax_try_statement [block handler finalizer] (->hydra_java_script_syntax_try_statement block handler finalizer))

(defrecord hydra_java_script_syntax_catch_clause [param body])
(defn make-hydra_java_script_syntax_catch_clause [param body] (->hydra_java_script_syntax_catch_clause param body))

(defrecord hydra_java_script_syntax_throw_statement [value])
(defn make-hydra_java_script_syntax_throw_statement [value] (->hydra_java_script_syntax_throw_statement value))

(defrecord hydra_java_script_syntax_function_declaration [id params body async generator])
(defn make-hydra_java_script_syntax_function_declaration [id params body async generator] (->hydra_java_script_syntax_function_declaration id params body async generator))

(defrecord hydra_java_script_syntax_class_declaration [id super_class body])
(defn make-hydra_java_script_syntax_class_declaration [id super_class body] (->hydra_java_script_syntax_class_declaration id super_class body))

(defrecord hydra_java_script_syntax_method_definition [key value kind computed static])
(defn make-hydra_java_script_syntax_method_definition [key value kind computed static] (->hydra_java_script_syntax_method_definition key value kind computed static))

(def hydra_java_script_syntax_method_kind-variants (list :constructor :method :get :set))

(defrecord hydra_java_script_syntax_program [body source_type])
(defn make-hydra_java_script_syntax_program [body source_type] (->hydra_java_script_syntax_program body source_type))

(def hydra_java_script_syntax_source_type-variants (list :module :script))

(def hydra_java_script_syntax_module_item-variants (list :statement :import :export))

(defrecord hydra_java_script_syntax_import_declaration [specifiers source])
(defn make-hydra_java_script_syntax_import_declaration [specifiers source] (->hydra_java_script_syntax_import_declaration specifiers source))

(def hydra_java_script_syntax_import_clause-variants (list :named :default :namespace))

(defrecord hydra_java_script_syntax_import_specifier [imported local])
(defn make-hydra_java_script_syntax_import_specifier [imported local] (->hydra_java_script_syntax_import_specifier imported local))

(defrecord hydra_java_script_syntax_import_default_specifier [value])
(defn make-hydra_java_script_syntax_import_default_specifier [value] (->hydra_java_script_syntax_import_default_specifier value))

(defrecord hydra_java_script_syntax_import_namespace_specifier [value])
(defn make-hydra_java_script_syntax_import_namespace_specifier [value] (->hydra_java_script_syntax_import_namespace_specifier value))

(def hydra_java_script_syntax_export_declaration-variants (list :named :default :declaration :all))

(defrecord hydra_java_script_syntax_named_export [specifiers source])
(defn make-hydra_java_script_syntax_named_export [specifiers source] (->hydra_java_script_syntax_named_export specifiers source))

(defrecord hydra_java_script_syntax_export_all_declaration [exported source])
(defn make-hydra_java_script_syntax_export_all_declaration [exported source] (->hydra_java_script_syntax_export_all_declaration exported source))

(defrecord hydra_java_script_syntax_export_specifier [local exported])
(defn make-hydra_java_script_syntax_export_specifier [local exported] (->hydra_java_script_syntax_export_specifier local exported))

(def hydra_java_script_syntax_binary_operator-variants (list :add :subtract :multiply :divide :modulo :exponentiate :equal :not_equal :strict_equal :strict_not_equal :less_than :less_than_or_equal :greater_than :greater_than_or_equal :and :or :nullish_coalescing :bitwise_and :bitwise_or :bitwise_xor :left_shift :right_shift :unsigned_right_shift :in :instanceof))

(def hydra_java_script_syntax_unary_operator-variants (list :negate :plus :not :bitwise_not :typeof :void :delete :increment :decrement))

(def hydra_java_script_syntax_assignment_operator-variants (list :assign :add_assign :subtract_assign :multiply_assign :divide_assign :modulo_assign :exponentiate_assign :left_shift_assign :right_shift_assign :unsigned_right_shift_assign :bitwise_and_assign :bitwise_or_assign :bitwise_xor_assign :and_assign :or_assign :nullish_assign))

(def hydra_java_script_syntax_comment-variants (list :line :block :documentation))

(defrecord hydra_java_script_syntax_documentation_comment [description tags])
(defn make-hydra_java_script_syntax_documentation_comment [description tags] (->hydra_java_script_syntax_documentation_comment description tags))

(defrecord hydra_java_script_syntax_documentation_tag [name type param_name description])
(defn make-hydra_java_script_syntax_documentation_tag [name type param_name description] (->hydra_java_script_syntax_documentation_tag name type param_name description))

(defrecord hydra_java_script_syntax_module_item_with_comments [body comments])
(defn make-hydra_java_script_syntax_module_item_with_comments [body comments] (->hydra_java_script_syntax_module_item_with_comments body comments))

(defrecord hydra_java_script_syntax_statement_with_comments [body comments])
(defn make-hydra_java_script_syntax_statement_with_comments [body comments] (->hydra_java_script_syntax_statement_with_comments body comments))

(defrecord hydra_java_script_syntax_function_declaration_with_comments [body comments])
(defn make-hydra_java_script_syntax_function_declaration_with_comments [body comments] (->hydra_java_script_syntax_function_declaration_with_comments body comments))

(defrecord hydra_java_script_syntax_class_declaration_with_comments [body comments])
(defn make-hydra_java_script_syntax_class_declaration_with_comments [body comments] (->hydra_java_script_syntax_class_declaration_with_comments body comments))
