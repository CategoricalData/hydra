(ns hydra.rust.syntax)

(declare hydra_rust_syntax_item-variants hydra_rust_syntax_use_tree-variants hydra_rust_syntax_struct_body-variants hydra_rust_syntax_enum_variant_body-variants hydra_rust_syntax_self_param-variants hydra_rust_syntax_method_param-variants hydra_rust_syntax_impl_item-variants hydra_rust_syntax_trait_item-variants hydra_rust_syntax_type_param_bound-variants hydra_rust_syntax_type-variants hydra_rust_syntax_generic_arguments-variants hydra_rust_syntax_generic_arg-variants hydra_rust_syntax_expression-variants hydra_rust_syntax_if_condition-variants hydra_rust_syntax_binary_op-variants hydra_rust_syntax_unary_op-variants hydra_rust_syntax_array_expr-variants hydra_rust_syntax_compound_assign_op-variants hydra_rust_syntax_macro_delimiter-variants hydra_rust_syntax_statement-variants hydra_rust_syntax_pattern-variants hydra_rust_syntax_literal-variants hydra_rust_syntax_visibility-variants)

(defrecord hydra_rust_syntax_crate [items])
(defn make-hydra_rust_syntax_crate [items] (->hydra_rust_syntax_crate items))

(def hydra_rust_syntax_item-variants (list :use :struct :enum :fn :type_alias :impl :trait :mod :const :static :macro))

(defrecord hydra_rust_syntax_item_with_comments [doc visibility item])
(defn make-hydra_rust_syntax_item_with_comments [doc visibility item] (->hydra_rust_syntax_item_with_comments doc visibility item))

(defrecord hydra_rust_syntax_use_declaration [public tree])
(defn make-hydra_rust_syntax_use_declaration [public tree] (->hydra_rust_syntax_use_declaration public tree))

(def hydra_rust_syntax_use_tree-variants (list :path :rename :glob :group))

(defrecord hydra_rust_syntax_use_path [segments])
(defn make-hydra_rust_syntax_use_path [segments] (->hydra_rust_syntax_use_path segments))

(defrecord hydra_rust_syntax_use_rename [path alias])
(defn make-hydra_rust_syntax_use_rename [path alias] (->hydra_rust_syntax_use_rename path alias))

(defrecord hydra_rust_syntax_use_group [prefix trees])
(defn make-hydra_rust_syntax_use_group [prefix trees] (->hydra_rust_syntax_use_group prefix trees))

(defrecord hydra_rust_syntax_struct_def [name generics where_clause body derives public doc])
(defn make-hydra_rust_syntax_struct_def [name generics where_clause body derives public doc] (->hydra_rust_syntax_struct_def name generics where_clause body derives public doc))

(def hydra_rust_syntax_struct_body-variants (list :named :tuple :unit))

(defrecord hydra_rust_syntax_struct_field [name type public doc])
(defn make-hydra_rust_syntax_struct_field [name type public doc] (->hydra_rust_syntax_struct_field name type public doc))

(defrecord hydra_rust_syntax_tuple_field [type public])
(defn make-hydra_rust_syntax_tuple_field [type public] (->hydra_rust_syntax_tuple_field type public))

(defrecord hydra_rust_syntax_enum_def [name generics where_clause variants derives public doc])
(defn make-hydra_rust_syntax_enum_def [name generics where_clause variants derives public doc] (->hydra_rust_syntax_enum_def name generics where_clause variants derives public doc))

(defrecord hydra_rust_syntax_enum_variant [name body doc])
(defn make-hydra_rust_syntax_enum_variant [name body doc] (->hydra_rust_syntax_enum_variant name body doc))

(def hydra_rust_syntax_enum_variant_body-variants (list :unit :tuple :struct))

(defrecord hydra_rust_syntax_fn_def [name generics where_clause params return_type body public async const unsafe doc])
(defn make-hydra_rust_syntax_fn_def [name generics where_clause params return_type body public async const unsafe doc] (->hydra_rust_syntax_fn_def name generics where_clause params return_type body public async const unsafe doc))

(defrecord hydra_rust_syntax_fn_param [pattern type])
(defn make-hydra_rust_syntax_fn_param [pattern type] (->hydra_rust_syntax_fn_param pattern type))

(def hydra_rust_syntax_self_param-variants (list :owned :ref :ref_mut))

(def hydra_rust_syntax_method_param-variants (list :self :regular))

(defrecord hydra_rust_syntax_type_alias [name generics type public doc])
(defn make-hydra_rust_syntax_type_alias [name generics type public doc] (->hydra_rust_syntax_type_alias name generics type public doc))

(defrecord hydra_rust_syntax_const_def [name type value public doc])
(defn make-hydra_rust_syntax_const_def [name type value public doc] (->hydra_rust_syntax_const_def name type value public doc))

(defrecord hydra_rust_syntax_static_def [name type value mutable public doc])
(defn make-hydra_rust_syntax_static_def [name type value mutable public doc] (->hydra_rust_syntax_static_def name type value mutable public doc))

(defrecord hydra_rust_syntax_mod_def [name body public doc])
(defn make-hydra_rust_syntax_mod_def [name body public doc] (->hydra_rust_syntax_mod_def name body public doc))

(defrecord hydra_rust_syntax_impl_block [generics where_clause trait negative self_type items])
(defn make-hydra_rust_syntax_impl_block [generics where_clause trait negative self_type items] (->hydra_rust_syntax_impl_block generics where_clause trait negative self_type items))

(def hydra_rust_syntax_impl_item-variants (list :method :type :const))

(defrecord hydra_rust_syntax_impl_method [name generics where_clause params return_type body public default doc])
(defn make-hydra_rust_syntax_impl_method [name generics where_clause params return_type body public default doc] (->hydra_rust_syntax_impl_method name generics where_clause params return_type body public default doc))

(defrecord hydra_rust_syntax_trait_def [name generics where_clause super_traits items public unsafe doc])
(defn make-hydra_rust_syntax_trait_def [name generics where_clause super_traits items public unsafe doc] (->hydra_rust_syntax_trait_def name generics where_clause super_traits items public unsafe doc))

(def hydra_rust_syntax_trait_item-variants (list :method :type :const))

(defrecord hydra_rust_syntax_trait_method [name generics where_clause params return_type default_body doc])
(defn make-hydra_rust_syntax_trait_method [name generics where_clause params return_type default_body doc] (->hydra_rust_syntax_trait_method name generics where_clause params return_type default_body doc))

(defrecord hydra_rust_syntax_trait_type [name bounds default doc])
(defn make-hydra_rust_syntax_trait_type [name bounds default doc] (->hydra_rust_syntax_trait_type name bounds default doc))

(defrecord hydra_rust_syntax_trait_const [name type default doc])
(defn make-hydra_rust_syntax_trait_const [name type default doc] (->hydra_rust_syntax_trait_const name type default doc))

(defrecord hydra_rust_syntax_generic_param [name bounds])
(defn make-hydra_rust_syntax_generic_param [name bounds] (->hydra_rust_syntax_generic_param name bounds))

(def hydra_rust_syntax_type_param_bound-variants (list :trait :lifetime))

(defrecord hydra_rust_syntax_lifetime [name])
(defn make-hydra_rust_syntax_lifetime [name] (->hydra_rust_syntax_lifetime name))

(defrecord hydra_rust_syntax_where_clause [predicates])
(defn make-hydra_rust_syntax_where_clause [predicates] (->hydra_rust_syntax_where_clause predicates))

(defrecord hydra_rust_syntax_where_predicate [type bounds])
(defn make-hydra_rust_syntax_where_predicate [type bounds] (->hydra_rust_syntax_where_predicate type bounds))

(def hydra_rust_syntax_type-variants (list :path :reference :slice :array :tuple :fn_pointer :impl_trait :dyn_trait :inferred :unit :never :raw_pointer :macro))

(defrecord hydra_rust_syntax_type_path [global segments])
(defn make-hydra_rust_syntax_type_path [global segments] (->hydra_rust_syntax_type_path global segments))

(defrecord hydra_rust_syntax_path_segment [name arguments])
(defn make-hydra_rust_syntax_path_segment [name arguments] (->hydra_rust_syntax_path_segment name arguments))

(def hydra_rust_syntax_generic_arguments-variants (list :none :angle_bracketed :parenthesized))

(defrecord hydra_rust_syntax_angle_bracketed_args [args])
(defn make-hydra_rust_syntax_angle_bracketed_args [args] (->hydra_rust_syntax_angle_bracketed_args args))

(def hydra_rust_syntax_generic_arg-variants (list :type :lifetime :const :binding))

(defrecord hydra_rust_syntax_type_binding [name type])
(defn make-hydra_rust_syntax_type_binding [name type] (->hydra_rust_syntax_type_binding name type))

(defrecord hydra_rust_syntax_parenthesized_args [inputs output])
(defn make-hydra_rust_syntax_parenthesized_args [inputs output] (->hydra_rust_syntax_parenthesized_args inputs output))

(defrecord hydra_rust_syntax_reference_type [lifetime mutable type])
(defn make-hydra_rust_syntax_reference_type [lifetime mutable type] (->hydra_rust_syntax_reference_type lifetime mutable type))

(defrecord hydra_rust_syntax_array_type [element length])
(defn make-hydra_rust_syntax_array_type [element length] (->hydra_rust_syntax_array_type element length))

(defrecord hydra_rust_syntax_fn_pointer_type [params return_type])
(defn make-hydra_rust_syntax_fn_pointer_type [params return_type] (->hydra_rust_syntax_fn_pointer_type params return_type))

(defrecord hydra_rust_syntax_raw_pointer_type [mutable type])
(defn make-hydra_rust_syntax_raw_pointer_type [mutable type] (->hydra_rust_syntax_raw_pointer_type mutable type))

(def hydra_rust_syntax_expression-variants (list :literal :path :block :call :method_call :field_access :tuple_index :closure :if :match :loop :while :for :binary :unary :reference :dereference :struct :tuple :array :index :range :return :break :continue :try :cast :type_ascription :await :assign :compound_assign :macro :paren))

(defrecord hydra_rust_syntax_expr_path [global segments])
(defn make-hydra_rust_syntax_expr_path [global segments] (->hydra_rust_syntax_expr_path global segments))

(defrecord hydra_rust_syntax_call_expr [function args])
(defn make-hydra_rust_syntax_call_expr [function args] (->hydra_rust_syntax_call_expr function args))

(defrecord hydra_rust_syntax_method_call_expr [receiver method turbofish args])
(defn make-hydra_rust_syntax_method_call_expr [receiver method turbofish args] (->hydra_rust_syntax_method_call_expr receiver method turbofish args))

(defrecord hydra_rust_syntax_field_access_expr [object field])
(defn make-hydra_rust_syntax_field_access_expr [object field] (->hydra_rust_syntax_field_access_expr object field))

(defrecord hydra_rust_syntax_tuple_index_expr [tuple index])
(defn make-hydra_rust_syntax_tuple_index_expr [tuple index] (->hydra_rust_syntax_tuple_index_expr tuple index))

(defrecord hydra_rust_syntax_closure_expr [move params return_type body])
(defn make-hydra_rust_syntax_closure_expr [move params return_type body] (->hydra_rust_syntax_closure_expr move params return_type body))

(defrecord hydra_rust_syntax_closure_param [pattern type])
(defn make-hydra_rust_syntax_closure_param [pattern type] (->hydra_rust_syntax_closure_param pattern type))

(defrecord hydra_rust_syntax_if_expr [condition then_block else_branch])
(defn make-hydra_rust_syntax_if_expr [condition then_block else_branch] (->hydra_rust_syntax_if_expr condition then_block else_branch))

(def hydra_rust_syntax_if_condition-variants (list :bool :let))

(defrecord hydra_rust_syntax_match_expr [scrutinee arms])
(defn make-hydra_rust_syntax_match_expr [scrutinee arms] (->hydra_rust_syntax_match_expr scrutinee arms))

(defrecord hydra_rust_syntax_match_arm [pattern guard body])
(defn make-hydra_rust_syntax_match_arm [pattern guard body] (->hydra_rust_syntax_match_arm pattern guard body))

(defrecord hydra_rust_syntax_loop_expr [label body])
(defn make-hydra_rust_syntax_loop_expr [label body] (->hydra_rust_syntax_loop_expr label body))

(defrecord hydra_rust_syntax_while_expr [label condition body])
(defn make-hydra_rust_syntax_while_expr [label condition body] (->hydra_rust_syntax_while_expr label condition body))

(defrecord hydra_rust_syntax_for_expr [label pattern iter body])
(defn make-hydra_rust_syntax_for_expr [label pattern iter body] (->hydra_rust_syntax_for_expr label pattern iter body))

(defrecord hydra_rust_syntax_binary_expr [left op right])
(defn make-hydra_rust_syntax_binary_expr [left op right] (->hydra_rust_syntax_binary_expr left op right))

(def hydra_rust_syntax_binary_op-variants (list :add :sub :mul :div :rem :and :or :bit_and :bit_or :bit_xor :shl :shr :eq :ne :lt :le :gt :ge))

(defrecord hydra_rust_syntax_unary_expr [op operand])
(defn make-hydra_rust_syntax_unary_expr [op operand] (->hydra_rust_syntax_unary_expr op operand))

(def hydra_rust_syntax_unary_op-variants (list :neg :not))

(defrecord hydra_rust_syntax_ref_expr [mutable expr])
(defn make-hydra_rust_syntax_ref_expr [mutable expr] (->hydra_rust_syntax_ref_expr mutable expr))

(defrecord hydra_rust_syntax_struct_expr [path fields rest])
(defn make-hydra_rust_syntax_struct_expr [path fields rest] (->hydra_rust_syntax_struct_expr path fields rest))

(defrecord hydra_rust_syntax_field_value [name value])
(defn make-hydra_rust_syntax_field_value [name value] (->hydra_rust_syntax_field_value name value))

(def hydra_rust_syntax_array_expr-variants (list :elements :repeat))

(defrecord hydra_rust_syntax_index_expr [object index])
(defn make-hydra_rust_syntax_index_expr [object index] (->hydra_rust_syntax_index_expr object index))

(defrecord hydra_rust_syntax_range_expr [from to inclusive])
(defn make-hydra_rust_syntax_range_expr [from to inclusive] (->hydra_rust_syntax_range_expr from to inclusive))

(defrecord hydra_rust_syntax_cast_expr [expr type])
(defn make-hydra_rust_syntax_cast_expr [expr type] (->hydra_rust_syntax_cast_expr expr type))

(defrecord hydra_rust_syntax_type_ascription_expr [expr type])
(defn make-hydra_rust_syntax_type_ascription_expr [expr type] (->hydra_rust_syntax_type_ascription_expr expr type))

(defrecord hydra_rust_syntax_assign_expr [target value])
(defn make-hydra_rust_syntax_assign_expr [target value] (->hydra_rust_syntax_assign_expr target value))

(defrecord hydra_rust_syntax_compound_assign_expr [target op value])
(defn make-hydra_rust_syntax_compound_assign_expr [target op value] (->hydra_rust_syntax_compound_assign_expr target op value))

(def hydra_rust_syntax_compound_assign_op-variants (list :add_assign :sub_assign :mul_assign :div_assign :rem_assign :bit_and_assign :bit_or_assign :bit_xor_assign :shl_assign :shr_assign))

(defrecord hydra_rust_syntax_macro_invocation [path delimiter tokens])
(defn make-hydra_rust_syntax_macro_invocation [path delimiter tokens] (->hydra_rust_syntax_macro_invocation path delimiter tokens))

(def hydra_rust_syntax_macro_delimiter-variants (list :paren :bracket :brace))

(def hydra_rust_syntax_statement-variants (list :let :expression :item :empty))

(defrecord hydra_rust_syntax_let_statement [pattern mutable type init])
(defn make-hydra_rust_syntax_let_statement [pattern mutable type init] (->hydra_rust_syntax_let_statement pattern mutable type init))

(defrecord hydra_rust_syntax_block [statements expression])
(defn make-hydra_rust_syntax_block [statements expression] (->hydra_rust_syntax_block statements expression))

(def hydra_rust_syntax_pattern-variants (list :wildcard :identifier :literal :reference :struct :tuple_struct :tuple :slice :or :path :range :rest :paren))

(defrecord hydra_rust_syntax_identifier_pattern [name mutable at_pattern])
(defn make-hydra_rust_syntax_identifier_pattern [name mutable at_pattern] (->hydra_rust_syntax_identifier_pattern name mutable at_pattern))

(defrecord hydra_rust_syntax_ref_pattern [mutable pattern])
(defn make-hydra_rust_syntax_ref_pattern [mutable pattern] (->hydra_rust_syntax_ref_pattern mutable pattern))

(defrecord hydra_rust_syntax_struct_pattern [path fields rest])
(defn make-hydra_rust_syntax_struct_pattern [path fields rest] (->hydra_rust_syntax_struct_pattern path fields rest))

(defrecord hydra_rust_syntax_field_pattern [name pattern])
(defn make-hydra_rust_syntax_field_pattern [name pattern] (->hydra_rust_syntax_field_pattern name pattern))

(defrecord hydra_rust_syntax_tuple_struct_pattern [path elements])
(defn make-hydra_rust_syntax_tuple_struct_pattern [path elements] (->hydra_rust_syntax_tuple_struct_pattern path elements))

(defrecord hydra_rust_syntax_range_pattern [from to inclusive])
(defn make-hydra_rust_syntax_range_pattern [from to inclusive] (->hydra_rust_syntax_range_pattern from to inclusive))

(def hydra_rust_syntax_literal-variants (list :integer :float :string :raw_string :byte_string :char :byte :bool))

(defrecord hydra_rust_syntax_integer_literal [value suffix])
(defn make-hydra_rust_syntax_integer_literal [value suffix] (->hydra_rust_syntax_integer_literal value suffix))

(defrecord hydra_rust_syntax_float_literal [value suffix])
(defn make-hydra_rust_syntax_float_literal [value suffix] (->hydra_rust_syntax_float_literal value suffix))

(defrecord hydra_rust_syntax_attribute [inner path tokens])
(defn make-hydra_rust_syntax_attribute [inner path tokens] (->hydra_rust_syntax_attribute inner path tokens))

(def hydra_rust_syntax_visibility-variants (list :public :crate :restricted :private))

(defrecord hydra_rust_syntax_let_condition [pattern expr])
(defn make-hydra_rust_syntax_let_condition [pattern expr] (->hydra_rust_syntax_let_condition pattern expr))

(defrecord hydra_rust_syntax_array_repeat [element length])
(defn make-hydra_rust_syntax_array_repeat [element length] (->hydra_rust_syntax_array_repeat element length))
