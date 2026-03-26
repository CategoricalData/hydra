(ns hydra.ext.scala.syntax)

(declare hydra_ext_scala_syntax_tree-variants hydra_ext_scala_syntax_ref-variants hydra_ext_scala_syntax_stat-variants hydra_ext_scala_syntax_name-variants hydra_ext_scala_syntax_lit-variants hydra_ext_scala_syntax_data-variants hydra_ext_scala_syntax_data_ref-variants hydra_ext_scala_syntax_data_function_data-variants hydra_ext_scala_syntax_type-variants hydra_ext_scala_syntax_type_ref-variants hydra_ext_scala_syntax_type_function_type-variants hydra_ext_scala_syntax_pat-variants hydra_ext_scala_syntax_member-variants hydra_ext_scala_syntax_member_data-variants hydra_ext_scala_syntax_decl-variants hydra_ext_scala_syntax_defn-variants hydra_ext_scala_syntax_ctor-variants hydra_ext_scala_syntax_mod-variants hydra_ext_scala_syntax_enumerator-variants hydra_ext_scala_syntax_import_export_stat-variants hydra_ext_scala_syntax_importee-variants hydra_ext_scala_syntax_case_tree-variants)

(defrecord hydra_ext_scala_syntax_predef_string [value])
(defn make-hydra_ext_scala_syntax_predef_string [value] (->hydra_ext_scala_syntax_predef_string value))

(defrecord hydra_ext_scala_syntax_scala_symbol [name])
(defn make-hydra_ext_scala_syntax_scala_symbol [name] (->hydra_ext_scala_syntax_scala_symbol name))

(def hydra_ext_scala_syntax_tree-variants (list :ref :stat :type :bounds :pat :member :ctor :template :mod :enumerator :importer :importee :case_tree :source :quasi))

(def hydra_ext_scala_syntax_ref-variants (list :name :init))

(def hydra_ext_scala_syntax_stat-variants (list :term :decl :defn :import_export))

(def hydra_ext_scala_syntax_name-variants (list :value :anonymous :indeterminate))

(def hydra_ext_scala_syntax_lit-variants (list :null :int :double :float :byte :short :char :long :boolean :unit :string :symbol))

(def hydra_ext_scala_syntax_data-variants (list :lit :ref :interpolate :xml :apply :apply_using :apply_type :assign :return :throw :ascribe :annotate :tuple :block :end_marker :if :quoted_macro_expr :quoted_macro_type :spliced_macro_expr :match :try :try_with_handler :function_data :poly_function :partial_function :while :do :for :for_yield :new :new_anonymous :placeholder :eta :repeated :param))

(def hydra_ext_scala_syntax_data_ref-variants (list :this :super :name :anonymous :select :apply_unary))

(defrecord hydra_ext_scala_syntax_data_this [value])
(defn make-hydra_ext_scala_syntax_data_this [value] (->hydra_ext_scala_syntax_data_this value))

(defrecord hydra_ext_scala_syntax_data_super [thisp superp])
(defn make-hydra_ext_scala_syntax_data_super [thisp superp] (->hydra_ext_scala_syntax_data_super thisp superp))

(defrecord hydra_ext_scala_syntax_data_name [value])
(defn make-hydra_ext_scala_syntax_data_name [value] (->hydra_ext_scala_syntax_data_name value))

(defrecord hydra_ext_scala_syntax_data_anonymous [value])
(defn make-hydra_ext_scala_syntax_data_anonymous [value] (->hydra_ext_scala_syntax_data_anonymous value))

(defrecord hydra_ext_scala_syntax_data_select [qual name])
(defn make-hydra_ext_scala_syntax_data_select [qual name] (->hydra_ext_scala_syntax_data_select qual name))

(defrecord hydra_ext_scala_syntax_data_interpolate [prefix parts args])
(defn make-hydra_ext_scala_syntax_data_interpolate [prefix parts args] (->hydra_ext_scala_syntax_data_interpolate prefix parts args))

(defrecord hydra_ext_scala_syntax_data_xml [parts args])
(defn make-hydra_ext_scala_syntax_data_xml [parts args] (->hydra_ext_scala_syntax_data_xml parts args))

(defrecord hydra_ext_scala_syntax_data_apply [fun args])
(defn make-hydra_ext_scala_syntax_data_apply [fun args] (->hydra_ext_scala_syntax_data_apply fun args))

(defrecord hydra_ext_scala_syntax_data_apply_using [fun targs])
(defn make-hydra_ext_scala_syntax_data_apply_using [fun targs] (->hydra_ext_scala_syntax_data_apply_using fun targs))

(defrecord hydra_ext_scala_syntax_data_apply_type [lhs op targs args])
(defn make-hydra_ext_scala_syntax_data_apply_type [lhs op targs args] (->hydra_ext_scala_syntax_data_apply_type lhs op targs args))

(defrecord hydra_ext_scala_syntax_data_apply_infix [lhs op targs args])
(defn make-hydra_ext_scala_syntax_data_apply_infix [lhs op targs args] (->hydra_ext_scala_syntax_data_apply_infix lhs op targs args))

(defrecord hydra_ext_scala_syntax_data_apply_unary [op arg])
(defn make-hydra_ext_scala_syntax_data_apply_unary [op arg] (->hydra_ext_scala_syntax_data_apply_unary op arg))

(defrecord hydra_ext_scala_syntax_data_assign [lhs rhs])
(defn make-hydra_ext_scala_syntax_data_assign [lhs rhs] (->hydra_ext_scala_syntax_data_assign lhs rhs))

(defrecord hydra_ext_scala_syntax_data_return [expr])
(defn make-hydra_ext_scala_syntax_data_return [expr] (->hydra_ext_scala_syntax_data_return expr))

(defrecord hydra_ext_scala_syntax_data_throw [expr])
(defn make-hydra_ext_scala_syntax_data_throw [expr] (->hydra_ext_scala_syntax_data_throw expr))

(defrecord hydra_ext_scala_syntax_data_ascribe [expr tpe])
(defn make-hydra_ext_scala_syntax_data_ascribe [expr tpe] (->hydra_ext_scala_syntax_data_ascribe expr tpe))

(defrecord hydra_ext_scala_syntax_data_annotate [expr annots])
(defn make-hydra_ext_scala_syntax_data_annotate [expr annots] (->hydra_ext_scala_syntax_data_annotate expr annots))

(defrecord hydra_ext_scala_syntax_data_tuple [args])
(defn make-hydra_ext_scala_syntax_data_tuple [args] (->hydra_ext_scala_syntax_data_tuple args))

(defrecord hydra_ext_scala_syntax_data_block [stats])
(defn make-hydra_ext_scala_syntax_data_block [stats] (->hydra_ext_scala_syntax_data_block stats))

(defrecord hydra_ext_scala_syntax_data_end_marker [name])
(defn make-hydra_ext_scala_syntax_data_end_marker [name] (->hydra_ext_scala_syntax_data_end_marker name))

(defrecord hydra_ext_scala_syntax_data_if [cond thenp elsep])
(defn make-hydra_ext_scala_syntax_data_if [cond thenp elsep] (->hydra_ext_scala_syntax_data_if cond thenp elsep))

(defrecord hydra_ext_scala_syntax_data_quoted_macro_expr [body])
(defn make-hydra_ext_scala_syntax_data_quoted_macro_expr [body] (->hydra_ext_scala_syntax_data_quoted_macro_expr body))

(defrecord hydra_ext_scala_syntax_data_quoted_macro_type [tpe])
(defn make-hydra_ext_scala_syntax_data_quoted_macro_type [tpe] (->hydra_ext_scala_syntax_data_quoted_macro_type tpe))

(defrecord hydra_ext_scala_syntax_data_spliced_macro_expr [body])
(defn make-hydra_ext_scala_syntax_data_spliced_macro_expr [body] (->hydra_ext_scala_syntax_data_spliced_macro_expr body))

(defrecord hydra_ext_scala_syntax_data_match [expr cases])
(defn make-hydra_ext_scala_syntax_data_match [expr cases] (->hydra_ext_scala_syntax_data_match expr cases))

(defrecord hydra_ext_scala_syntax_data_try [expr catchp finallyp])
(defn make-hydra_ext_scala_syntax_data_try [expr catchp finallyp] (->hydra_ext_scala_syntax_data_try expr catchp finallyp))

(defrecord hydra_ext_scala_syntax_data_try_with_handler [expr catchp finallyp])
(defn make-hydra_ext_scala_syntax_data_try_with_handler [expr catchp finallyp] (->hydra_ext_scala_syntax_data_try_with_handler expr catchp finallyp))

(def hydra_ext_scala_syntax_data_function_data-variants (list :context_function :function))

(defrecord hydra_ext_scala_syntax_data_context_function [params body])
(defn make-hydra_ext_scala_syntax_data_context_function [params body] (->hydra_ext_scala_syntax_data_context_function params body))

(defrecord hydra_ext_scala_syntax_data_function [params body])
(defn make-hydra_ext_scala_syntax_data_function [params body] (->hydra_ext_scala_syntax_data_function params body))

(defrecord hydra_ext_scala_syntax_data_poly_function [tparams body])
(defn make-hydra_ext_scala_syntax_data_poly_function [tparams body] (->hydra_ext_scala_syntax_data_poly_function tparams body))

(defrecord hydra_ext_scala_syntax_data_partial_function [cases])
(defn make-hydra_ext_scala_syntax_data_partial_function [cases] (->hydra_ext_scala_syntax_data_partial_function cases))

(defrecord hydra_ext_scala_syntax_data_while [expr body])
(defn make-hydra_ext_scala_syntax_data_while [expr body] (->hydra_ext_scala_syntax_data_while expr body))

(defrecord hydra_ext_scala_syntax_data_do [body expr])
(defn make-hydra_ext_scala_syntax_data_do [body expr] (->hydra_ext_scala_syntax_data_do body expr))

(defrecord hydra_ext_scala_syntax_data_for [enums])
(defn make-hydra_ext_scala_syntax_data_for [enums] (->hydra_ext_scala_syntax_data_for enums))

(defrecord hydra_ext_scala_syntax_data_for_yield [enums])
(defn make-hydra_ext_scala_syntax_data_for_yield [enums] (->hydra_ext_scala_syntax_data_for_yield enums))

(defrecord hydra_ext_scala_syntax_data_new [init])
(defn make-hydra_ext_scala_syntax_data_new [init] (->hydra_ext_scala_syntax_data_new init))

(defrecord hydra_ext_scala_syntax_data_new_anonymous [templ])
(defn make-hydra_ext_scala_syntax_data_new_anonymous [templ] (->hydra_ext_scala_syntax_data_new_anonymous templ))

(defrecord hydra_ext_scala_syntax_data_eta [expr])
(defn make-hydra_ext_scala_syntax_data_eta [expr] (->hydra_ext_scala_syntax_data_eta expr))

(defrecord hydra_ext_scala_syntax_data_repeated [expr])
(defn make-hydra_ext_scala_syntax_data_repeated [expr] (->hydra_ext_scala_syntax_data_repeated expr))

(defrecord hydra_ext_scala_syntax_data_param [mods name decltpe default])
(defn make-hydra_ext_scala_syntax_data_param [mods name decltpe default] (->hydra_ext_scala_syntax_data_param mods name decltpe default))

(def hydra_ext_scala_syntax_type-variants (list :ref :anonymous_name :apply :apply_infix :function_type :poly_function :implicit_function :tuple :with :and :or :refine :existential :annotate :lambda :macro :method :placeholder :by_name :repeated :var :typed_param :match))

(def hydra_ext_scala_syntax_type_ref-variants (list :name :select :project :singleton))

(defrecord hydra_ext_scala_syntax_type_name [value])
(defn make-hydra_ext_scala_syntax_type_name [value] (->hydra_ext_scala_syntax_type_name value))

(defrecord hydra_ext_scala_syntax_type_anonymous_name [value])
(defn make-hydra_ext_scala_syntax_type_anonymous_name [value] (->hydra_ext_scala_syntax_type_anonymous_name value))

(defrecord hydra_ext_scala_syntax_type_select [qual name])
(defn make-hydra_ext_scala_syntax_type_select [qual name] (->hydra_ext_scala_syntax_type_select qual name))

(defrecord hydra_ext_scala_syntax_type_project [qual name])
(defn make-hydra_ext_scala_syntax_type_project [qual name] (->hydra_ext_scala_syntax_type_project qual name))

(defrecord hydra_ext_scala_syntax_type_singleton [ref])
(defn make-hydra_ext_scala_syntax_type_singleton [ref] (->hydra_ext_scala_syntax_type_singleton ref))

(defrecord hydra_ext_scala_syntax_type_apply [tpe args])
(defn make-hydra_ext_scala_syntax_type_apply [tpe args] (->hydra_ext_scala_syntax_type_apply tpe args))

(defrecord hydra_ext_scala_syntax_type_apply_infix [lhs op rhs])
(defn make-hydra_ext_scala_syntax_type_apply_infix [lhs op rhs] (->hydra_ext_scala_syntax_type_apply_infix lhs op rhs))

(def hydra_ext_scala_syntax_type_function_type-variants (list :function :context_function))

(defrecord hydra_ext_scala_syntax_type_function [params res])
(defn make-hydra_ext_scala_syntax_type_function [params res] (->hydra_ext_scala_syntax_type_function params res))

(defrecord hydra_ext_scala_syntax_type_poly_function [tparams tpe])
(defn make-hydra_ext_scala_syntax_type_poly_function [tparams tpe] (->hydra_ext_scala_syntax_type_poly_function tparams tpe))

(defrecord hydra_ext_scala_syntax_type_context_function [params res])
(defn make-hydra_ext_scala_syntax_type_context_function [params res] (->hydra_ext_scala_syntax_type_context_function params res))

(defrecord hydra_ext_scala_syntax_type_implicit_function [params res])
(defn make-hydra_ext_scala_syntax_type_implicit_function [params res] (->hydra_ext_scala_syntax_type_implicit_function params res))

(defrecord hydra_ext_scala_syntax_type_tuple [args])
(defn make-hydra_ext_scala_syntax_type_tuple [args] (->hydra_ext_scala_syntax_type_tuple args))

(defrecord hydra_ext_scala_syntax_type_with [lhs rhs])
(defn make-hydra_ext_scala_syntax_type_with [lhs rhs] (->hydra_ext_scala_syntax_type_with lhs rhs))

(defrecord hydra_ext_scala_syntax_type_and [lhs rhs])
(defn make-hydra_ext_scala_syntax_type_and [lhs rhs] (->hydra_ext_scala_syntax_type_and lhs rhs))

(defrecord hydra_ext_scala_syntax_type_or [lhs rhs])
(defn make-hydra_ext_scala_syntax_type_or [lhs rhs] (->hydra_ext_scala_syntax_type_or lhs rhs))

(defrecord hydra_ext_scala_syntax_type_refine [tpe stats])
(defn make-hydra_ext_scala_syntax_type_refine [tpe stats] (->hydra_ext_scala_syntax_type_refine tpe stats))

(defrecord hydra_ext_scala_syntax_type_existential [tpe stats])
(defn make-hydra_ext_scala_syntax_type_existential [tpe stats] (->hydra_ext_scala_syntax_type_existential tpe stats))

(defrecord hydra_ext_scala_syntax_type_annotate [tpe annots])
(defn make-hydra_ext_scala_syntax_type_annotate [tpe annots] (->hydra_ext_scala_syntax_type_annotate tpe annots))

(defrecord hydra_ext_scala_syntax_type_lambda [tparams tpe])
(defn make-hydra_ext_scala_syntax_type_lambda [tparams tpe] (->hydra_ext_scala_syntax_type_lambda tparams tpe))

(defrecord hydra_ext_scala_syntax_type_macro [body])
(defn make-hydra_ext_scala_syntax_type_macro [body] (->hydra_ext_scala_syntax_type_macro body))

(defrecord hydra_ext_scala_syntax_type_method [paramss tpe])
(defn make-hydra_ext_scala_syntax_type_method [paramss tpe] (->hydra_ext_scala_syntax_type_method paramss tpe))

(defrecord hydra_ext_scala_syntax_type_placeholder [bounds])
(defn make-hydra_ext_scala_syntax_type_placeholder [bounds] (->hydra_ext_scala_syntax_type_placeholder bounds))

(defrecord hydra_ext_scala_syntax_type_bounds [lo hi])
(defn make-hydra_ext_scala_syntax_type_bounds [lo hi] (->hydra_ext_scala_syntax_type_bounds lo hi))

(defrecord hydra_ext_scala_syntax_type_by_name [tpe])
(defn make-hydra_ext_scala_syntax_type_by_name [tpe] (->hydra_ext_scala_syntax_type_by_name tpe))

(defrecord hydra_ext_scala_syntax_type_repeated [tpe])
(defn make-hydra_ext_scala_syntax_type_repeated [tpe] (->hydra_ext_scala_syntax_type_repeated tpe))

(defrecord hydra_ext_scala_syntax_type_var [name])
(defn make-hydra_ext_scala_syntax_type_var [name] (->hydra_ext_scala_syntax_type_var name))

(defrecord hydra_ext_scala_syntax_type_typed_param [name typ])
(defn make-hydra_ext_scala_syntax_type_typed_param [name typ] (->hydra_ext_scala_syntax_type_typed_param name typ))

(defrecord hydra_ext_scala_syntax_type_param [mods name tparams tbounds vbounds cbounds])
(defn make-hydra_ext_scala_syntax_type_param [mods name tparams tbounds vbounds cbounds] (->hydra_ext_scala_syntax_type_param mods name tparams tbounds vbounds cbounds))

(defrecord hydra_ext_scala_syntax_type_match [tpe cases])
(defn make-hydra_ext_scala_syntax_type_match [tpe cases] (->hydra_ext_scala_syntax_type_match tpe cases))

(def hydra_ext_scala_syntax_pat-variants (list :var :wildcard :seq_wildcard :bind :alternative :tuple :repeated :extract :extract_infix :interpolate :xml :typed :macro :given))

(defrecord hydra_ext_scala_syntax_pat_var [name])
(defn make-hydra_ext_scala_syntax_pat_var [name] (->hydra_ext_scala_syntax_pat_var name))

(defrecord hydra_ext_scala_syntax_pat_bind [lhs rhs])
(defn make-hydra_ext_scala_syntax_pat_bind [lhs rhs] (->hydra_ext_scala_syntax_pat_bind lhs rhs))

(defrecord hydra_ext_scala_syntax_pat_alternative [lhs rhs])
(defn make-hydra_ext_scala_syntax_pat_alternative [lhs rhs] (->hydra_ext_scala_syntax_pat_alternative lhs rhs))

(defrecord hydra_ext_scala_syntax_pat_tuple [args])
(defn make-hydra_ext_scala_syntax_pat_tuple [args] (->hydra_ext_scala_syntax_pat_tuple args))

(defrecord hydra_ext_scala_syntax_pat_repeated [name])
(defn make-hydra_ext_scala_syntax_pat_repeated [name] (->hydra_ext_scala_syntax_pat_repeated name))

(defrecord hydra_ext_scala_syntax_pat_extract [fun args])
(defn make-hydra_ext_scala_syntax_pat_extract [fun args] (->hydra_ext_scala_syntax_pat_extract fun args))

(defrecord hydra_ext_scala_syntax_pat_extract_infix [lhs op rhs])
(defn make-hydra_ext_scala_syntax_pat_extract_infix [lhs op rhs] (->hydra_ext_scala_syntax_pat_extract_infix lhs op rhs))

(defrecord hydra_ext_scala_syntax_pat_interpolate [prefix parts])
(defn make-hydra_ext_scala_syntax_pat_interpolate [prefix parts] (->hydra_ext_scala_syntax_pat_interpolate prefix parts))

(defrecord hydra_ext_scala_syntax_pat_xml [parts args])
(defn make-hydra_ext_scala_syntax_pat_xml [parts args] (->hydra_ext_scala_syntax_pat_xml parts args))

(defrecord hydra_ext_scala_syntax_pat_typed [lhs rhs])
(defn make-hydra_ext_scala_syntax_pat_typed [lhs rhs] (->hydra_ext_scala_syntax_pat_typed lhs rhs))

(defrecord hydra_ext_scala_syntax_pat_macro [body])
(defn make-hydra_ext_scala_syntax_pat_macro [body] (->hydra_ext_scala_syntax_pat_macro body))

(defrecord hydra_ext_scala_syntax_pat_given [tpe])
(defn make-hydra_ext_scala_syntax_pat_given [tpe] (->hydra_ext_scala_syntax_pat_given tpe))

(def hydra_ext_scala_syntax_member-variants (list :term :type :term_param :type_param :self))

(def hydra_ext_scala_syntax_member_data-variants (list :pkg :object))

(defrecord hydra_ext_scala_syntax_member_type [name])
(defn make-hydra_ext_scala_syntax_member_type [name] (->hydra_ext_scala_syntax_member_type name))

(def hydra_ext_scala_syntax_decl-variants (list :val :var :def :type :given))

(defrecord hydra_ext_scala_syntax_decl_val [mods pats decltpe])
(defn make-hydra_ext_scala_syntax_decl_val [mods pats decltpe] (->hydra_ext_scala_syntax_decl_val mods pats decltpe))

(defrecord hydra_ext_scala_syntax_decl_var [mods pats decltpe])
(defn make-hydra_ext_scala_syntax_decl_var [mods pats decltpe] (->hydra_ext_scala_syntax_decl_var mods pats decltpe))

(defrecord hydra_ext_scala_syntax_decl_def [mods name tparams paramss decltpe])
(defn make-hydra_ext_scala_syntax_decl_def [mods name tparams paramss decltpe] (->hydra_ext_scala_syntax_decl_def mods name tparams paramss decltpe))

(defrecord hydra_ext_scala_syntax_decl_type [mods name tparams bounds])
(defn make-hydra_ext_scala_syntax_decl_type [mods name tparams bounds] (->hydra_ext_scala_syntax_decl_type mods name tparams bounds))

(defrecord hydra_ext_scala_syntax_decl_given [mods name tparams sparams decltpe])
(defn make-hydra_ext_scala_syntax_decl_given [mods name tparams sparams decltpe] (->hydra_ext_scala_syntax_decl_given mods name tparams sparams decltpe))

(def hydra_ext_scala_syntax_defn-variants (list :val :var :given :enum :enum_case :repeated_enum_case :given_alias :extension_group :def :macro :type :class :trait :object))

(defrecord hydra_ext_scala_syntax_defn_val [mods pats decltpe rhs])
(defn make-hydra_ext_scala_syntax_defn_val [mods pats decltpe rhs] (->hydra_ext_scala_syntax_defn_val mods pats decltpe rhs))

(defrecord hydra_ext_scala_syntax_defn_var [mods pats decltpe rhs])
(defn make-hydra_ext_scala_syntax_defn_var [mods pats decltpe rhs] (->hydra_ext_scala_syntax_defn_var mods pats decltpe rhs))

(defrecord hydra_ext_scala_syntax_defn_given [mods name tparams sparams templ])
(defn make-hydra_ext_scala_syntax_defn_given [mods name tparams sparams templ] (->hydra_ext_scala_syntax_defn_given mods name tparams sparams templ))

(defrecord hydra_ext_scala_syntax_defn_enum [mods name tparams ctor template])
(defn make-hydra_ext_scala_syntax_defn_enum [mods name tparams ctor template] (->hydra_ext_scala_syntax_defn_enum mods name tparams ctor template))

(defrecord hydra_ext_scala_syntax_defn_enum_case [mods name tparams ctor inits])
(defn make-hydra_ext_scala_syntax_defn_enum_case [mods name tparams ctor inits] (->hydra_ext_scala_syntax_defn_enum_case mods name tparams ctor inits))

(defrecord hydra_ext_scala_syntax_defn_repeated_enum_case [mods cases])
(defn make-hydra_ext_scala_syntax_defn_repeated_enum_case [mods cases] (->hydra_ext_scala_syntax_defn_repeated_enum_case mods cases))

(defrecord hydra_ext_scala_syntax_defn_given_alias [mods name tparams sparams decltpe body])
(defn make-hydra_ext_scala_syntax_defn_given_alias [mods name tparams sparams decltpe body] (->hydra_ext_scala_syntax_defn_given_alias mods name tparams sparams decltpe body))

(defrecord hydra_ext_scala_syntax_defn_extension_group [tparams parmss body])
(defn make-hydra_ext_scala_syntax_defn_extension_group [tparams parmss body] (->hydra_ext_scala_syntax_defn_extension_group tparams parmss body))

(defrecord hydra_ext_scala_syntax_defn_def [mods name tparams paramss decltpe body])
(defn make-hydra_ext_scala_syntax_defn_def [mods name tparams paramss decltpe body] (->hydra_ext_scala_syntax_defn_def mods name tparams paramss decltpe body))

(defrecord hydra_ext_scala_syntax_defn_macro [mods name tparams paramss decltpe body])
(defn make-hydra_ext_scala_syntax_defn_macro [mods name tparams paramss decltpe body] (->hydra_ext_scala_syntax_defn_macro mods name tparams paramss decltpe body))

(defrecord hydra_ext_scala_syntax_defn_type [mods name tparams body])
(defn make-hydra_ext_scala_syntax_defn_type [mods name tparams body] (->hydra_ext_scala_syntax_defn_type mods name tparams body))

(defrecord hydra_ext_scala_syntax_defn_class [mods name tparams ctor template])
(defn make-hydra_ext_scala_syntax_defn_class [mods name tparams ctor template] (->hydra_ext_scala_syntax_defn_class mods name tparams ctor template))

(defrecord hydra_ext_scala_syntax_defn_trait [mods name tparams ctor template])
(defn make-hydra_ext_scala_syntax_defn_trait [mods name tparams ctor template] (->hydra_ext_scala_syntax_defn_trait mods name tparams ctor template))

(defrecord hydra_ext_scala_syntax_defn_object [name])
(defn make-hydra_ext_scala_syntax_defn_object [name] (->hydra_ext_scala_syntax_defn_object name))

(defrecord hydra_ext_scala_syntax_pkg [name ref stats])
(defn make-hydra_ext_scala_syntax_pkg [name ref stats] (->hydra_ext_scala_syntax_pkg name ref stats))

(defrecord hydra_ext_scala_syntax_pkg_object [mods name template])
(defn make-hydra_ext_scala_syntax_pkg_object [mods name template] (->hydra_ext_scala_syntax_pkg_object mods name template))

(def hydra_ext_scala_syntax_ctor-variants (list :primary :secondary))

(defrecord hydra_ext_scala_syntax_ctor_primary [mods name paramss])
(defn make-hydra_ext_scala_syntax_ctor_primary [mods name paramss] (->hydra_ext_scala_syntax_ctor_primary mods name paramss))

(defrecord hydra_ext_scala_syntax_ctor_secondary [mods name paramss init stats])
(defn make-hydra_ext_scala_syntax_ctor_secondary [mods name paramss init stats] (->hydra_ext_scala_syntax_ctor_secondary mods name paramss init stats))

(defrecord hydra_ext_scala_syntax_init [tpe name argss])
(defn make-hydra_ext_scala_syntax_init [tpe name argss] (->hydra_ext_scala_syntax_init tpe name argss))

(defrecord hydra_ext_scala_syntax_self [value])
(defn make-hydra_ext_scala_syntax_self [value] (->hydra_ext_scala_syntax_self value))

(defrecord hydra_ext_scala_syntax_template [early inits self stats])
(defn make-hydra_ext_scala_syntax_template [early inits self stats] (->hydra_ext_scala_syntax_template early inits self stats))

(def hydra_ext_scala_syntax_mod-variants (list :annot :private :protected :implicit :final :sealed :open :super :override :case :abstract :covariant :contravariant :lazy :val_param :var_param :infix :inline :using :opaque :transparent))

(defrecord hydra_ext_scala_syntax_mod_annot [init])
(defn make-hydra_ext_scala_syntax_mod_annot [init] (->hydra_ext_scala_syntax_mod_annot init))

(defrecord hydra_ext_scala_syntax_mod_private [within])
(defn make-hydra_ext_scala_syntax_mod_private [within] (->hydra_ext_scala_syntax_mod_private within))

(defrecord hydra_ext_scala_syntax_mod_protected [within])
(defn make-hydra_ext_scala_syntax_mod_protected [within] (->hydra_ext_scala_syntax_mod_protected within))

(def hydra_ext_scala_syntax_enumerator-variants (list :generator :case_generator :val :guard))

(defrecord hydra_ext_scala_syntax_enumerator_generator [pat rhs])
(defn make-hydra_ext_scala_syntax_enumerator_generator [pat rhs] (->hydra_ext_scala_syntax_enumerator_generator pat rhs))

(defrecord hydra_ext_scala_syntax_enumerator_case_generator [pat rhs])
(defn make-hydra_ext_scala_syntax_enumerator_case_generator [pat rhs] (->hydra_ext_scala_syntax_enumerator_case_generator pat rhs))

(defrecord hydra_ext_scala_syntax_enumerator_val [pat rhs])
(defn make-hydra_ext_scala_syntax_enumerator_val [pat rhs] (->hydra_ext_scala_syntax_enumerator_val pat rhs))

(defrecord hydra_ext_scala_syntax_enumerator_guard [cond])
(defn make-hydra_ext_scala_syntax_enumerator_guard [cond] (->hydra_ext_scala_syntax_enumerator_guard cond))

(def hydra_ext_scala_syntax_import_export_stat-variants (list :import :export))

(defrecord hydra_ext_scala_syntax_import [importers])
(defn make-hydra_ext_scala_syntax_import [importers] (->hydra_ext_scala_syntax_import importers))

(defrecord hydra_ext_scala_syntax_export [importers])
(defn make-hydra_ext_scala_syntax_export [importers] (->hydra_ext_scala_syntax_export importers))

(defrecord hydra_ext_scala_syntax_importer [ref importees])
(defn make-hydra_ext_scala_syntax_importer [ref importees] (->hydra_ext_scala_syntax_importer ref importees))

(def hydra_ext_scala_syntax_importee-variants (list :wildcard :given :given_all :name :rename :unimport))

(defrecord hydra_ext_scala_syntax_importee_given [tpe])
(defn make-hydra_ext_scala_syntax_importee_given [tpe] (->hydra_ext_scala_syntax_importee_given tpe))

(defrecord hydra_ext_scala_syntax_importee_name [name])
(defn make-hydra_ext_scala_syntax_importee_name [name] (->hydra_ext_scala_syntax_importee_name name))

(defrecord hydra_ext_scala_syntax_importee_rename [name rename])
(defn make-hydra_ext_scala_syntax_importee_rename [name rename] (->hydra_ext_scala_syntax_importee_rename name rename))

(defrecord hydra_ext_scala_syntax_importee_unimport [name])
(defn make-hydra_ext_scala_syntax_importee_unimport [name] (->hydra_ext_scala_syntax_importee_unimport name))

(def hydra_ext_scala_syntax_case_tree-variants (list :case :type_case))

(defrecord hydra_ext_scala_syntax_case [pat cond body])
(defn make-hydra_ext_scala_syntax_case [pat cond body] (->hydra_ext_scala_syntax_case pat cond body))

(defrecord hydra_ext_scala_syntax_type_case [pat body])
(defn make-hydra_ext_scala_syntax_type_case [pat body] (->hydra_ext_scala_syntax_type_case pat body))

(defrecord hydra_ext_scala_syntax_source [stats])
(defn make-hydra_ext_scala_syntax_source [stats] (->hydra_ext_scala_syntax_source stats))

(defrecord hydra_ext_scala_syntax_quasi [value])
(defn make-hydra_ext_scala_syntax_quasi [value] (->hydra_ext_scala_syntax_quasi value))
