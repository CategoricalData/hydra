(require 'cl-lib)

(cl-defstruct hydra_ext_scala_syntax_predef_string value)

(cl-defstruct hydra_ext_scala_syntax_scala_symbol name)

(defvar hydra_ext_scala_syntax_tree-variants (list :ref :stat :type :bounds :pat :member :ctor :template :mod :enumerator :importer :importee :case_tree :source :quasi))

(defvar hydra_ext_scala_syntax_ref-variants (list :name :init))

(defvar hydra_ext_scala_syntax_stat-variants (list :term :decl :defn :import_export))

(defvar hydra_ext_scala_syntax_name-variants (list :value :anonymous :indeterminate))

(defvar hydra_ext_scala_syntax_lit-variants (list :null :int :double :float :byte :short :char :long :boolean :unit :string :bytes :symbol))

(defvar hydra_ext_scala_syntax_data-variants (list :lit :ref :interpolate :xml :apply :apply_using :apply_type :assign :return :throw :ascribe :annotate :tuple :block :end_marker :if :quoted_macro_expr :quoted_macro_type :spliced_macro_expr :match :try :try_with_handler :function_data :poly_function :partial_function :while :do :for :for_yield :new :new_anonymous :placeholder :eta :repeated :param))

(defvar hydra_ext_scala_syntax_data_ref-variants (list :this :super :name :anonymous :select :apply_unary))

(cl-defstruct hydra_ext_scala_syntax_data_this value)

(cl-defstruct hydra_ext_scala_syntax_data_super thisp superp)

(cl-defstruct hydra_ext_scala_syntax_data_name value)

(cl-defstruct hydra_ext_scala_syntax_data_anonymous value)

(cl-defstruct hydra_ext_scala_syntax_data_select qual name)

(cl-defstruct hydra_ext_scala_syntax_data_interpolate prefix parts args)

(cl-defstruct hydra_ext_scala_syntax_data_xml parts args)

(cl-defstruct hydra_ext_scala_syntax_data_apply fun args)

(cl-defstruct hydra_ext_scala_syntax_data_apply_using fun targs)

(cl-defstruct hydra_ext_scala_syntax_data_apply_type lhs op targs args)

(cl-defstruct hydra_ext_scala_syntax_data_apply_infix lhs op targs args)

(cl-defstruct hydra_ext_scala_syntax_data_apply_unary op arg)

(cl-defstruct hydra_ext_scala_syntax_data_assign lhs rhs)

(cl-defstruct hydra_ext_scala_syntax_data_return expr)

(cl-defstruct hydra_ext_scala_syntax_data_throw expr)

(cl-defstruct hydra_ext_scala_syntax_data_ascribe expr tpe)

(cl-defstruct hydra_ext_scala_syntax_data_annotate expr annots)

(cl-defstruct hydra_ext_scala_syntax_data_tuple args)

(cl-defstruct hydra_ext_scala_syntax_data_block stats)

(cl-defstruct hydra_ext_scala_syntax_data_end_marker name)

(cl-defstruct hydra_ext_scala_syntax_data_if cond thenp elsep)

(cl-defstruct hydra_ext_scala_syntax_data_quoted_macro_expr body)

(cl-defstruct hydra_ext_scala_syntax_data_quoted_macro_type tpe)

(cl-defstruct hydra_ext_scala_syntax_data_spliced_macro_expr body)

(cl-defstruct hydra_ext_scala_syntax_data_match expr cases)

(cl-defstruct hydra_ext_scala_syntax_data_try expr catchp finallyp)

(cl-defstruct hydra_ext_scala_syntax_data_try_with_handler expr catchp finallyp)

(defvar hydra_ext_scala_syntax_data_function_data-variants (list :context_function :function))

(cl-defstruct hydra_ext_scala_syntax_data_context_function params body)

(cl-defstruct hydra_ext_scala_syntax_data_function params body)

(cl-defstruct hydra_ext_scala_syntax_data_poly_function tparams body)

(cl-defstruct hydra_ext_scala_syntax_data_partial_function cases)

(cl-defstruct hydra_ext_scala_syntax_data_while expr body)

(cl-defstruct hydra_ext_scala_syntax_data_do body expr)

(cl-defstruct hydra_ext_scala_syntax_data_for enums)

(cl-defstruct hydra_ext_scala_syntax_data_for_yield enums)

(cl-defstruct hydra_ext_scala_syntax_data_new init)

(cl-defstruct hydra_ext_scala_syntax_data_new_anonymous templ)

(cl-defstruct hydra_ext_scala_syntax_data_eta expr)

(cl-defstruct hydra_ext_scala_syntax_data_repeated expr)

(cl-defstruct hydra_ext_scala_syntax_data_param mods name decltpe default)

(defvar hydra_ext_scala_syntax_type-variants (list :ref :anonymous_name :apply :apply_infix :function_type :poly_function :implicit_function :tuple :with :and :or :refine :existential :annotate :lambda :macro :method :placeholder :by_name :repeated :var :typed_param :match))

(defvar hydra_ext_scala_syntax_type_ref-variants (list :name :select :project :singleton))

(cl-defstruct hydra_ext_scala_syntax_type_name value)

(cl-defstruct hydra_ext_scala_syntax_type_anonymous_name value)

(cl-defstruct hydra_ext_scala_syntax_type_select qual name)

(cl-defstruct hydra_ext_scala_syntax_type_project qual name)

(cl-defstruct hydra_ext_scala_syntax_type_singleton ref)

(cl-defstruct hydra_ext_scala_syntax_type_apply tpe args)

(cl-defstruct hydra_ext_scala_syntax_type_apply_infix lhs op rhs)

(defvar hydra_ext_scala_syntax_type_function_type-variants (list :function :context_function))

(cl-defstruct hydra_ext_scala_syntax_type_function params res)

(cl-defstruct hydra_ext_scala_syntax_type_poly_function tparams tpe)

(cl-defstruct hydra_ext_scala_syntax_type_context_function params res)

(cl-defstruct hydra_ext_scala_syntax_type_implicit_function params res)

(cl-defstruct hydra_ext_scala_syntax_type_tuple args)

(cl-defstruct hydra_ext_scala_syntax_type_with lhs rhs)

(cl-defstruct hydra_ext_scala_syntax_type_and lhs rhs)

(cl-defstruct hydra_ext_scala_syntax_type_or lhs rhs)

(cl-defstruct hydra_ext_scala_syntax_type_refine tpe stats)

(cl-defstruct hydra_ext_scala_syntax_type_existential tpe stats)

(cl-defstruct hydra_ext_scala_syntax_type_annotate tpe annots)

(cl-defstruct hydra_ext_scala_syntax_type_lambda tparams tpe)

(cl-defstruct hydra_ext_scala_syntax_type_macro body)

(cl-defstruct hydra_ext_scala_syntax_type_method paramss tpe)

(cl-defstruct hydra_ext_scala_syntax_type_placeholder bounds)

(cl-defstruct hydra_ext_scala_syntax_type_bounds lo hi)

(cl-defstruct hydra_ext_scala_syntax_type_by_name tpe)

(cl-defstruct hydra_ext_scala_syntax_type_repeated tpe)

(cl-defstruct hydra_ext_scala_syntax_type_var name)

(cl-defstruct hydra_ext_scala_syntax_type_typed_param name typ)

(cl-defstruct hydra_ext_scala_syntax_type_param mods name tparams tbounds vbounds cbounds)

(cl-defstruct hydra_ext_scala_syntax_type_match tpe cases)

(defvar hydra_ext_scala_syntax_pat-variants (list :var :wildcard :seq_wildcard :bind :alternative :tuple :repeated :extract :extract_infix :interpolate :xml :typed :macro :given))

(cl-defstruct hydra_ext_scala_syntax_pat_var name)

(cl-defstruct hydra_ext_scala_syntax_pat_bind lhs rhs)

(cl-defstruct hydra_ext_scala_syntax_pat_alternative lhs rhs)

(cl-defstruct hydra_ext_scala_syntax_pat_tuple args)

(cl-defstruct hydra_ext_scala_syntax_pat_repeated name)

(cl-defstruct hydra_ext_scala_syntax_pat_extract fun args)

(cl-defstruct hydra_ext_scala_syntax_pat_extract_infix lhs op rhs)

(cl-defstruct hydra_ext_scala_syntax_pat_interpolate prefix parts)

(cl-defstruct hydra_ext_scala_syntax_pat_xml parts args)

(cl-defstruct hydra_ext_scala_syntax_pat_typed lhs rhs)

(cl-defstruct hydra_ext_scala_syntax_pat_macro body)

(cl-defstruct hydra_ext_scala_syntax_pat_given tpe)

(defvar hydra_ext_scala_syntax_member-variants (list :term :type :term_param :type_param :self))

(defvar hydra_ext_scala_syntax_member_data-variants (list :pkg :object))

(cl-defstruct hydra_ext_scala_syntax_member_type name)

(defvar hydra_ext_scala_syntax_decl-variants (list :val :var :def :type :given))

(cl-defstruct hydra_ext_scala_syntax_decl_val mods pats decltpe)

(cl-defstruct hydra_ext_scala_syntax_decl_var mods pats decltpe)

(cl-defstruct hydra_ext_scala_syntax_decl_def mods name tparams paramss decltpe)

(cl-defstruct hydra_ext_scala_syntax_decl_type mods name tparams bounds)

(cl-defstruct hydra_ext_scala_syntax_decl_given mods name tparams sparams decltpe)

(defvar hydra_ext_scala_syntax_defn-variants (list :val :var :given :enum :enum_case :repeated_enum_case :given_alias :extension_group :def :macro :type :class :trait :object))

(cl-defstruct hydra_ext_scala_syntax_defn_val mods pats decltpe rhs)

(cl-defstruct hydra_ext_scala_syntax_defn_var mods pats decltpe rhs)

(cl-defstruct hydra_ext_scala_syntax_defn_given mods name tparams sparams templ)

(cl-defstruct hydra_ext_scala_syntax_defn_enum mods name tparams ctor template)

(cl-defstruct hydra_ext_scala_syntax_defn_enum_case mods name tparams ctor inits)

(cl-defstruct hydra_ext_scala_syntax_defn_repeated_enum_case mods cases)

(cl-defstruct hydra_ext_scala_syntax_defn_given_alias mods name tparams sparams decltpe body)

(cl-defstruct hydra_ext_scala_syntax_defn_extension_group tparams parmss body)

(cl-defstruct hydra_ext_scala_syntax_defn_def mods name tparams paramss decltpe body)

(cl-defstruct hydra_ext_scala_syntax_defn_macro mods name tparams paramss decltpe body)

(cl-defstruct hydra_ext_scala_syntax_defn_type mods name tparams body)

(cl-defstruct hydra_ext_scala_syntax_defn_class mods name tparams ctor template)

(cl-defstruct hydra_ext_scala_syntax_defn_trait mods name tparams ctor template)

(cl-defstruct hydra_ext_scala_syntax_defn_object name)

(cl-defstruct hydra_ext_scala_syntax_pkg name ref stats)

(cl-defstruct hydra_ext_scala_syntax_pkg_object mods name template)

(defvar hydra_ext_scala_syntax_ctor-variants (list :primary :secondary))

(cl-defstruct hydra_ext_scala_syntax_ctor_primary mods name paramss)

(cl-defstruct hydra_ext_scala_syntax_ctor_secondary mods name paramss init stats)

(cl-defstruct hydra_ext_scala_syntax_init tpe name argss)

(cl-defstruct hydra_ext_scala_syntax_self value)

(cl-defstruct hydra_ext_scala_syntax_template early inits self stats)

(defvar hydra_ext_scala_syntax_mod-variants (list :annot :private :protected :implicit :final :sealed :open :super :override :case :abstract :covariant :contravariant :lazy :val_param :var_param :infix :inline :using :opaque :transparent))

(cl-defstruct hydra_ext_scala_syntax_mod_annot init)

(cl-defstruct hydra_ext_scala_syntax_mod_private within)

(cl-defstruct hydra_ext_scala_syntax_mod_protected within)

(defvar hydra_ext_scala_syntax_enumerator-variants (list :generator :case_generator :val :guard))

(cl-defstruct hydra_ext_scala_syntax_enumerator_generator pat rhs)

(cl-defstruct hydra_ext_scala_syntax_enumerator_case_generator pat rhs)

(cl-defstruct hydra_ext_scala_syntax_enumerator_val pat rhs)

(cl-defstruct hydra_ext_scala_syntax_enumerator_guard cond)

(defvar hydra_ext_scala_syntax_import_export_stat-variants (list :import :export))

(cl-defstruct hydra_ext_scala_syntax_import importers)

(cl-defstruct hydra_ext_scala_syntax_export importers)

(cl-defstruct hydra_ext_scala_syntax_importer ref importees)

(defvar hydra_ext_scala_syntax_importee-variants (list :wildcard :given :given_all :name :rename :unimport))

(cl-defstruct hydra_ext_scala_syntax_importee_given tpe)

(cl-defstruct hydra_ext_scala_syntax_importee_name name)

(cl-defstruct hydra_ext_scala_syntax_importee_rename name rename)

(cl-defstruct hydra_ext_scala_syntax_importee_unimport name)

(defvar hydra_ext_scala_syntax_case_tree-variants (list :case :type_case))

(cl-defstruct hydra_ext_scala_syntax_case pat cond body)

(cl-defstruct hydra_ext_scala_syntax_type_case pat body)

(cl-defstruct hydra_ext_scala_syntax_source stats)

(cl-defstruct hydra_ext_scala_syntax_quasi value)

(provide 'hydra.ext.scala.syntax)
