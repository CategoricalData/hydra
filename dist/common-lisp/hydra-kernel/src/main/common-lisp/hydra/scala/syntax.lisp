(defpackage :hydra.scala.syntax
(:use :cl)
(:export :make-hydra_scala_syntax_predef_string :hydra_scala_syntax_predef_string? :hydra_scala_syntax_predef_string-value :make-hydra_scala_syntax_scala_symbol :hydra_scala_syntax_scala_symbol? :hydra_scala_syntax_scala_symbol-name :hydra_scala_syntax_tree-variants :hydra_scala_syntax_ref-variants :hydra_scala_syntax_stat-variants :hydra_scala_syntax_name-variants :hydra_scala_syntax_lit-variants :hydra_scala_syntax_data-variants :hydra_scala_syntax_data_ref-variants :make-hydra_scala_syntax_data_this :hydra_scala_syntax_data_this? :hydra_scala_syntax_data_this-value :make-hydra_scala_syntax_data_super :hydra_scala_syntax_data_super? :hydra_scala_syntax_data_super-thisp :hydra_scala_syntax_data_super-superp :make-hydra_scala_syntax_data_name :hydra_scala_syntax_data_name? :hydra_scala_syntax_data_name-value :make-hydra_scala_syntax_data_anonymous :hydra_scala_syntax_data_anonymous? :hydra_scala_syntax_data_anonymous-value :make-hydra_scala_syntax_data_select :hydra_scala_syntax_data_select? :hydra_scala_syntax_data_select-qual :hydra_scala_syntax_data_select-name :make-hydra_scala_syntax_data_interpolate :hydra_scala_syntax_data_interpolate? :hydra_scala_syntax_data_interpolate-prefix :hydra_scala_syntax_data_interpolate-parts :hydra_scala_syntax_data_interpolate-args :make-hydra_scala_syntax_data_xml :hydra_scala_syntax_data_xml? :hydra_scala_syntax_data_xml-parts :hydra_scala_syntax_data_xml-args :make-hydra_scala_syntax_data_apply :hydra_scala_syntax_data_apply? :hydra_scala_syntax_data_apply-fun :hydra_scala_syntax_data_apply-args :make-hydra_scala_syntax_data_apply_using :hydra_scala_syntax_data_apply_using? :hydra_scala_syntax_data_apply_using-fun :hydra_scala_syntax_data_apply_using-targs :make-hydra_scala_syntax_data_apply_type :hydra_scala_syntax_data_apply_type? :hydra_scala_syntax_data_apply_type-lhs :hydra_scala_syntax_data_apply_type-op :hydra_scala_syntax_data_apply_type-targs :hydra_scala_syntax_data_apply_type-args :make-hydra_scala_syntax_data_apply_infix :hydra_scala_syntax_data_apply_infix? :hydra_scala_syntax_data_apply_infix-lhs :hydra_scala_syntax_data_apply_infix-op :hydra_scala_syntax_data_apply_infix-targs :hydra_scala_syntax_data_apply_infix-args :make-hydra_scala_syntax_data_apply_unary :hydra_scala_syntax_data_apply_unary? :hydra_scala_syntax_data_apply_unary-op :hydra_scala_syntax_data_apply_unary-arg :make-hydra_scala_syntax_data_assign :hydra_scala_syntax_data_assign? :hydra_scala_syntax_data_assign-lhs :hydra_scala_syntax_data_assign-rhs :make-hydra_scala_syntax_data_return :hydra_scala_syntax_data_return? :hydra_scala_syntax_data_return-expr :make-hydra_scala_syntax_data_throw :hydra_scala_syntax_data_throw? :hydra_scala_syntax_data_throw-expr :make-hydra_scala_syntax_data_ascribe :hydra_scala_syntax_data_ascribe? :hydra_scala_syntax_data_ascribe-expr :hydra_scala_syntax_data_ascribe-tpe :make-hydra_scala_syntax_data_annotate :hydra_scala_syntax_data_annotate? :hydra_scala_syntax_data_annotate-expr :hydra_scala_syntax_data_annotate-annots :make-hydra_scala_syntax_data_tuple :hydra_scala_syntax_data_tuple? :hydra_scala_syntax_data_tuple-args :make-hydra_scala_syntax_data_block :hydra_scala_syntax_data_block? :hydra_scala_syntax_data_block-stats :make-hydra_scala_syntax_data_end_marker :hydra_scala_syntax_data_end_marker? :hydra_scala_syntax_data_end_marker-name :make-hydra_scala_syntax_data_if :hydra_scala_syntax_data_if? :hydra_scala_syntax_data_if-cond :hydra_scala_syntax_data_if-thenp :hydra_scala_syntax_data_if-elsep :make-hydra_scala_syntax_data_quoted_macro_expr :hydra_scala_syntax_data_quoted_macro_expr? :hydra_scala_syntax_data_quoted_macro_expr-body :make-hydra_scala_syntax_data_quoted_macro_type :hydra_scala_syntax_data_quoted_macro_type? :hydra_scala_syntax_data_quoted_macro_type-tpe :make-hydra_scala_syntax_data_spliced_macro_expr :hydra_scala_syntax_data_spliced_macro_expr? :hydra_scala_syntax_data_spliced_macro_expr-body :make-hydra_scala_syntax_data_match :hydra_scala_syntax_data_match? :hydra_scala_syntax_data_match-expr :hydra_scala_syntax_data_match-cases :make-hydra_scala_syntax_data_try :hydra_scala_syntax_data_try? :hydra_scala_syntax_data_try-expr :hydra_scala_syntax_data_try-catchp :hydra_scala_syntax_data_try-finallyp :make-hydra_scala_syntax_data_try_with_handler :hydra_scala_syntax_data_try_with_handler? :hydra_scala_syntax_data_try_with_handler-expr :hydra_scala_syntax_data_try_with_handler-catchp :hydra_scala_syntax_data_try_with_handler-finallyp :hydra_scala_syntax_data_function_data-variants :make-hydra_scala_syntax_data_context_function :hydra_scala_syntax_data_context_function? :hydra_scala_syntax_data_context_function-params :hydra_scala_syntax_data_context_function-body :make-hydra_scala_syntax_data_function :hydra_scala_syntax_data_function? :hydra_scala_syntax_data_function-params :hydra_scala_syntax_data_function-body :make-hydra_scala_syntax_data_poly_function :hydra_scala_syntax_data_poly_function? :hydra_scala_syntax_data_poly_function-tparams :hydra_scala_syntax_data_poly_function-body :make-hydra_scala_syntax_data_partial_function :hydra_scala_syntax_data_partial_function? :hydra_scala_syntax_data_partial_function-cases :make-hydra_scala_syntax_data_while :hydra_scala_syntax_data_while? :hydra_scala_syntax_data_while-expr :hydra_scala_syntax_data_while-body :make-hydra_scala_syntax_data_do :hydra_scala_syntax_data_do? :hydra_scala_syntax_data_do-body :hydra_scala_syntax_data_do-expr :make-hydra_scala_syntax_data_for :hydra_scala_syntax_data_for? :hydra_scala_syntax_data_for-enums :make-hydra_scala_syntax_data_for_yield :hydra_scala_syntax_data_for_yield? :hydra_scala_syntax_data_for_yield-enums :make-hydra_scala_syntax_data_new :hydra_scala_syntax_data_new? :hydra_scala_syntax_data_new-init :make-hydra_scala_syntax_data_new_anonymous :hydra_scala_syntax_data_new_anonymous? :hydra_scala_syntax_data_new_anonymous-templ :make-hydra_scala_syntax_data_eta :hydra_scala_syntax_data_eta? :hydra_scala_syntax_data_eta-expr :make-hydra_scala_syntax_data_repeated :hydra_scala_syntax_data_repeated? :hydra_scala_syntax_data_repeated-expr :make-hydra_scala_syntax_data_param :hydra_scala_syntax_data_param? :hydra_scala_syntax_data_param-mods :hydra_scala_syntax_data_param-name :hydra_scala_syntax_data_param-decltpe :hydra_scala_syntax_data_param-default :hydra_scala_syntax_type-variants :hydra_scala_syntax_type_ref-variants :make-hydra_scala_syntax_type_name :hydra_scala_syntax_type_name? :hydra_scala_syntax_type_name-value :make-hydra_scala_syntax_type_anonymous_name :hydra_scala_syntax_type_anonymous_name? :hydra_scala_syntax_type_anonymous_name-value :make-hydra_scala_syntax_type_select :hydra_scala_syntax_type_select? :hydra_scala_syntax_type_select-qual :hydra_scala_syntax_type_select-name :make-hydra_scala_syntax_type_project :hydra_scala_syntax_type_project? :hydra_scala_syntax_type_project-qual :hydra_scala_syntax_type_project-name :make-hydra_scala_syntax_type_singleton :hydra_scala_syntax_type_singleton? :hydra_scala_syntax_type_singleton-ref :make-hydra_scala_syntax_type_apply :hydra_scala_syntax_type_apply? :hydra_scala_syntax_type_apply-tpe :hydra_scala_syntax_type_apply-args :make-hydra_scala_syntax_type_apply_infix :hydra_scala_syntax_type_apply_infix? :hydra_scala_syntax_type_apply_infix-lhs :hydra_scala_syntax_type_apply_infix-op :hydra_scala_syntax_type_apply_infix-rhs :hydra_scala_syntax_type_function_type-variants :make-hydra_scala_syntax_type_function :hydra_scala_syntax_type_function? :hydra_scala_syntax_type_function-params :hydra_scala_syntax_type_function-res :make-hydra_scala_syntax_type_poly_function :hydra_scala_syntax_type_poly_function? :hydra_scala_syntax_type_poly_function-tparams :hydra_scala_syntax_type_poly_function-tpe :make-hydra_scala_syntax_type_context_function :hydra_scala_syntax_type_context_function? :hydra_scala_syntax_type_context_function-params :hydra_scala_syntax_type_context_function-res :make-hydra_scala_syntax_type_implicit_function :hydra_scala_syntax_type_implicit_function? :hydra_scala_syntax_type_implicit_function-params :hydra_scala_syntax_type_implicit_function-res :make-hydra_scala_syntax_type_tuple :hydra_scala_syntax_type_tuple? :hydra_scala_syntax_type_tuple-args :make-hydra_scala_syntax_type_with :hydra_scala_syntax_type_with? :hydra_scala_syntax_type_with-lhs :hydra_scala_syntax_type_with-rhs :make-hydra_scala_syntax_type_and :hydra_scala_syntax_type_and? :hydra_scala_syntax_type_and-lhs :hydra_scala_syntax_type_and-rhs :make-hydra_scala_syntax_type_or :hydra_scala_syntax_type_or? :hydra_scala_syntax_type_or-lhs :hydra_scala_syntax_type_or-rhs :make-hydra_scala_syntax_type_refine :hydra_scala_syntax_type_refine? :hydra_scala_syntax_type_refine-tpe :hydra_scala_syntax_type_refine-stats :make-hydra_scala_syntax_type_existential :hydra_scala_syntax_type_existential? :hydra_scala_syntax_type_existential-tpe :hydra_scala_syntax_type_existential-stats :make-hydra_scala_syntax_type_annotate :hydra_scala_syntax_type_annotate? :hydra_scala_syntax_type_annotate-tpe :hydra_scala_syntax_type_annotate-annots :make-hydra_scala_syntax_type_lambda :hydra_scala_syntax_type_lambda? :hydra_scala_syntax_type_lambda-tparams :hydra_scala_syntax_type_lambda-tpe :make-hydra_scala_syntax_type_macro :hydra_scala_syntax_type_macro? :hydra_scala_syntax_type_macro-body :make-hydra_scala_syntax_type_method :hydra_scala_syntax_type_method? :hydra_scala_syntax_type_method-paramss :hydra_scala_syntax_type_method-tpe :make-hydra_scala_syntax_type_placeholder :hydra_scala_syntax_type_placeholder? :hydra_scala_syntax_type_placeholder-bounds :make-hydra_scala_syntax_type_bounds :hydra_scala_syntax_type_bounds? :hydra_scala_syntax_type_bounds-lo :hydra_scala_syntax_type_bounds-hi :make-hydra_scala_syntax_type_by_name :hydra_scala_syntax_type_by_name? :hydra_scala_syntax_type_by_name-tpe :make-hydra_scala_syntax_type_repeated :hydra_scala_syntax_type_repeated? :hydra_scala_syntax_type_repeated-tpe :make-hydra_scala_syntax_type_var :hydra_scala_syntax_type_var? :hydra_scala_syntax_type_var-name :make-hydra_scala_syntax_type_typed_param :hydra_scala_syntax_type_typed_param? :hydra_scala_syntax_type_typed_param-name :hydra_scala_syntax_type_typed_param-typ :make-hydra_scala_syntax_type_param :hydra_scala_syntax_type_param? :hydra_scala_syntax_type_param-mods :hydra_scala_syntax_type_param-name :hydra_scala_syntax_type_param-tparams :hydra_scala_syntax_type_param-tbounds :hydra_scala_syntax_type_param-vbounds :hydra_scala_syntax_type_param-cbounds :make-hydra_scala_syntax_type_match :hydra_scala_syntax_type_match? :hydra_scala_syntax_type_match-tpe :hydra_scala_syntax_type_match-cases :hydra_scala_syntax_pat-variants :make-hydra_scala_syntax_pat_var :hydra_scala_syntax_pat_var? :hydra_scala_syntax_pat_var-name :make-hydra_scala_syntax_pat_bind :hydra_scala_syntax_pat_bind? :hydra_scala_syntax_pat_bind-lhs :hydra_scala_syntax_pat_bind-rhs :make-hydra_scala_syntax_pat_alternative :hydra_scala_syntax_pat_alternative? :hydra_scala_syntax_pat_alternative-lhs :hydra_scala_syntax_pat_alternative-rhs :make-hydra_scala_syntax_pat_tuple :hydra_scala_syntax_pat_tuple? :hydra_scala_syntax_pat_tuple-args :make-hydra_scala_syntax_pat_repeated :hydra_scala_syntax_pat_repeated? :hydra_scala_syntax_pat_repeated-name :make-hydra_scala_syntax_pat_extract :hydra_scala_syntax_pat_extract? :hydra_scala_syntax_pat_extract-fun :hydra_scala_syntax_pat_extract-args :make-hydra_scala_syntax_pat_extract_infix :hydra_scala_syntax_pat_extract_infix? :hydra_scala_syntax_pat_extract_infix-lhs :hydra_scala_syntax_pat_extract_infix-op :hydra_scala_syntax_pat_extract_infix-rhs :make-hydra_scala_syntax_pat_interpolate :hydra_scala_syntax_pat_interpolate? :hydra_scala_syntax_pat_interpolate-prefix :hydra_scala_syntax_pat_interpolate-parts :make-hydra_scala_syntax_pat_xml :hydra_scala_syntax_pat_xml? :hydra_scala_syntax_pat_xml-parts :hydra_scala_syntax_pat_xml-args :make-hydra_scala_syntax_pat_typed :hydra_scala_syntax_pat_typed? :hydra_scala_syntax_pat_typed-lhs :hydra_scala_syntax_pat_typed-rhs :make-hydra_scala_syntax_pat_macro :hydra_scala_syntax_pat_macro? :hydra_scala_syntax_pat_macro-body :make-hydra_scala_syntax_pat_given :hydra_scala_syntax_pat_given? :hydra_scala_syntax_pat_given-tpe :hydra_scala_syntax_member-variants :hydra_scala_syntax_member_data-variants :make-hydra_scala_syntax_member_type :hydra_scala_syntax_member_type? :hydra_scala_syntax_member_type-name :hydra_scala_syntax_decl-variants :make-hydra_scala_syntax_decl_val :hydra_scala_syntax_decl_val? :hydra_scala_syntax_decl_val-mods :hydra_scala_syntax_decl_val-pats :hydra_scala_syntax_decl_val-decltpe :make-hydra_scala_syntax_decl_var :hydra_scala_syntax_decl_var? :hydra_scala_syntax_decl_var-mods :hydra_scala_syntax_decl_var-pats :hydra_scala_syntax_decl_var-decltpe :make-hydra_scala_syntax_decl_def :hydra_scala_syntax_decl_def? :hydra_scala_syntax_decl_def-mods :hydra_scala_syntax_decl_def-name :hydra_scala_syntax_decl_def-tparams :hydra_scala_syntax_decl_def-paramss :hydra_scala_syntax_decl_def-decltpe :make-hydra_scala_syntax_decl_type :hydra_scala_syntax_decl_type? :hydra_scala_syntax_decl_type-mods :hydra_scala_syntax_decl_type-name :hydra_scala_syntax_decl_type-tparams :hydra_scala_syntax_decl_type-bounds :make-hydra_scala_syntax_decl_given :hydra_scala_syntax_decl_given? :hydra_scala_syntax_decl_given-mods :hydra_scala_syntax_decl_given-name :hydra_scala_syntax_decl_given-tparams :hydra_scala_syntax_decl_given-sparams :hydra_scala_syntax_decl_given-decltpe :hydra_scala_syntax_defn-variants :make-hydra_scala_syntax_defn_val :hydra_scala_syntax_defn_val? :hydra_scala_syntax_defn_val-mods :hydra_scala_syntax_defn_val-pats :hydra_scala_syntax_defn_val-decltpe :hydra_scala_syntax_defn_val-rhs :make-hydra_scala_syntax_defn_var :hydra_scala_syntax_defn_var? :hydra_scala_syntax_defn_var-mods :hydra_scala_syntax_defn_var-pats :hydra_scala_syntax_defn_var-decltpe :hydra_scala_syntax_defn_var-rhs :make-hydra_scala_syntax_defn_given :hydra_scala_syntax_defn_given? :hydra_scala_syntax_defn_given-mods :hydra_scala_syntax_defn_given-name :hydra_scala_syntax_defn_given-tparams :hydra_scala_syntax_defn_given-sparams :hydra_scala_syntax_defn_given-templ :make-hydra_scala_syntax_defn_enum :hydra_scala_syntax_defn_enum? :hydra_scala_syntax_defn_enum-mods :hydra_scala_syntax_defn_enum-name :hydra_scala_syntax_defn_enum-tparams :hydra_scala_syntax_defn_enum-ctor :hydra_scala_syntax_defn_enum-template :make-hydra_scala_syntax_defn_enum_case :hydra_scala_syntax_defn_enum_case? :hydra_scala_syntax_defn_enum_case-mods :hydra_scala_syntax_defn_enum_case-name :hydra_scala_syntax_defn_enum_case-tparams :hydra_scala_syntax_defn_enum_case-ctor :hydra_scala_syntax_defn_enum_case-inits :make-hydra_scala_syntax_defn_repeated_enum_case :hydra_scala_syntax_defn_repeated_enum_case? :hydra_scala_syntax_defn_repeated_enum_case-mods :hydra_scala_syntax_defn_repeated_enum_case-cases :make-hydra_scala_syntax_defn_given_alias :hydra_scala_syntax_defn_given_alias? :hydra_scala_syntax_defn_given_alias-mods :hydra_scala_syntax_defn_given_alias-name :hydra_scala_syntax_defn_given_alias-tparams :hydra_scala_syntax_defn_given_alias-sparams :hydra_scala_syntax_defn_given_alias-decltpe :hydra_scala_syntax_defn_given_alias-body :make-hydra_scala_syntax_defn_extension_group :hydra_scala_syntax_defn_extension_group? :hydra_scala_syntax_defn_extension_group-tparams :hydra_scala_syntax_defn_extension_group-parmss :hydra_scala_syntax_defn_extension_group-body :make-hydra_scala_syntax_defn_def :hydra_scala_syntax_defn_def? :hydra_scala_syntax_defn_def-mods :hydra_scala_syntax_defn_def-name :hydra_scala_syntax_defn_def-tparams :hydra_scala_syntax_defn_def-paramss :hydra_scala_syntax_defn_def-decltpe :hydra_scala_syntax_defn_def-body :make-hydra_scala_syntax_defn_macro :hydra_scala_syntax_defn_macro? :hydra_scala_syntax_defn_macro-mods :hydra_scala_syntax_defn_macro-name :hydra_scala_syntax_defn_macro-tparams :hydra_scala_syntax_defn_macro-paramss :hydra_scala_syntax_defn_macro-decltpe :hydra_scala_syntax_defn_macro-body :make-hydra_scala_syntax_defn_type :hydra_scala_syntax_defn_type? :hydra_scala_syntax_defn_type-mods :hydra_scala_syntax_defn_type-name :hydra_scala_syntax_defn_type-tparams :hydra_scala_syntax_defn_type-body :make-hydra_scala_syntax_defn_class :hydra_scala_syntax_defn_class? :hydra_scala_syntax_defn_class-mods :hydra_scala_syntax_defn_class-name :hydra_scala_syntax_defn_class-tparams :hydra_scala_syntax_defn_class-ctor :hydra_scala_syntax_defn_class-template :make-hydra_scala_syntax_defn_trait :hydra_scala_syntax_defn_trait? :hydra_scala_syntax_defn_trait-mods :hydra_scala_syntax_defn_trait-name :hydra_scala_syntax_defn_trait-tparams :hydra_scala_syntax_defn_trait-ctor :hydra_scala_syntax_defn_trait-template :make-hydra_scala_syntax_defn_object :hydra_scala_syntax_defn_object? :hydra_scala_syntax_defn_object-name :make-hydra_scala_syntax_pkg :hydra_scala_syntax_pkg? :hydra_scala_syntax_pkg-name :hydra_scala_syntax_pkg-ref :hydra_scala_syntax_pkg-stats :make-hydra_scala_syntax_pkg_object :hydra_scala_syntax_pkg_object? :hydra_scala_syntax_pkg_object-mods :hydra_scala_syntax_pkg_object-name :hydra_scala_syntax_pkg_object-template :hydra_scala_syntax_ctor-variants :make-hydra_scala_syntax_ctor_primary :hydra_scala_syntax_ctor_primary? :hydra_scala_syntax_ctor_primary-mods :hydra_scala_syntax_ctor_primary-name :hydra_scala_syntax_ctor_primary-paramss :make-hydra_scala_syntax_ctor_secondary :hydra_scala_syntax_ctor_secondary? :hydra_scala_syntax_ctor_secondary-mods :hydra_scala_syntax_ctor_secondary-name :hydra_scala_syntax_ctor_secondary-paramss :hydra_scala_syntax_ctor_secondary-init :hydra_scala_syntax_ctor_secondary-stats :make-hydra_scala_syntax_init :hydra_scala_syntax_init? :hydra_scala_syntax_init-tpe :hydra_scala_syntax_init-name :hydra_scala_syntax_init-argss :make-hydra_scala_syntax_self :hydra_scala_syntax_self? :hydra_scala_syntax_self-value :make-hydra_scala_syntax_template :hydra_scala_syntax_template? :hydra_scala_syntax_template-early :hydra_scala_syntax_template-inits :hydra_scala_syntax_template-self :hydra_scala_syntax_template-stats :hydra_scala_syntax_mod-variants :make-hydra_scala_syntax_mod_annot :hydra_scala_syntax_mod_annot? :hydra_scala_syntax_mod_annot-init :make-hydra_scala_syntax_mod_private :hydra_scala_syntax_mod_private? :hydra_scala_syntax_mod_private-within :make-hydra_scala_syntax_mod_protected :hydra_scala_syntax_mod_protected? :hydra_scala_syntax_mod_protected-within :hydra_scala_syntax_enumerator-variants :make-hydra_scala_syntax_enumerator_generator :hydra_scala_syntax_enumerator_generator? :hydra_scala_syntax_enumerator_generator-pat :hydra_scala_syntax_enumerator_generator-rhs :make-hydra_scala_syntax_enumerator_case_generator :hydra_scala_syntax_enumerator_case_generator? :hydra_scala_syntax_enumerator_case_generator-pat :hydra_scala_syntax_enumerator_case_generator-rhs :make-hydra_scala_syntax_enumerator_val :hydra_scala_syntax_enumerator_val? :hydra_scala_syntax_enumerator_val-pat :hydra_scala_syntax_enumerator_val-rhs :make-hydra_scala_syntax_enumerator_guard :hydra_scala_syntax_enumerator_guard? :hydra_scala_syntax_enumerator_guard-cond :hydra_scala_syntax_import_export_stat-variants :make-hydra_scala_syntax_import :hydra_scala_syntax_import? :hydra_scala_syntax_import-importers :make-hydra_scala_syntax_export :hydra_scala_syntax_export? :hydra_scala_syntax_export-importers :make-hydra_scala_syntax_importer :hydra_scala_syntax_importer? :hydra_scala_syntax_importer-ref :hydra_scala_syntax_importer-importees :hydra_scala_syntax_importee-variants :make-hydra_scala_syntax_importee_given :hydra_scala_syntax_importee_given? :hydra_scala_syntax_importee_given-tpe :make-hydra_scala_syntax_importee_name :hydra_scala_syntax_importee_name? :hydra_scala_syntax_importee_name-name :make-hydra_scala_syntax_importee_rename :hydra_scala_syntax_importee_rename? :hydra_scala_syntax_importee_rename-name :hydra_scala_syntax_importee_rename-rename :make-hydra_scala_syntax_importee_unimport :hydra_scala_syntax_importee_unimport? :hydra_scala_syntax_importee_unimport-name :hydra_scala_syntax_case_tree-variants :make-hydra_scala_syntax_case :hydra_scala_syntax_case? :hydra_scala_syntax_case-pat :hydra_scala_syntax_case-cond :hydra_scala_syntax_case-body :make-hydra_scala_syntax_type_case :hydra_scala_syntax_type_case? :hydra_scala_syntax_type_case-pat :hydra_scala_syntax_type_case-body :make-hydra_scala_syntax_source :hydra_scala_syntax_source? :hydra_scala_syntax_source-stats :make-hydra_scala_syntax_quasi :hydra_scala_syntax_quasi? :hydra_scala_syntax_quasi-value))

(in-package :hydra.scala.syntax)

(cl:defstruct hydra_scala_syntax_predef_string value)

(cl:defstruct hydra_scala_syntax_scala_symbol name)

(cl:defvar hydra_scala_syntax_tree-variants (cl:list :ref :stat :type :bounds :pat :member :ctor :template :mod :enumerator :importer :importee :case_tree :source :quasi))

(cl:defvar hydra_scala_syntax_ref-variants (cl:list :name :init))

(cl:defvar hydra_scala_syntax_stat-variants (cl:list :term :decl :defn :import_export))

(cl:defvar hydra_scala_syntax_name-variants (cl:list :value :anonymous :indeterminate))

(cl:defvar hydra_scala_syntax_lit-variants (cl:list :null :int :double :float :byte :short :char :long :boolean :unit :string :bytes :symbol))

(cl:defvar hydra_scala_syntax_data-variants (cl:list :lit :ref :interpolate :xml :apply :apply_using :apply_type :assign :return :throw :ascribe :annotate :tuple :block :end_marker :if :quoted_macro_expr :quoted_macro_type :spliced_macro_expr :match :try :try_with_handler :function_data :poly_function :partial_function :while :do :for :for_yield :new :new_anonymous :placeholder :eta :repeated :param))

(cl:defvar hydra_scala_syntax_data_ref-variants (cl:list :this :super :name :anonymous :select :apply_unary))

(cl:defstruct hydra_scala_syntax_data_this value)

(cl:defstruct hydra_scala_syntax_data_super thisp superp)

(cl:defstruct hydra_scala_syntax_data_name value)

(cl:defstruct hydra_scala_syntax_data_anonymous value)

(cl:defstruct hydra_scala_syntax_data_select qual name)

(cl:defstruct hydra_scala_syntax_data_interpolate prefix parts args)

(cl:defstruct hydra_scala_syntax_data_xml parts args)

(cl:defstruct hydra_scala_syntax_data_apply fun args)

(cl:defstruct hydra_scala_syntax_data_apply_using fun targs)

(cl:defstruct hydra_scala_syntax_data_apply_type lhs op targs args)

(cl:defstruct hydra_scala_syntax_data_apply_infix lhs op targs args)

(cl:defstruct hydra_scala_syntax_data_apply_unary op arg)

(cl:defstruct hydra_scala_syntax_data_assign lhs rhs)

(cl:defstruct hydra_scala_syntax_data_return expr)

(cl:defstruct hydra_scala_syntax_data_throw expr)

(cl:defstruct hydra_scala_syntax_data_ascribe expr tpe)

(cl:defstruct hydra_scala_syntax_data_annotate expr annots)

(cl:defstruct hydra_scala_syntax_data_tuple args)

(cl:defstruct hydra_scala_syntax_data_block stats)

(cl:defstruct hydra_scala_syntax_data_end_marker name)

(cl:defstruct hydra_scala_syntax_data_if cond thenp elsep)

(cl:defstruct hydra_scala_syntax_data_quoted_macro_expr body)

(cl:defstruct hydra_scala_syntax_data_quoted_macro_type tpe)

(cl:defstruct hydra_scala_syntax_data_spliced_macro_expr body)

(cl:defstruct hydra_scala_syntax_data_match expr cases)

(cl:defstruct hydra_scala_syntax_data_try expr catchp finallyp)

(cl:defstruct hydra_scala_syntax_data_try_with_handler expr catchp finallyp)

(cl:defvar hydra_scala_syntax_data_function_data-variants (cl:list :context_function :function))

(cl:defstruct hydra_scala_syntax_data_context_function params body)

(cl:defstruct hydra_scala_syntax_data_function params body)

(cl:defstruct hydra_scala_syntax_data_poly_function tparams body)

(cl:defstruct hydra_scala_syntax_data_partial_function cases)

(cl:defstruct hydra_scala_syntax_data_while expr body)

(cl:defstruct hydra_scala_syntax_data_do body expr)

(cl:defstruct hydra_scala_syntax_data_for enums)

(cl:defstruct hydra_scala_syntax_data_for_yield enums)

(cl:defstruct hydra_scala_syntax_data_new init)

(cl:defstruct hydra_scala_syntax_data_new_anonymous templ)

(cl:defstruct hydra_scala_syntax_data_eta expr)

(cl:defstruct hydra_scala_syntax_data_repeated expr)

(cl:defstruct hydra_scala_syntax_data_param mods name decltpe default)

(cl:defvar hydra_scala_syntax_type-variants (cl:list :ref :anonymous_name :apply :apply_infix :function_type :poly_function :implicit_function :tuple :with :and :or :refine :existential :annotate :lambda :macro :method :placeholder :by_name :repeated :var :typed_param :match))

(cl:defvar hydra_scala_syntax_type_ref-variants (cl:list :name :select :project :singleton))

(cl:defstruct hydra_scala_syntax_type_name value)

(cl:defstruct hydra_scala_syntax_type_anonymous_name value)

(cl:defstruct hydra_scala_syntax_type_select qual name)

(cl:defstruct hydra_scala_syntax_type_project qual name)

(cl:defstruct hydra_scala_syntax_type_singleton ref)

(cl:defstruct hydra_scala_syntax_type_apply tpe args)

(cl:defstruct hydra_scala_syntax_type_apply_infix lhs op rhs)

(cl:defvar hydra_scala_syntax_type_function_type-variants (cl:list :function :context_function))

(cl:defstruct hydra_scala_syntax_type_function params res)

(cl:defstruct hydra_scala_syntax_type_poly_function tparams tpe)

(cl:defstruct hydra_scala_syntax_type_context_function params res)

(cl:defstruct hydra_scala_syntax_type_implicit_function params res)

(cl:defstruct hydra_scala_syntax_type_tuple args)

(cl:defstruct hydra_scala_syntax_type_with lhs rhs)

(cl:defstruct hydra_scala_syntax_type_and lhs rhs)

(cl:defstruct hydra_scala_syntax_type_or lhs rhs)

(cl:defstruct hydra_scala_syntax_type_refine tpe stats)

(cl:defstruct hydra_scala_syntax_type_existential tpe stats)

(cl:defstruct hydra_scala_syntax_type_annotate tpe annots)

(cl:defstruct hydra_scala_syntax_type_lambda tparams tpe)

(cl:defstruct hydra_scala_syntax_type_macro body)

(cl:defstruct hydra_scala_syntax_type_method paramss tpe)

(cl:defstruct hydra_scala_syntax_type_placeholder bounds)

(cl:defstruct hydra_scala_syntax_type_bounds lo hi)

(cl:defstruct hydra_scala_syntax_type_by_name tpe)

(cl:defstruct hydra_scala_syntax_type_repeated tpe)

(cl:defstruct hydra_scala_syntax_type_var name)

(cl:defstruct hydra_scala_syntax_type_typed_param name typ)

(cl:defstruct hydra_scala_syntax_type_param mods name tparams tbounds vbounds cbounds)

(cl:defstruct hydra_scala_syntax_type_match tpe cases)

(cl:defvar hydra_scala_syntax_pat-variants (cl:list :var :wildcard :seq_wildcard :bind :alternative :tuple :repeated :extract :extract_infix :interpolate :xml :typed :macro :given))

(cl:defstruct hydra_scala_syntax_pat_var name)

(cl:defstruct hydra_scala_syntax_pat_bind lhs rhs)

(cl:defstruct hydra_scala_syntax_pat_alternative lhs rhs)

(cl:defstruct hydra_scala_syntax_pat_tuple args)

(cl:defstruct hydra_scala_syntax_pat_repeated name)

(cl:defstruct hydra_scala_syntax_pat_extract fun args)

(cl:defstruct hydra_scala_syntax_pat_extract_infix lhs op rhs)

(cl:defstruct hydra_scala_syntax_pat_interpolate prefix parts)

(cl:defstruct hydra_scala_syntax_pat_xml parts args)

(cl:defstruct hydra_scala_syntax_pat_typed lhs rhs)

(cl:defstruct hydra_scala_syntax_pat_macro body)

(cl:defstruct hydra_scala_syntax_pat_given tpe)

(cl:defvar hydra_scala_syntax_member-variants (cl:list :term :type :term_param :type_param :self))

(cl:defvar hydra_scala_syntax_member_data-variants (cl:list :pkg :object))

(cl:defstruct hydra_scala_syntax_member_type name)

(cl:defvar hydra_scala_syntax_decl-variants (cl:list :val :var :def :type :given))

(cl:defstruct hydra_scala_syntax_decl_val mods pats decltpe)

(cl:defstruct hydra_scala_syntax_decl_var mods pats decltpe)

(cl:defstruct hydra_scala_syntax_decl_def mods name tparams paramss decltpe)

(cl:defstruct hydra_scala_syntax_decl_type mods name tparams bounds)

(cl:defstruct hydra_scala_syntax_decl_given mods name tparams sparams decltpe)

(cl:defvar hydra_scala_syntax_defn-variants (cl:list :val :var :given :enum :enum_case :repeated_enum_case :given_alias :extension_group :def :macro :type :class :trait :object))

(cl:defstruct hydra_scala_syntax_defn_val mods pats decltpe rhs)

(cl:defstruct hydra_scala_syntax_defn_var mods pats decltpe rhs)

(cl:defstruct hydra_scala_syntax_defn_given mods name tparams sparams templ)

(cl:defstruct hydra_scala_syntax_defn_enum mods name tparams ctor template)

(cl:defstruct hydra_scala_syntax_defn_enum_case mods name tparams ctor inits)

(cl:defstruct hydra_scala_syntax_defn_repeated_enum_case mods cases)

(cl:defstruct hydra_scala_syntax_defn_given_alias mods name tparams sparams decltpe body)

(cl:defstruct hydra_scala_syntax_defn_extension_group tparams parmss body)

(cl:defstruct hydra_scala_syntax_defn_def mods name tparams paramss decltpe body)

(cl:defstruct hydra_scala_syntax_defn_macro mods name tparams paramss decltpe body)

(cl:defstruct hydra_scala_syntax_defn_type mods name tparams body)

(cl:defstruct hydra_scala_syntax_defn_class mods name tparams ctor template)

(cl:defstruct hydra_scala_syntax_defn_trait mods name tparams ctor template)

(cl:defstruct hydra_scala_syntax_defn_object name)

(cl:defstruct hydra_scala_syntax_pkg name ref stats)

(cl:defstruct hydra_scala_syntax_pkg_object mods name template)

(cl:defvar hydra_scala_syntax_ctor-variants (cl:list :primary :secondary))

(cl:defstruct hydra_scala_syntax_ctor_primary mods name paramss)

(cl:defstruct hydra_scala_syntax_ctor_secondary mods name paramss init stats)

(cl:defstruct hydra_scala_syntax_init tpe name argss)

(cl:defstruct hydra_scala_syntax_self value)

(cl:defstruct hydra_scala_syntax_template early inits self stats)

(cl:defvar hydra_scala_syntax_mod-variants (cl:list :annot :private :protected :implicit :final :sealed :open :super :override :case :abstract :covariant :contravariant :lazy :val_param :var_param :infix :inline :using :opaque :transparent))

(cl:defstruct hydra_scala_syntax_mod_annot init)

(cl:defstruct hydra_scala_syntax_mod_private within)

(cl:defstruct hydra_scala_syntax_mod_protected within)

(cl:defvar hydra_scala_syntax_enumerator-variants (cl:list :generator :case_generator :val :guard))

(cl:defstruct hydra_scala_syntax_enumerator_generator pat rhs)

(cl:defstruct hydra_scala_syntax_enumerator_case_generator pat rhs)

(cl:defstruct hydra_scala_syntax_enumerator_val pat rhs)

(cl:defstruct hydra_scala_syntax_enumerator_guard cond)

(cl:defvar hydra_scala_syntax_import_export_stat-variants (cl:list :import :export))

(cl:defstruct hydra_scala_syntax_import importers)

(cl:defstruct hydra_scala_syntax_export importers)

(cl:defstruct hydra_scala_syntax_importer ref importees)

(cl:defvar hydra_scala_syntax_importee-variants (cl:list :wildcard :given :given_all :name :rename :unimport))

(cl:defstruct hydra_scala_syntax_importee_given tpe)

(cl:defstruct hydra_scala_syntax_importee_name name)

(cl:defstruct hydra_scala_syntax_importee_rename name rename)

(cl:defstruct hydra_scala_syntax_importee_unimport name)

(cl:defvar hydra_scala_syntax_case_tree-variants (cl:list :case :type_case))

(cl:defstruct hydra_scala_syntax_case pat cond body)

(cl:defstruct hydra_scala_syntax_type_case pat body)

(cl:defstruct hydra_scala_syntax_source stats)

(cl:defstruct hydra_scala_syntax_quasi value)
