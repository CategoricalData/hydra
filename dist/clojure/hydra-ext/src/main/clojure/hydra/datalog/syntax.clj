(ns hydra.datalog.syntax)

(declare hydra_datalog_syntax_program_elmt-variants hydra_datalog_syntax_atom_list-variants hydra_datalog_syntax_term-variants hydra_datalog_syntax_term_list-variants hydra_datalog_syntax_constant_list-variants)

(defrecord hydra_datalog_syntax_constant [value])
(defn make-hydra_datalog_syntax_constant [value] (->hydra_datalog_syntax_constant value))

(defrecord hydra_datalog_syntax_relation [value])
(defn make-hydra_datalog_syntax_relation [value] (->hydra_datalog_syntax_relation value))

(defrecord hydra_datalog_syntax_variable [value])
(defn make-hydra_datalog_syntax_variable [value] (->hydra_datalog_syntax_variable value))

(defrecord hydra_datalog_syntax_program [value])
(defn make-hydra_datalog_syntax_program [value] (->hydra_datalog_syntax_program value))

(def hydra_datalog_syntax_program_elmt-variants (list :fact :rule))

(defrecord hydra_datalog_syntax_fact [relation constant_list])
(defn make-hydra_datalog_syntax_fact [relation constant_list] (->hydra_datalog_syntax_fact relation constant_list))

(defrecord hydra_datalog_syntax_rule [atom atom_list])
(defn make-hydra_datalog_syntax_rule [atom atom_list] (->hydra_datalog_syntax_rule atom atom_list))

(defrecord hydra_datalog_syntax_atom [relation term_list])
(defn make-hydra_datalog_syntax_atom [relation term_list] (->hydra_datalog_syntax_atom relation term_list))

(def hydra_datalog_syntax_atom_list-variants (list :single :multiple))

(defrecord hydra_datalog_syntax_atom_list_multiple [atom atom_list])
(defn make-hydra_datalog_syntax_atom_list_multiple [atom atom_list] (->hydra_datalog_syntax_atom_list_multiple atom atom_list))

(def hydra_datalog_syntax_term-variants (list :constant :variable))

(def hydra_datalog_syntax_term_list-variants (list :single :multiple))

(defrecord hydra_datalog_syntax_term_list_multiple [term term_list])
(defn make-hydra_datalog_syntax_term_list_multiple [term term_list] (->hydra_datalog_syntax_term_list_multiple term term_list))

(def hydra_datalog_syntax_constant_list-variants (list :single :multiple))

(defrecord hydra_datalog_syntax_constant_list_multiple [constant constant_list])
(defn make-hydra_datalog_syntax_constant_list_multiple [constant constant_list] (->hydra_datalog_syntax_constant_list_multiple constant constant_list))
