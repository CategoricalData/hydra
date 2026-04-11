(defpackage :hydra.json.model
(:use :cl)
(:export :hydra_json_model_value-variants))

(in-package :hydra.json.model)

(cl:defvar hydra_json_model_value-variants (cl:list :array :boolean :null :number :object :string))
