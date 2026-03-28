(defpackage :hydra.packaging
(:use :cl :hydra.module)
(:export :make-hydra_packaging_package :hydra_packaging_package? :hydra_packaging_package-name :hydra_packaging_package-modules :hydra_packaging_package-dependencies :hydra_packaging_package-description :make-hydra_packaging_package_name :hydra_packaging_package_name? :hydra_packaging_package_name-value))

(in-package :hydra.packaging)

(cl:defstruct hydra_packaging_package name modules dependencies description)

(cl:defstruct hydra_packaging_package_name value)
