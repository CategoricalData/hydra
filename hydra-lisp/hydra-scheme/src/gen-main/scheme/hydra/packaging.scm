(define-library (hydra packaging)
(export make-hydra_packaging_package hydra_packaging_package? hydra_packaging_package-name hydra_packaging_package-modules hydra_packaging_package-dependencies hydra_packaging_package-description make-hydra_packaging_package_name hydra_packaging_package_name? hydra_packaging_package_name-value)
(import (scheme base) (hydra module))
(begin
(define-record-type hydra_packaging_package (make-hydra_packaging_package name modules dependencies description) hydra_packaging_package? (name hydra_packaging_package-name) (modules hydra_packaging_package-modules) (dependencies hydra_packaging_package-dependencies) (description hydra_packaging_package-description))
(define-record-type hydra_packaging_package_name (make-hydra_packaging_package_name value) hydra_packaging_package_name? (value hydra_packaging_package_name-value))))
