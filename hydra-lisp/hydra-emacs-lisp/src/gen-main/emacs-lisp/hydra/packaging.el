(require 'cl-lib)

(require 'hydra.module)

(cl-defstruct hydra_packaging_package name modules dependencies description)

(cl-defstruct hydra_packaging_package_name value)

(provide 'hydra.packaging)
