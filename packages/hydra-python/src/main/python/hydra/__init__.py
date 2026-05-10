# Namespace package: merges with hydra package roots from heads/python and dist/python.
from pkgutil import extend_path
__path__ = extend_path(__path__, __name__)
