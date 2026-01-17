# Namespace package - extends path to include src/gen-main/python
from pkgutil import extend_path
__path__ = extend_path(__path__, __name__)
