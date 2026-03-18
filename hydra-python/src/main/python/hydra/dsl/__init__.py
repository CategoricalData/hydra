# Make this a namespace package to merge with src/gen-main/python/hydra/dsl
from pkgutil import extend_path
__path__ = extend_path(__path__, __name__)
