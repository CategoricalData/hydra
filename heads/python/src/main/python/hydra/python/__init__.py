# Make this a namespace package so heads-side hydra.python.util can coexist
# with dist-side kernel modules (hydra.python.coder, hydra.python.environment, etc.)
from pkgutil import extend_path
__path__ = extend_path(__path__, __name__)
