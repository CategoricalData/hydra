// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class ServiceCall implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.ServiceCall");
  
  public static final hydra.core.Name FIELD_NAME_SERVICE = new hydra.core.Name("service");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.tinkerpop.gremlin.StringArgument service;
  
  public final hydra.ext.tinkerpop.gremlin.ServiceArguments arguments;
  
  public ServiceCall (hydra.ext.tinkerpop.gremlin.StringArgument service, hydra.ext.tinkerpop.gremlin.ServiceArguments arguments) {
    java.util.Objects.requireNonNull((service));
    java.util.Objects.requireNonNull((arguments));
    this.service = service;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ServiceCall)) {
      return false;
    }
    ServiceCall o = (ServiceCall) (other);
    return service.equals(o.service) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * service.hashCode() + 3 * arguments.hashCode();
  }
  
  public ServiceCall withService(hydra.ext.tinkerpop.gremlin.StringArgument service) {
    java.util.Objects.requireNonNull((service));
    return new ServiceCall(service, arguments);
  }
  
  public ServiceCall withArguments(hydra.ext.tinkerpop.gremlin.ServiceArguments arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new ServiceCall(service, arguments);
  }
}
