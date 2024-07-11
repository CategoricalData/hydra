// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class ServiceCall implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ServiceCall");
  
  public final hydra.langs.tinkerpop.gremlin.StringArgument service;
  
  public final hydra.langs.tinkerpop.gremlin.ServiceArguments arguments;
  
  public ServiceCall (hydra.langs.tinkerpop.gremlin.StringArgument service, hydra.langs.tinkerpop.gremlin.ServiceArguments arguments) {
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
  
  public ServiceCall withService(hydra.langs.tinkerpop.gremlin.StringArgument service) {
    java.util.Objects.requireNonNull((service));
    return new ServiceCall(service, arguments);
  }
  
  public ServiceCall withArguments(hydra.langs.tinkerpop.gremlin.ServiceArguments arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new ServiceCall(service, arguments);
  }
}