// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class ServiceCall implements Serializable, Comparable<ServiceCall> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ServiceCall");
  
  public static final hydra.core.Name SERVICE = new hydra.core.Name("service");
  
  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument service;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.ServiceArguments arguments;
  
  public ServiceCall (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument service, hydra.ext.org.apache.tinkerpop.gremlin.ServiceArguments arguments) {
    this.service = service;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ServiceCall)) {
      return false;
    }
    ServiceCall o = (ServiceCall) other;
    return java.util.Objects.equals(
      this.service,
      o.service) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(service) + 3 * java.util.Objects.hashCode(arguments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ServiceCall other) {
    int cmp = 0;
    cmp = ((Comparable) service).compareTo(other.service);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) arguments).compareTo(other.arguments);
  }
  
  public ServiceCall withService(hydra.ext.org.apache.tinkerpop.gremlin.StringArgument service) {
    return new ServiceCall(service, arguments);
  }
  
  public ServiceCall withArguments(hydra.ext.org.apache.tinkerpop.gremlin.ServiceArguments arguments) {
    return new ServiceCall(service, arguments);
  }
}
