// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * A Command describes a function or operation that can be performed on any digital twin.
 */
public class Command implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.azure.dtld.Command");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  public static final hydra.core.Name FIELD_NAME_COMMAND_TYPE = new hydra.core.Name("commandType");
  
  public static final hydra.core.Name FIELD_NAME_REQUEST = new hydra.core.Name("request");
  
  public static final hydra.core.Name FIELD_NAME_RESPONSE = new hydra.core.Name("response");
  
  /**
   * This must be 'Command'
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * The 'programming' name of the command. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$.
   */
  public final String name;
  
  /**
   * The ID of the command. If no @id is provided, the digital twin interface processor will assign one.
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id;
  
  /**
   * A comment for model authors
   */
  public final hydra.util.Opt<String> comment;
  
  /**
   * A localizable description for display
   */
  public final hydra.util.Opt<String> description;
  
  /**
   * A localizable name for display
   */
  public final hydra.util.Opt<String> displayName;
  
  /**
   * This property is deprecated. Either value, synchronous or asynchronous, has the same meaning: a command that starts execution within a configurable time and that completes execution within a configurable time.
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandType> commandType;
  
  /**
   * A description of the input to the Command
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandPayload> request;
  
  /**
   * A description of the output of the Command
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandPayload> response;
  
  public Command (hydra.ext.com.microsoft.azure.dtld.Iri type, String name, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandType> commandType, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandPayload> request, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandPayload> response) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    java.util.Objects.requireNonNull((commandType));
    java.util.Objects.requireNonNull((request));
    java.util.Objects.requireNonNull((response));
    this.type = type;
    this.name = name;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
    this.commandType = commandType;
    this.request = request;
    this.response = response;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Command)) {
      return false;
    }
    Command o = (Command) (other);
    return type.equals(o.type) && name.equals(o.name) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName) && commandType.equals(o.commandType) && request.equals(o.request) && response.equals(o.response);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * name.hashCode() + 5 * id.hashCode() + 7 * comment.hashCode() + 11 * description.hashCode() + 13 * displayName.hashCode() + 17 * commandType.hashCode() + 19 * request.hashCode() + 23 * response.hashCode();
  }
  
  public Command withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
  
  public Command withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
  
  public Command withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
  
  public Command withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
  
  public Command withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
  
  public Command withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
  
  public Command withCommandType(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandType> commandType) {
    java.util.Objects.requireNonNull((commandType));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
  
  public Command withRequest(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandPayload> request) {
    java.util.Objects.requireNonNull((request));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
  
  public Command withResponse(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.CommandPayload> response) {
    java.util.Objects.requireNonNull((response));
    return new Command(type, name, id, comment, description, displayName, commandType, request, response);
  }
}