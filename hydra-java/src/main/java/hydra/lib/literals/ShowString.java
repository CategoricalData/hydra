package hydra.lib.literals;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;
import org.apache.commons.text.StringEscapeUtils;

public class ShowString<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.showString");
    }

    public static String apply(String value) {
        return "\"" + StringEscapeUtils.escapeJava(value) + "\"";
    }
}
