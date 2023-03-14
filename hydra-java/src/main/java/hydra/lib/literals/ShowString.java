package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;
import org.apache.commons.text.StringEscapeUtils;
import static hydra.dsl.Types.*;

public class ShowString<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.showString");
    }

    @Override
    public Type<A> type() {
        return function(string(), string());
    }

    public static String apply(String value) {
        return "\"" + StringEscapeUtils.escapeJava(value) + "\"";
    }
}
