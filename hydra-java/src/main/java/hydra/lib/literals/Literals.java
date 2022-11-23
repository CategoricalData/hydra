package hydra.lib.literals;

import org.apache.commons.text.StringEscapeUtils;

public interface Literals {
    static String showInt32(Integer value) {
        return Integer.toString(value);
    }

    static String showString(String value) {
      return StringEscapeUtils.escapeJava(value);
    }
}
