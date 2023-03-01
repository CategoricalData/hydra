package hydra.lib.strings;

import hydra.core.Name;
import hydra.PrimitiveFunction;
import java.util.List;
import java.util.ArrayList;

public class SplitOn<M> extends PrimitiveFunction<M> {
    public Name name() {
        return new Name("hydra/lib/strings.splitOn");
    }

    // Note: the substring is not interpreted as a regular expression; it is simply a literal string. See Haskell's Data.List.Split.
    public static List<String> apply(String delim, String string) {
        List<String> parts = new ArrayList<>();

        if (delim.length() == 0) {
            parts.add("");
            for (int i = 0; i < string.length(); i++) {
                parts.add(string.substring(i, i+1));
            }
        } else {

            byte[] delimBytes = delim.getBytes();
            byte[] stringBytes = string.getBytes();

            int k = 0;
            for (int i = 0; i <= stringBytes.length - delimBytes.length; i++) {
                boolean match = true;

                for (int j = 0; j < delimBytes.length; j++) {
                    if (stringBytes[i + j] != delimBytes[j]) {
                        match = false;
                        break;
                    }
                }

                if (match) {
                    parts.add(string.substring(k, i));
                    i += delimBytes.length;
                    k = i;
                    i--;
                }
            }

            parts.add(string.substring(k));
        }

        return parts;
    }
}
