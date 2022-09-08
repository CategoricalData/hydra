package hydra.lib;

import java.util.ArrayList;
import java.util.List;

public interface Strings {
    static String cat(List<String> args) {
        return String.join("", args);
    }

    static int length(String s) {
        return s.length();
    }

    // Note: the substring is not interpreted as a regular expression; it is simply a literal string. See Haskell's Data.List.Split.
    static List<String> splitOn(String delim, String string) {
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

    static String toLower(String upper) {
        return upper.toLowerCase(); // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
    }

    static String toUpper(String lower) {
        return lower.toUpperCase(); // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
    }
}
