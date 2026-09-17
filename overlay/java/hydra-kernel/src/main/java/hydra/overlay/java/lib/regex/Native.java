package hydra.overlay.java.lib.regex;

import hydra.overlay.java.util.Optional;

/**
 * Translates a Hydra regex pattern (docs/specification/regex.md) to PCRE syntax via
 * hydra.parse.regex |&gt; hydra.print.pcre.regex, so the native java.util.regex engine (PCRE-like) sees
 * the same pattern that every other host does. An ill-formed pattern (one that hydra.parse.regex
 * rejects) yields None; each primitive treats this as "no match", the same portable-failure
 * convention as a well-formed pattern with no match. See issue #603.
 */
final class Native {
    private Native() {
    }

    static Optional<String> toNative(String pattern) {
        return hydra.parse.Regex.parseRegex(pattern).map(hydra.print.pcre.Regex::printRegex);
    }
}
