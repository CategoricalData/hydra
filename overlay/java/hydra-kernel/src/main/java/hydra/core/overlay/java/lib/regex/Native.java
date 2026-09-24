package hydra.core.overlay.java.lib.regex;

import hydra.core.overlay.java.util.Optional;

/**
 * Translates a Hydra regex pattern (docs/specification/regex.md) to PCRE syntax via
 * hydra.core.parse.regex |&gt; hydra.core.print.pcre.regex, so the native java.util.regex engine (PCRE-like) sees
 * the same pattern that every other host does. An ill-formed pattern (one that hydra.core.parse.regex
 * rejects) yields None; each primitive treats this as "no match", the same portable-failure
 * convention as a well-formed pattern with no match. See issue #603.
 */
final class Native {
    private Native() {
    }

    static Optional<String> toNative(String pattern) {
        return hydra.core.parse.Regex.parseRegex(pattern).map(hydra.core.print.pcre.Regex::printRegex);
    }
}
