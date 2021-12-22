package hydra.lib.literals

import org.apache.commons.text.StringEscapeUtils


def showInt32(i: Int): String = i.toString

def showString(s: String): String = StringEscapeUtils.escapeJava(s)
