package hydra.ext.gql;

import hydra.tools.AntlrReaderBase;
import hydra.util.Opt;
import java.util.BitSet;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.neo4j.GQLLexer;
import org.neo4j.GQLParser;
import org.antlr.v4.runtime.ANTLRErrorListener;


public class GQLReader extends AntlrReaderBase {
  public static Object read(String query) {
    GQLLexer lexer = new GQLLexer(CharStreams.fromString(query));
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    GQLParser parser = new GQLParser(tokens);

    parser.addErrorListener(new ANTLRErrorListener() {
      @Override
      public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine,
          String msg, RecognitionException e) {
        throw new GQLSyntaxError(line, charPositionInLine, offendingSymbol, msg);
      }

      @Override
      public void reportAmbiguity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, boolean exact,
          BitSet ambigAlts, ATNConfigSet configs) {
//        throw new GQLSyntaxError(startIndex, stopIndex, "ambiguity");
      }

      @Override
      public void reportAttemptingFullContext(Parser recognizer, DFA dfa, int startIndex, int stopIndex,
          BitSet conflictingAlts, ATNConfigSet configs) {
//        throw new GQLSyntaxError(startIndex, stopIndex, "full context");
      }

      @Override
      public void reportContextSensitivity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, int prediction,
          ATNConfigSet configs) {
//        throw new GQLSyntaxError(startIndex, stopIndex, "context sensitivity");
      }
    });

    GQLParser.GqlProgramContext ctx = parser.gqlProgram();

    return new Object(); // TODO
  }

  public static boolean isValidGqlQuery(String query) {
    try {
      read(query);
    } catch (GQLSyntaxError e) {
      return false;
    }

    return true;
  }

  public static Opt<String> tryQuery(String query) {
    try {
      read(query);
    } catch (GQLSyntaxError e) {
      return Opt.of(e.getMessage());
    }

    return Opt.empty();
  }

  public static class GQLSyntaxError extends RuntimeException {
    public GQLSyntaxError(int line, int ch, Object symbol, String message) {
      super("[line:" + line + ", char:" + ch + "]: " + message);
      //super("[line:" + line + ", char:" + ch + ", symbol:" + symbol + "]: " + message);
    }

    public GQLSyntaxError(int startIndex, int stopIndex, String message) {
      super("[start:" + startIndex + ", stop:" + stopIndex + "]: " + message);
    }
  }
}
