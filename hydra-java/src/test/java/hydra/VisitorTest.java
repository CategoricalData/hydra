package hydra;

import hydra.ext.yaml.model.Node;
import hydra.ext.yaml.model.Scalar;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

import static hydra.VisitorTest.YamlDsl.*;
import static org.junit.jupiter.api.Assertions.*;


/**
 * This test illustrates defining a simple domain-specific API on top of the generated YAML API in Java,
 * then constructing and visiting YAML nodes.
 */
public class VisitorTest {
  private final Node yamlNode1 = str("foo bar baz");
  private final Node yamlNode2 = int_(42);
  private final Node yamlNode3 = seq(yamlNode1, yamlNode2);
  private final Node yamlNode4 = map(
      "lat", float_(37.7749),
      "lon", float_(-122.4194));

  // Example visitor prints a given YAML node in a simple format
  private final Node.Visitor<String> exampleVisitor = new Node.Visitor<String>() {
    @Override
    public String visit(Node.Mapping mapping) {
      return "{" + mapping.value.entrySet().stream()
          .map(e -> e.getKey().accept(exampleVisitor) + "=" + e.getValue().accept(exampleVisitor))
          .collect(Collectors.joining(", ")) + "}";
    }

    @Override
    public String visit(Node.Scalar scalar) {
      return scalar.value.accept(new Scalar.Visitor<String>() {
        @Override
        public String visit(Scalar.Bool bool) {
          return "bool:" + bool.value;
        }

        @Override
        public String visit(Scalar.Float_ float_) {
          return "float:" + float_.value;
        }

        @Override
        public String visit(Scalar.Int anInt) {
          return "int:" + anInt.value;
        }

        @Override
        public String visit(Scalar.Null aNull) {
          return "null";
        }

        @Override
        public String visit(Scalar.Str str) {
          return "str:\"" + str.value + "\"";
        }
      });
    }

    @Override
    public String visit(Node.Sequence sequence) {
      return "[" + sequence.value.stream()
          .map(node -> node.accept(exampleVisitor))
          .collect(Collectors.joining(", ")) + "]";
    }
  };

  // Example partial visitor checks whether a node is numeric and equal to the number 42
  private final Node.PartialVisitor<Boolean> examplePartialVisitor = new Node.PartialVisitor<Boolean>() {
    @Override
    public Boolean visit(Node.Scalar instance) {
      return instance.value.accept(new Scalar.PartialVisitor<Boolean>() {
        @Override
        public Boolean visit(Scalar.Float_ instance) {
          return instance.value == 42.0;
        }

        @Override
        public Boolean visit(Scalar.Int instance) {
          return instance.value.intValue() == 42;
        }

        @Override
        public Boolean otherwise (Scalar ignored) {
          return false;
        }
      });
    }

    @Override
    public Boolean otherwise(Node instance) {
      return false;
    }
  };

  @Test
  public void demonstrateVisitor() {
    assertEquals("str:\"foo bar baz\"", yamlNode1.accept(exampleVisitor));
    assertEquals("int:42", yamlNode2.accept(exampleVisitor));
    assertEquals("[str:\"foo bar baz\", int:42]", yamlNode3.accept(exampleVisitor));
    assertEquals("{str:\"lat\"=float:37.7749, str:\"lon\"=float:-122.4194}", yamlNode4.accept(exampleVisitor));
  }

  @Test
  public void demonstratePartialVisitor() {
    assertFalse(yamlNode1.accept(examplePartialVisitor));
    assertTrue(yamlNode2.accept(examplePartialVisitor));
    assertFalse(yamlNode3.accept(examplePartialVisitor));
  }

  public interface YamlDsl {
    static Node bool(boolean b) {
      return new Node.Scalar(new Scalar.Bool(b));
    }

    static Node float_(double value) {
      return new Node.Scalar(new Scalar.Float_(value));
    }

    static Node int_(int value) {
      return new Node.Scalar(new Scalar.Int(BigInteger.valueOf(value)));
    }

    static Node map(Map<Node, Node> m) {
      return new Node.Mapping(m);
    }

    static Node map(Object... keyvals) {
      assert keyvals.length % 2 == 0;

      Map<Node, Node> m = new HashMap<>();
      for (int i = 0; i < keyvals.length; i += 2) {
        String key = (String) keyvals[i];
        Node value = (Node) keyvals[i + 1];
        m.put(str(key), value);
      }

      return map(m);
    }

    static Node seq(Node... nodes) {
      return new Node.Sequence(Arrays.asList(nodes));
    }

    static Node str(String s) {
      return new Node.Scalar(new Scalar.Str(s));
    }
  }
}