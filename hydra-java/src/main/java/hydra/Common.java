package hydra;

import hydra.core.Term;
import hydra.core.Type;


public interface Common {
    static <A> Term<A> stripTerm(Term<A> term) {
        return term.accept(new Term.PartialVisitor<>() {
            @Override
            public Term<A> otherwise(Term ignored) {
                return term;
            }

            @Override
            public Term<A> visit(Term.Annotated instance) {
                return stripTerm(((Term.Annotated<A>) instance).value.subject);
            }
        });
    }

    static <A> Type<A> stripType(Type<A> type) {
        return type.accept(new Type.PartialVisitor<>() {
            @Override
            public Type<A> otherwise(Type ignored) {
                return type;
            }

            @Override
            public Type<A> visit(Type.Annotated instance) {
                return stripType(((Type.Annotated<A>) instance).value.subject);
            }
        });
    }
}
