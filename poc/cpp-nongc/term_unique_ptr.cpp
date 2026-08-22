// Proof-of-concept for #678: structural tree-ownership in C++ via std::unique_ptr RAII.
//
// Hydra runtime data is an immutable, acyclic, tree-shaped Term (recursion is name-based; see Core.hs
// Lambda { parameter :: Name, body :: Term } and Term::Variable(Name) — a name lookup, not a back-pointer).
// So ownership is structural: each node owned by its unique parent, freed when the parent is freed.
//
// Discipline demonstrated here (the recommended C++ default):
//   * Hydra record  -> struct
//   * Hydra union   -> a tagged struct (variant kind + payload)
//   * recursive child -> std::unique_ptr<Term>   (unique ownership; ~Term() frees children recursively)
//   * list<T> -> std::vector<T>, optional<T> -> std::optional<...>, name -> std::string
//   * NO shared_ptr (that is shared ownership — unneeded for a unique-ownership tree)
//   * NO raw owning pointers, NO manual delete of children — RAII does it
//
// Leak-freedom: two independent signals.
//   (1) A static drop counter incremented in ~Term(); asserted == node count (deterministic).
//   (2) Built with -fsanitize=address,leak: LSan reports 0 leaks at exit, ASan aborts on any double-free.

#include <cassert>
#include <cstdio>
#include <memory>
#include <optional>
#include <string>
#include <vector>

static long g_drop_count = 0;

using Name = std::string;

// A faithful slice of Hydra's Term exercising every ownership shape.
struct Term {
    enum class Kind { Variable, Literal, Lambda, Application, List, Optional, Pair };
    Kind kind;

    // Leaves.
    Name name;      // Variable: a NAME, not a link to the binder.
    long literal = 0;

    // Lambda: parameter is a NAME; body is an owned recursive child.
    Name parameter;
    std::unique_ptr<Term> body;

    // Application / Pair: two owned recursive children.
    std::unique_ptr<Term> left;
    std::unique_ptr<Term> right;

    // List: owned recursive children.
    std::vector<std::unique_ptr<Term>> items;

    // Optional: an optional owned recursive child.
    std::optional<std::unique_ptr<Term>> opt;

    ~Term() { ++g_drop_count; }  // RAII: children's unique_ptrs are destroyed automatically before/after this.
};

static std::unique_ptr<Term> mk(Term::Kind k) {
    auto t = std::make_unique<Term>();
    t->kind = k;
    return t;
}

static std::unique_ptr<Term> variable(const Name& n) { auto t = mk(Term::Kind::Variable); t->name = n; return t; }
static std::unique_ptr<Term> literal(long v)          { auto t = mk(Term::Kind::Literal);  t->literal = v; return t; }

// Build a representative Term: (\x -> (f x)) applied to 42, wrapped in a list + pair, with an optional.
static std::unique_ptr<Term> build_sample() {
    auto lam = mk(Term::Kind::Lambda);
    lam->parameter = "x";
    {
        auto app = mk(Term::Kind::Application);
        app->left = variable("f");
        app->right = variable("x");
        lam->body = std::move(app);
    }
    auto applied = mk(Term::Kind::Application);
    applied->left = std::move(lam);
    applied->right = literal(42);

    auto list = mk(Term::Kind::List);
    list->items.push_back(std::move(applied));
    {
        auto some = mk(Term::Kind::Optional);
        some->opt = literal(7);
        list->items.push_back(std::move(some));
    }
    {
        auto none = mk(Term::Kind::Optional);
        none->opt = std::nullopt;
        list->items.push_back(std::move(none));
    }

    auto pair = mk(Term::Kind::Pair);
    pair->left = std::move(list);
    pair->right = variable("top");
    return pair;
}

// Count nodes by structural traversal (independent of the drop counter).
static long count_nodes(const Term* t) {
    if (!t) return 0;
    long n = 1;
    n += count_nodes(t->body.get());
    n += count_nodes(t->left.get());
    n += count_nodes(t->right.get());
    for (const auto& it : t->items) n += count_nodes(it.get());
    if (t->opt.has_value()) n += count_nodes(t->opt->get());
    return n;
}

int main() {
    g_drop_count = 0;
    long expected;
    {
        auto root = build_sample();
        expected = count_nodes(root.get());
        assert(g_drop_count == 0 && "nothing dropped during construction");
        // root goes out of scope here -> RAII recursively destroys the whole tree.
    }
    if (g_drop_count != expected) {
        std::fprintf(stderr, "FAIL: expected %ld drops, got %ld\n", expected, g_drop_count);
        return 1;
    }
    std::printf("OK: unique_ptr RAII — %ld nodes constructed, %ld destroyed (exactly once each), leak-free.\n",
                expected, g_drop_count);
    return 0;
}
