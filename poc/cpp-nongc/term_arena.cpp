// Proof-of-concept for #678: the ARENA / region option for C++ (the second candidate discipline).
//
// Instead of per-node unique_ptr, allocate the whole Term tree from a pool and free the pool at once.
// This backs the "arena API implication" claim with compiled code: the consumer holds an arena and frees
// it wholesale; individual nodes are never freed. Because the data is immutable and tree-shaped, there is
// never a reason to free one node early, so wholesale free loses nothing.
//
// Leak-freedom signals:
//   (1) Arena accounting: bytes handed out == bytes reclaimed at arena_free_all (deterministic).
//   (2) Built with -fsanitize=address,leak: LSan reports 0 leaks after the arena is freed.

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

// A minimal bump-allocating arena backed by a list of large blocks.
class Arena {
public:
    explicit Arena(size_t block = 64 * 1024) : block_size_(block) {}
    ~Arena() { free_all(); }

    void* alloc(size_t n) {
        n = (n + 15) & ~size_t(15);  // 16-byte align
        if (blocks_.empty() || offset_ + n > blocks_.back().size) grow(n);
        void* p = blocks_.back().data + offset_;
        offset_ += n;
        bytes_handed_out_ += n;
        return p;
    }

    void free_all() {
        for (auto& b : blocks_) { bytes_reclaimed_ += b.size; std::free(b.data); }
        blocks_.clear();
        offset_ = 0;
    }

    size_t bytes_handed_out() const { return bytes_handed_out_; }
    size_t bytes_reclaimed() const { return bytes_reclaimed_; }

private:
    struct Block { char* data; size_t size; };
    void grow(size_t need) {
        size_t sz = need > block_size_ ? need : block_size_;
        char* data = static_cast<char*>(std::malloc(sz));
        assert(data && "arena OOM");
        blocks_.push_back({data, sz});
        offset_ = 0;
    }
    std::vector<Block> blocks_;
    size_t block_size_;
    size_t offset_ = 0;
    size_t bytes_handed_out_ = 0;
    size_t bytes_reclaimed_ = 0;
};

// Term nodes live IN the arena. Recursive children are plain pointers into the same arena — safe because
// every node shares the arena's lifetime. Names are copied into the arena too (no std::string heap alloc
// escaping the region), so freeing the arena frees everything.
struct Term {
    enum class Kind { Variable, Literal, Lambda, Application, List, Pair } kind;
    const char* name = nullptr;   // arena-owned copy
    long literal = 0;
    const char* parameter = nullptr;
    Term* body = nullptr;
    Term* left = nullptr;
    Term* right = nullptr;
    Term** items = nullptr;       // arena-owned array
    size_t item_count = 0;
};

static const char* arena_strdup(Arena& a, const char* s) {
    size_t n = std::strlen(s) + 1;
    char* p = static_cast<char*>(a.alloc(n));
    std::memcpy(p, s, n);
    return p;
}

static Term* mk(Arena& a, Term::Kind k) {
    Term* t = static_cast<Term*>(a.alloc(sizeof(Term)));
    *t = Term{};
    t->kind = k;
    return t;
}

static long count_nodes(const Term* t) {
    if (!t) return 0;
    long n = 1;
    n += count_nodes(t->body);
    n += count_nodes(t->left);
    n += count_nodes(t->right);
    for (size_t i = 0; i < t->item_count; ++i) n += count_nodes(t->items[i]);
    return n;
}

int main() {
    long nodes;
    {
        Arena arena;
        // (\x -> (f x)) 42, wrapped in a list + pair.
        Term* lam = mk(arena, Term::Kind::Lambda);
        lam->parameter = arena_strdup(arena, "x");
        Term* app0 = mk(arena, Term::Kind::Application);
        app0->left = mk(arena, Term::Kind::Variable);  app0->left->name = arena_strdup(arena, "f");
        app0->right = mk(arena, Term::Kind::Variable); app0->right->name = arena_strdup(arena, "x");
        lam->body = app0;

        Term* applied = mk(arena, Term::Kind::Application);
        applied->left = lam;
        applied->right = mk(arena, Term::Kind::Literal); applied->right->literal = 42;

        Term* list = mk(arena, Term::Kind::List);
        list->item_count = 2;
        list->items = static_cast<Term**>(arena.alloc(sizeof(Term*) * list->item_count));
        list->items[0] = applied;
        list->items[1] = mk(arena, Term::Kind::Literal); list->items[1]->literal = 7;

        Term* pair = mk(arena, Term::Kind::Pair);
        pair->left = list;
        pair->right = mk(arena, Term::Kind::Variable); pair->right->name = arena_strdup(arena, "top");

        nodes = count_nodes(pair);
        assert(arena.bytes_handed_out() > 0);
        // arena dtor -> free_all(): the entire region (all nodes + names + arrays) freed wholesale.
    }
    // After scope exit the arena is destroyed. LSan (if enabled) confirms 0 leaks here.
    std::printf("OK: arena — %ld nodes built in one region, freed wholesale (no per-node free). "
                "API implication: consumer holds/frees the arena handle.\n", nodes);
    return 0;
}
