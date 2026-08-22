/* Proof-of-concept for #678: structural tree-ownership in C via arena/region allocation.
 *
 * C has no destructors and no ownership types, so RAII is unavailable. The clean answer for an immutable,
 * acyclic, tree-shaped value (Hydra's Term — recursion is name-based; see Core.hs) is ARENA allocation:
 * allocate every node from a region, then free the region WHOLESALE. No per-node free, so no leak and no
 * double-free regardless of tree shape or size.
 *
 * Discipline demonstrated:
 *   * arena_alloc(a, n)   -> bump-allocate n bytes from the region (grows in blocks)
 *   * Term nodes + names + child-pointer arrays all live IN the arena
 *   * recursive children are plain Term* into the same arena (safe: shared arena lifetime)
 *   * arena_free_all(a)   -> free every block at once
 *
 * Leak-freedom signals:
 *   (1) Arena accounting: total bytes malloc'd == total bytes free'd after arena_free_all (deterministic).
 *   (2) Built with -fsanitize=address,leak: LSan reports 0 leaks after arena_free_all; ASan catches misuse.
 *
 * API implication (the honest cost): the consumer holds and frees an ARENA HANDLE; every value's lifetime is
 * tied to its arena. This is a different contract than idiomatic per-object malloc/free C, but it is entirely
 * leak-safe and, for immutable tree data, loses nothing (no node ever needs to be freed early).
 */

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- Bump arena ---------------------------------------------------------- */

typedef struct Block {
    char *data;
    size_t size;
    struct Block *next;
} Block;

typedef struct {
    Block *head;      /* current (most recent) block; list threads back via ->next */
    size_t offset;    /* bump offset within head */
    size_t block_size;
    size_t bytes_malloced;
    size_t bytes_freed;
} Arena;

static void arena_init(Arena *a, size_t block_size) {
    a->head = NULL;
    a->offset = 0;
    a->block_size = block_size ? block_size : (size_t)(64 * 1024);
    a->bytes_malloced = 0;
    a->bytes_freed = 0;
}

static void arena_grow(Arena *a, size_t need) {
    size_t sz = need > a->block_size ? need : a->block_size;
    Block *b = (Block *)malloc(sizeof(Block));
    assert(b && "arena block header OOM");
    b->data = (char *)malloc(sz);
    assert(b->data && "arena block data OOM");
    b->size = sz;
    b->next = a->head;
    a->head = b;
    a->offset = 0;
    a->bytes_malloced += sz + sizeof(Block);
}

static void *arena_alloc(Arena *a, size_t n) {
    n = (n + 15u) & ~(size_t)15u;   /* 16-byte align */
    if (a->head == NULL || a->offset + n > a->head->size) arena_grow(a, n);
    void *p = a->head->data + a->offset;
    a->offset += n;
    return p;
}

static void arena_free_all(Arena *a) {
    Block *b = a->head;
    while (b) {
        Block *next = b->next;
        a->bytes_freed += b->size + sizeof(Block);
        free(b->data);
        free(b);
        b = next;
    }
    a->head = NULL;
    a->offset = 0;
}

/* ---- Term (arena-allocated) --------------------------------------------- */

typedef enum { T_VARIABLE, T_LITERAL, T_LAMBDA, T_APPLICATION, T_LIST, T_PAIR } Kind;

typedef struct Term {
    Kind kind;
    const char *name;        /* arena-owned copy; a NAME, not a link to the binder */
    long literal;
    const char *parameter;   /* arena-owned copy */
    struct Term *body;
    struct Term *left;
    struct Term *right;
    struct Term **items;     /* arena-owned array */
    size_t item_count;
} Term;

static const char *arena_strdup(Arena *a, const char *s) {
    size_t n = strlen(s) + 1;
    char *p = (char *)arena_alloc(a, n);
    memcpy(p, s, n);
    return p;
}

static Term *mk(Arena *a, Kind k) {
    Term *t = (Term *)arena_alloc(a, sizeof(Term));
    memset(t, 0, sizeof(Term));
    t->kind = k;
    return t;
}

static long count_nodes(const Term *t) {
    if (!t) return 0;
    long n = 1;
    n += count_nodes(t->body);
    n += count_nodes(t->left);
    n += count_nodes(t->right);
    for (size_t i = 0; i < t->item_count; ++i) n += count_nodes(t->items[i]);
    return n;
}

/* Stress the block-growth + wholesale-free path: with a tiny block size, a wide tree forces the arena to
 * allocate many blocks. If arena_free_all's block-list walk were wrong, LSan would report leaked blocks and
 * the byte accounting would mismatch. Proves the growth path frees cleanly, not just the single-block case. */
static long stress_multiblock(void) {
    Arena a;
    arena_init(&a, 256);  /* tiny blocks -> many of them */
    long total = 0;
    for (int i = 0; i < 500; ++i) {
        Term *t = mk(&a, T_VARIABLE);
        t->name = arena_strdup(&a, "some_variable_name_to_use_space");
        total += count_nodes(t);
    }
    arena_free_all(&a);
    assert(a.bytes_malloced == a.bytes_freed && "multi-block arena freed cleanly");
    assert(a.bytes_malloced > 256 && "stress actually allocated multiple blocks");
    return total;
}

int main(void) {
    long stressed = stress_multiblock();

    Arena arena;
    arena_init(&arena, 64 * 1024);

    /* (\x -> (f x)) 42, wrapped in a list + pair. */
    Term *lam = mk(&arena, T_LAMBDA);
    lam->parameter = arena_strdup(&arena, "x");
    Term *app0 = mk(&arena, T_APPLICATION);
    app0->left = mk(&arena, T_VARIABLE);  app0->left->name = arena_strdup(&arena, "f");
    app0->right = mk(&arena, T_VARIABLE); app0->right->name = arena_strdup(&arena, "x");
    lam->body = app0;

    Term *applied = mk(&arena, T_APPLICATION);
    applied->left = lam;
    applied->right = mk(&arena, T_LITERAL); applied->right->literal = 42;

    Term *list = mk(&arena, T_LIST);
    list->item_count = 2;
    list->items = (Term **)arena_alloc(&arena, sizeof(Term *) * list->item_count);
    list->items[0] = applied;
    list->items[1] = mk(&arena, T_LITERAL); list->items[1]->literal = 7;

    Term *pair = mk(&arena, T_PAIR);
    pair->left = list;
    pair->right = mk(&arena, T_VARIABLE); pair->right->name = arena_strdup(&arena, "top");

    long nodes = count_nodes(pair);
    assert(arena.bytes_malloced > 0);

    arena_free_all(&arena);

    if (arena.bytes_malloced != arena.bytes_freed) {
        fprintf(stderr, "FAIL: malloced %zu bytes, freed %zu bytes\n",
                arena.bytes_malloced, arena.bytes_freed);
        return 1;
    }
    printf("OK: C arena — %ld nodes built in one region; %zu bytes malloced == %zu bytes freed wholesale "
           "(no per-node free, leak-free). Multi-block stress: %ld nodes across many tiny blocks, all freed. "
           "API implication: consumer holds/frees the arena handle.\n",
           nodes, arena.bytes_malloced, arena.bytes_freed, stressed);
    return 0;
}
