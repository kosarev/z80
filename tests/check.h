
#ifndef CHECK_H
#define CHECK_H

#define CHECK(c) (check((c), __LINE__))

static void check(bool c, unsigned line_no) {
    if(c)
        return;

    std::fprintf(stderr, "%s:%u: check failed\n", __FILE__,
                 static_cast<unsigned>(line_no));
    std::abort();
}

#endif
