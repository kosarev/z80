
#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <cerrno>

#include "z80.h"

namespace {

using z80::fast_u8;
using z80::fast_u16;
using z80::least_u8;

#if defined(__GNUC__) || defined(__clang__)
# define LIKE_PRINTF(format, args) \
      __attribute__((__format__(__printf__, format, args)))
#else
# define LIKE_PRINTF(format, args) /* nothing */
#endif

const char program_name[] = "benchmark";

[[noreturn]] LIKE_PRINTF(1, 0)
void verror(const char *format, va_list args) {
    std::fprintf(stderr, "%s: ", program_name);
    std::vfprintf(stderr, format, args);
    std::fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

[[noreturn]] LIKE_PRINTF(1, 2)
void error(const char *format, ...) {
    va_list args;
    va_start(args, format);
    verror(format, args);
    va_end(args);
}

static constexpr fast_u16 quit_addr = 0x0000;
static constexpr fast_u16 bdos_addr = 0x0005;
static constexpr fast_u16 entry_addr = 0x0100;

// Handles CP/M BDOS calls to write text messages.
template<typename B>
class default_watcher : public B {
public:
    typedef B base;

    void write_char(fast_u8 c) {
        std::putchar(static_cast<char>(c));
    }

    void handle_c_write() {
        write_char(base::get_e());
    }

    void handle_c_writestr() {
        fast_u16 addr = base::get_de();
        for(;;) {
            fast_u8 c = self().on_read(addr);
            if(c == '$')
                break;

            write_char(c);
            addr = (addr + 1) % z80::address_space_size;
        }
    }

    void handle_bdos_call() {
        switch(base::get_c()) {
        case c_write:
            handle_c_write();
            break;
        case c_writestr:
            handle_c_writestr();
            break;
        }
    }

    void on_step() {
        if(base::get_pc() == bdos_addr)
            handle_bdos_call();

        base::on_step();
    }

protected:
    using base::self;

private:
    static constexpr fast_u8 c_write = 0x02;
    static constexpr fast_u8 c_writestr = 0x09;
};

// Lets the emulator to perform at full speed.
template<typename B>
class empty_watcher : public B {
public:
    typedef B base;

protected:
    using base::self;
};

#define WATCHER default_watcher

template<typename B>
class emulator : public WATCHER<B> {
public:
    typedef WATCHER<B> base;

    emulator() {}

    fast_u8 on_read(fast_u16 addr) {
        assert(addr < z80::address_space_size);
        return memory[addr];
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        assert(addr < z80::address_space_size);
        memory[addr] = static_cast<least_u8>(n);
    }

    void run(const char *program) {
        FILE *f = std::fopen(program, "rb");
        if(!f) {
            error("Cannot open file '%s': %s", program,
                  std::strerror(errno));
        }

        std::size_t count = std::fread(
            memory + entry_addr, /* size= */ 1,
            z80::address_space_size - entry_addr, f);
        if(ferror(f)) {
            error("Cannot read file '%s': %s", program,
                  std::strerror(errno));
        }
        if(count == 0)
            error("Program file '%s' is empty", program);
        if(!feof(f))
            error("Program file '%s' is too large", program);

        if(std::fclose(f) != 0) {
            error("Cannot close file '%s': %s", program,
                  std::strerror(errno));
        }

        base::set_pc(entry_addr);
        memory[bdos_addr] = 0xc9;  // ret

        for(;;) {
            fast_u16 pc = base::get_pc();
            if(pc == quit_addr)
                break;

            self().on_step();
        }
    }

protected:
    using base::self;

private:
    least_u8 memory[z80::address_space_size] = {};
};

class i8080_emulator : public emulator<z80::i8080_cpu<i8080_emulator>>
{};

class z80_emulator : public emulator<z80::z80_cpu<z80_emulator>>
{};

[[noreturn]] static void usage() {
    error("benchmark {i8080|z80} <program.com>");
}

}  // anonymous namespace

int main(int argc, char *argv[]) {
    if(argc != 3)
        usage();

    const char *program = argv[2];

    const char *cpu = argv[1];
    if(std::strcmp(cpu, "i8080") == 0) {
        i8080_emulator e;
        e.run(program);
    } else if(std::strcmp(cpu, "z80") == 0) {
        z80_emulator e;
        e.run(program);
    } else {
        error("Unknown CPU '%s'", cpu);
    }
}
