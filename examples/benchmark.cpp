
#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <cerrno>

#include "z80.h"

namespace {

using z80::fast_u8;
using z80::fast_u16;
using z80::least_u8;
using z80::unused;

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

    void on_report() {}

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

    // The benchmark emulator provides no support for interrupts,
    // so no need to track the flags.
    // TODO: Remove that flag from the emulator's state at all.
    void on_set_is_int_disabled(bool f) { unused(f); }
    void on_set_iff(bool f) { unused(f); }

    void on_report() {}

protected:
    using base::self;
};

// Tracks use of CPU state.
template<typename B>
class state_watcher : public B {
public:
    typedef B base;

    fast_u16 on_get_pc() { ++pc_reads; return base::on_get_pc(); }
    void on_set_pc(fast_u16 nn) { ++pc_writes; base::on_set_pc(nn); }

    fast_u16 on_get_sp() { ++sp_reads; return base::on_get_sp(); }
    void on_set_sp(fast_u16 nn) { ++sp_writes; base::on_set_sp(nn); }

    fast_u16 on_get_wz() { ++wz_reads; return base::on_get_wz(); }
    void on_set_wz(fast_u16 nn) { ++wz_writes; base::on_set_wz(nn); }

    fast_u16 on_get_bc() { ++bc_reads; return base::on_get_bc(); }
    void on_set_bc(fast_u16 nn) { ++bc_writes; base::on_set_bc(nn); }
    fast_u8 on_get_b() { ++b_reads; return base::on_get_b(); }
    void on_set_b(fast_u8 n) { ++b_writes; base::on_set_b(n); }
    fast_u8 on_get_c() { ++c_reads; return base::on_get_c(); }
    void on_set_c(fast_u8 n) { ++c_writes; base::on_set_c(n); }

    fast_u16 on_get_de() { ++de_reads; return base::on_get_de(); }
    void on_set_de(fast_u16 nn) { ++de_writes; base::on_set_de(nn); }
    fast_u8 on_get_d() { ++d_reads; return base::on_get_d(); }
    void on_set_d(fast_u8 n) { ++d_writes; base::on_set_d(n); }
    fast_u8 on_get_e() { ++e_reads; return base::on_get_e(); }
    void on_set_e(fast_u8 n) { ++e_writes; base::on_set_e(n); }

    fast_u16 on_get_hl() { ++hl_reads; return base::on_get_hl(); }
    void on_set_hl(fast_u16 nn) { ++hl_writes; base::on_set_hl(nn); }
    fast_u8 on_get_h() { ++h_reads; return base::on_get_h(); }
    void on_set_h(fast_u8 n) { ++h_writes; base::on_set_h(n); }
    fast_u8 on_get_l() { ++l_reads; return base::on_get_l(); }
    void on_set_l(fast_u8 n) { ++l_writes; base::on_set_l(n); }

    fast_u16 on_get_af() { ++af_reads; return base::on_get_af(); }
    void on_set_af(fast_u16 nn) { ++af_writes; base::on_set_af(nn); }
    fast_u8 on_get_a() { ++a_reads; return base::on_get_a(); }
    void on_set_a(fast_u8 n) { ++a_writes; base::on_set_a(n); }
    fast_u8 on_get_f() { ++f_reads; return base::on_get_f(); }
    void on_set_f(fast_u8 n) { ++f_writes; base::on_set_f(n); }

    bool on_is_int_disabled() {
        ++is_int_disabled_reads;
        return base::on_is_int_disabled(); }
    void on_set_is_int_disabled(bool f) {
        ++is_int_disabled_writes;
        base::on_set_is_int_disabled(f); }

    bool on_is_halted() {
        ++is_halted_reads;
        return base::on_is_halted(); }
    void on_set_is_halted(bool f) {
        ++is_halted_writes;
        base::on_set_is_halted(f); }

    bool on_get_iff() { ++iff_reads; return base::on_get_iff(); }
    void on_set_iff(bool f) { ++iff_writes; base::on_set_iff(f); }

    void on_report() {
        std::printf("             pc reads:  %10.0f\n"
                    "             pc writes: %10.0f\n"
                    "             sp reads:  %10.0f\n"
                    "             sp writes: %10.0f\n"
                    "             wz reads:  %10.0f\n"
                    "             wz writes: %10.0f\n"
                    "             bc reads:  %10.0f\n"
                    "             bc writes: %10.0f\n"
                    "              b reads:  %10.0f\n"
                    "              b writes: %10.0f\n"
                    "              c reads:  %10.0f\n"
                    "              c writes: %10.0f\n"
                    "             de reads:  %10.0f\n"
                    "             de writes: %10.0f\n"
                    "              d reads:  %10.0f\n"
                    "              d writes: %10.0f\n"
                    "              e reads:  %10.0f\n"
                    "              e writes: %10.0f\n"
                    "             hl reads:  %10.0f\n"
                    "             hl writes: %10.0f\n"
                    "              h reads:  %10.0f\n"
                    "              h writes: %10.0f\n"
                    "              l reads:  %10.0f\n"
                    "              l writes: %10.0f\n"
                    "             af reads:  %10.0f\n"
                    "             af writes: %10.0f\n"
                    "              a reads:  %10.0f\n"
                    "              a writes: %10.0f\n"
                    "              f reads:  %10.0f\n"
                    "              f writes: %10.0f\n"
                    "            iff reads:  %10.0f\n"
                    "            iff writes: %10.0f\n"
                    "is_int_disabled reads:  %10.0f\n"
                    "is_int_disabled writes: %10.0f\n"
                    "      is_halted reads:  %10.0f\n"
                    "      is_halted writes: %10.0f\n",
                    static_cast<double>(pc_reads),
                    static_cast<double>(pc_writes),
                    static_cast<double>(sp_reads),
                    static_cast<double>(sp_writes),
                    static_cast<double>(wz_reads),
                    static_cast<double>(wz_writes),
                    static_cast<double>(bc_reads),
                    static_cast<double>(bc_writes),
                    static_cast<double>(b_reads),
                    static_cast<double>(b_writes),
                    static_cast<double>(c_reads),
                    static_cast<double>(c_writes),
                    static_cast<double>(de_reads),
                    static_cast<double>(de_writes),
                    static_cast<double>(d_reads),
                    static_cast<double>(d_writes),
                    static_cast<double>(e_reads),
                    static_cast<double>(e_writes),
                    static_cast<double>(hl_reads),
                    static_cast<double>(hl_writes),
                    static_cast<double>(h_reads),
                    static_cast<double>(h_writes),
                    static_cast<double>(l_reads),
                    static_cast<double>(l_writes),
                    static_cast<double>(af_reads),
                    static_cast<double>(af_writes),
                    static_cast<double>(a_reads),
                    static_cast<double>(a_writes),
                    static_cast<double>(f_reads),
                    static_cast<double>(f_writes),
                    static_cast<double>(iff_reads),
                    static_cast<double>(iff_writes),
                    static_cast<double>(is_int_disabled_reads),
                    static_cast<double>(is_int_disabled_writes),
                    static_cast<double>(is_halted_reads),
                    static_cast<double>(is_halted_writes));
    }

protected:
    using base::self;

    double pc_reads = 0;
    double pc_writes = 0;
    double sp_reads = 0;
    double sp_writes = 0;
    double wz_reads = 0;
    double wz_writes = 0;
    double bc_reads = 0;
    double bc_writes = 0;
    double b_reads = 0;
    double b_writes = 0;
    double c_reads = 0;
    double c_writes = 0;
    double de_reads = 0;
    double de_writes = 0;
    double d_reads = 0;
    double d_writes = 0;
    double e_reads = 0;
    double e_writes = 0;
    double hl_reads = 0;
    double hl_writes = 0;
    double h_reads = 0;
    double h_writes = 0;
    double l_reads = 0;
    double l_writes = 0;
    double af_reads = 0;
    double af_writes = 0;
    double a_reads = 0;
    double a_writes = 0;
    double f_reads = 0;
    double f_writes = 0;
    double iff_reads = 0;
    double iff_writes = 0;
    double is_int_disabled_reads = 0;
    double is_int_disabled_writes = 0;
    double is_halted_reads = 0;
    double is_halted_writes = 0;
};

// Tracks use of memory.
template<typename B>
class memory_watcher : public B {
public:
    typedef B base;

    fast_u8 on_read(fast_u16 addr) {
        ++memory_reads;
        return base::on_read(addr);
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        ++memory_writes;
        base::on_write(addr, n);
    }

    void on_report() {
        std::printf("         memory reads:  %10.0f\n"
                    "         memory writes: %10.0f\n",
                    static_cast<double>(memory_reads),
                    static_cast<double>(memory_writes));
    }

protected:
    using base::self;

    double memory_reads = 0;
    double memory_writes = 0;
};


#define WATCHER default_watcher

template<typename B>
class emulator : public WATCHER<B> {
public:
    typedef WATCHER<B> base;

    emulator() {}

    fast_u8 on_read(fast_u16 addr) {
        assert(addr < z80::address_space_size);
        base::on_read(addr);
        return memory[addr];
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        assert(addr < z80::address_space_size);
        base::on_write(addr, n);
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

        self().on_report();
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
