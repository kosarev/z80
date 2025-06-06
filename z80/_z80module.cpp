
/*  Z80 Machine Emulation Module for Python.
    https://github.com/kosarev/z80

    Copyright (C) 2019-2025 Ivan Kosarev.
    mail@ivankosarev.com

    Published under the MIT license.
*/

#include <Python.h>

#include <algorithm>
#include <cstring>
#include <new>

#include "../z80.h"

namespace {

using z80::fast_u8;
using z80::fast_u16;
using z80::fast_u32;
using z80::least_u8;
using z80::least_u16;
using z80::least_u32;

using z80::unused;
using z80::get_low8;
using z80::get_high8;
using z80::make16;
using z80::iregp;

static inline void split16(least_u8 &h, least_u8 &l, fast_u16 n) {
    h = get_high8(n);
    l = get_low8(n);
}

class decref_guard {
public:
    decref_guard(PyObject *object)
        : object(object)
    {}

    ~decref_guard() {
        if(object)
            Py_XDECREF(object);
    }

    explicit operator bool () const {
        return object != nullptr;
    }

    PyObject *get() const {
        return object;
    }

private:
    PyObject *object;
};

template<typename B, typename S>
class machine : public B {
public:
    typedef B base;
    typedef S machine_state;

    machine() {}

    machine_state &get_state() {
        return state;
    }

    fast_u8 on_read(fast_u16 addr) {
        const fast_u8 default_value = 0xff;
        assert(addr < z80::address_space_size);

        if(!on_read_callback) {
            return state.memory[addr];
        }

        PyObject *arg = Py_BuildValue("(i)", addr);
        decref_guard arg_guard(arg);

        PyObject *result = PyObject_CallObject(on_read_callback, arg);
        decref_guard result_guard(result);

        if(!result) {
            assert(0);  // TODO: stop();
            return default_value;
        }

        if(!PyLong_Check(result)) {
            PyErr_SetString(PyExc_TypeError, "returning value must be integer");
            assert(0);  // TODO: stop();
            return default_value;
        }

        return z80::mask8(PyLong_AsUnsignedLong(result));
    }

    PyObject *set_read_callback(PyObject *callback) {
        PyObject *old_callback = on_read_callback;
        on_read_callback = callback;
        return old_callback;
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        assert(addr < z80::address_space_size);

        if(!on_write_callback) {
            state.memory[addr] = n;
            return;
        }

        PyObject *args = Py_BuildValue("(i, i)", addr, n);
        decref_guard arg_guard(args);

        PyObject *result = PyObject_CallObject(on_write_callback, args);
        decref_guard result_guard(result);
    }

    PyObject *set_write_callback(PyObject *callback) {
        PyObject *old_callback = on_write_callback;
        on_write_callback = callback;
        return old_callback;
    }

    fast_u8 on_input(fast_u16 addr) {
        const fast_u8 default_value = 0xff;
        if(!on_input_callback)
            return default_value;

        PyObject *arg = Py_BuildValue("(i)", addr);
        decref_guard arg_guard(arg);

        PyObject *result = PyObject_CallObject(on_input_callback, arg);
        decref_guard result_guard(result);

        if(!result) {
            assert(0);  // TODO: stop();
            return default_value;
        }

        if(!PyLong_Check(result)) {
            PyErr_SetString(PyExc_TypeError, "returning value must be integer");
            assert(0);  // TODO: stop();
            return default_value;
        }

        return z80::mask8(PyLong_AsUnsignedLong(result));
    }

    PyObject *set_input_callback(PyObject *callback) {
        PyObject *old_callback = on_input_callback;
        on_input_callback = callback;
        return old_callback;
    }

    void on_output(fast_u16 addr, fast_u8 value) {
        if(!on_output_callback)
            return;

        PyObject *args = Py_BuildValue("(i, i)", addr, value);
        decref_guard arg_guard(args);

        PyObject *result = PyObject_CallObject(on_output_callback, args);
        decref_guard result_guard(result);
    }

    PyObject *set_output_callback(PyObject *callback) {
        PyObject *old_callback = on_output_callback;
        on_output_callback = callback;
        return old_callback;
    }

    void on_reti() {
        base::on_reti();

        if(on_reti_callback) {
            PyObject *result = PyObject_CallObject(on_reti_callback, NULL);
            decref_guard result_guard(result);
        }
    }

    PyObject *set_reti_callback(PyObject *callback) {
        PyObject *old_callback = on_reti_callback;
        on_reti_callback = callback;
        return old_callback;
    }

    fast_u8 on_get_int_vector() {
        const fast_u8 default_value = 0xff;
        if(!on_get_int_vector_callback)
            return base::on_get_int_vector();

        PyObject *result = PyObject_CallObject(on_get_int_vector_callback, NULL);
        decref_guard result_guard(result);

        if(!result) {
            assert(0);  // TODO: stop();
            return default_value;
        }

        if(!PyLong_Check(result)) {
            PyErr_SetString(PyExc_TypeError, "returning value must be integer");
            assert(0);  // TODO: stop();
            return default_value;
        }

        return z80::mask8(PyLong_AsUnsignedLong(result));
    }

    PyObject *set_get_int_vector_callback(PyObject *callback) {
        PyObject *old_callback = on_get_int_vector_callback;
        on_get_int_vector_callback = callback;
        return old_callback;
    }

    fast_u8 on_get_b() const { return state.b; }
    void on_set_b(fast_u8 n) { state.b = n; }

    fast_u8 on_get_c() const { return state.c; }
    void on_set_c(fast_u8 n) { state.c = n; }

    fast_u8 on_get_d() const { return state.d; }
    void on_set_d(fast_u8 n) { state.d = n; }

    fast_u8 on_get_e() const { return state.e; }
    void on_set_e(fast_u8 n) { state.e = n; }

    fast_u8 on_get_h() const { return state.h; }
    void on_set_h(fast_u8 n) { state.h = n; }

    fast_u8 on_get_l() const { return state.l; }
    void on_set_l(fast_u8 n) { state.l = n; }

    fast_u8 on_get_a() const { return state.a; }
    void on_set_a(fast_u8 n) { state.a = n; }

    fast_u8 on_get_f() const { return state.f; }
    void on_set_f(fast_u8 n) { state.f = n; }

    fast_u16 on_get_bc() const { return make16(state.b, state.c); }
    void on_set_bc(fast_u16 n) { split16(state.b, state.c, n); }

    fast_u16 on_get_de() const { return make16(state.d, state.e); }
    void on_set_de(fast_u16 n) { split16(state.d, state.e, n); }

    fast_u16 on_get_hl() const { return make16(state.h, state.l); }
    void on_set_hl(fast_u16 n) { split16(state.h, state.l, n); }

    fast_u16 on_get_af() const { return make16(state.a, state.f); }
    void on_set_af(fast_u16 n) { split16(state.a, state.f, n); }

    fast_u16 on_get_pc() const { return state.pc; }
    void on_set_pc(fast_u16 n) { state.pc = n; }

    fast_u16 on_get_sp() const { return state.sp; }
    void on_set_sp(fast_u16 n) { state.sp = n; }

    fast_u16 on_get_wz() const { return state.wz; }
    void on_set_wz(fast_u16 n) { state.wz = n; }

    fast_u16 on_get_last_read_addr() const { return state.last_read_addr; }
    void on_set_last_read_addr(fast_u16 n) { state.last_read_addr = n; }

    bool on_is_int_disabled() const { return state.int_disabled; }
    void on_set_is_int_disabled(bool f) { state.int_disabled = f; }

    bool on_is_halted() const { return state.halted; }
    void on_set_is_halted(bool f) { state.halted = f; }

    void on_ex_de_hl_regs() {
        std::swap(state.d, state.h);
        std::swap(state.e, state.l);
    }

    void on_tick(unsigned t) {
        base::on_tick(t);

        // Handle stopping by hitting a specified number of ticks.
        if(state.ticks_to_stop) {
            if(state.ticks_to_stop > t) {
                state.ticks_to_stop -= t;
            } else {
                state.ticks_to_stop = 0;
                self().on_raise_events(z80::events_mask::ticks_limit_hit);
            }
        }
    }

#if 0  // TODO
    void on_step() {
        fprintf(stderr, "PC=%04x SP=%04x BC=%04x DE=%04x HL=%04x AF=%04x %02x%02x%02x%02x\n",
                static_cast<unsigned>(state.pc),
                static_cast<unsigned>(state.sp),
                static_cast<unsigned>(make16(state.b, state.c)),
                static_cast<unsigned>(make16(state.d, state.e)),
                static_cast<unsigned>(make16(state.h, state.l)),
                static_cast<unsigned>(make16(state.a, state.f)),
                static_cast<unsigned>(state.memory[(state.pc + 0) & 0xffff]),
                static_cast<unsigned>(state.memory[(state.pc + 1) & 0xffff]),
                static_cast<unsigned>(state.memory[(state.pc + 2) & 0xffff]),
                static_cast<unsigned>(state.memory[(state.pc + 3) & 0xffff]));
        base::on_step();
    }
#endif

protected:
    using base::self;
    machine_state state;

private:
    PyObject *on_read_callback = nullptr;
    PyObject *on_write_callback = nullptr;
    PyObject *on_input_callback = nullptr;
    PyObject *on_output_callback = nullptr;
    PyObject *on_reti_callback = nullptr;
    PyObject *on_get_int_vector_callback = nullptr;
};

static const unsigned max_instr_size = 4;

static constexpr inline unsigned get_char_code(char c) {
    return static_cast<unsigned char>(c);
}

template<typename B>
class disasm_base : public B {
public:
    typedef B base;

    disasm_base() {}

    const char *get_output() const {
        return output_buff;
    }

    void on_emit(const char *out) {
        std::snprintf(output_buff, max_output_buff_size, "%s", out);
    }

    void on_format_char(char c, const void **&args,
                        typename base::output_buff &out) {
        unsigned n = get_char_code(c);
        if(get_char_code('A') <= n && n <= get_char_code('Z'))
            out.append(c);
        base::on_format_char(c, args, out);
    }

    fast_u8 on_read_next_byte() {
        assert(index < instr_size);
        return instr_code[index++];
    }

    void set_instr_code(const least_u8 *code, unsigned size) {
        assert(size <= max_instr_size);
        std::memset(instr_code, 0, max_instr_size);
        std::memcpy(instr_code, code, size);
        index = 0;
        output_buff[0] = '\0';
    }

    unsigned get_num_of_consumed_bytes() const {
        return index;
    }

private:
    unsigned index = 0;
    least_u8 instr_code[max_instr_size];

    static const std::size_t max_output_buff_size = 32;
    char output_buff[max_output_buff_size];
};

namespace i8080_machine {
#define I8080_MACHINE
#include "machine.inc"
#undef I8080_MACHINE
}

namespace z80_machine {
#define Z80_MACHINE
#include "machine.inc"
#undef Z80_MACHINE
}

static PyModuleDef module = {
    PyModuleDef_HEAD_INIT,      // m_base
    "z80._z80",                 // m_name
    "Z80 Machine Emulation Module",
                                // m_doc
    -1,                         // m_size
    nullptr,                    // m_methods
    nullptr,                    // m_slots
    nullptr,                    // m_traverse
    nullptr,                    // m_clear
    nullptr,                    // m_free
};

}  // anonymous namespace

extern "C" PyMODINIT_FUNC PyInit__z80(void) {
    PyObject *m = PyModule_Create(&module);
    if(!m)
        return nullptr;

    if(PyType_Ready(&z80_machine::type_object) < 0)
        return nullptr;
    if(PyType_Ready(&i8080_machine::type_object) < 0)
        return nullptr;
    Py_INCREF(&z80_machine::type_object);
    Py_INCREF(&i8080_machine::type_object);

    // TODO: Check the returning value.
    PyModule_AddObject(m, "_Z80Machine",
                       &z80_machine::type_object.ob_base.ob_base);
    PyModule_AddObject(m, "_I8080Machine",
                       &i8080_machine::type_object.ob_base.ob_base);
    return m;
}
