
/*  Z80 Machine Emulation Module for Python.

    Copyright (C) 2019 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include <Python.h>

#include <new>

#include "../z80.h"

namespace {

using z80::fast_u8;
using z80::fast_u16;
using z80::fast_u32;
using z80::least_u8;
using z80::least_u16;

using z80::unused;

class decref_guard {
public:
    decref_guard(PyObject *object)
        : object(object)
    {}

    ~decref_guard() {
        Py_XDECREF(object);
    }

private:
    PyObject *object;
};

// TODO: Should be part of z80.h?
struct __attribute__((packed)) processor_state {
    least_u16 bc;
    least_u16 de;
    least_u16 hl;
    least_u16 af;
    least_u16 ix;
    least_u16 iy;

    least_u16 alt_bc;
    least_u16 alt_de;
    least_u16 alt_hl;
    least_u16 alt_af;

    least_u16 pc;
    least_u16 sp;
    least_u16 ir;
    least_u16 memptr;

    least_u8 iff1;
    least_u8 iff2;
    least_u8 int_mode;
    least_u8 index_rp_kind;
};

struct __attribute__((packed)) memory_image {
    static const z80::size_type size = 0x10000;  // 64K bytes.
    least_u8 bytes[size];

    memory_image() {
        uint_fast32_t rnd = 0xde347a01;
        for(auto &cell : bytes) {
            cell = static_cast<least_u8>(rnd);
            rnd = (rnd * 0x74392cef) ^ (rnd >> 16);
        }
    }
};

struct __attribute__((packed)) machine_state {
    struct processor_state proc;
    memory_image memory;
};

class z80_machine : public z80::processor<z80_machine> {
public:
    typedef z80::processor<z80_machine> base;

    z80_machine() {
        retrieve_state();
    }

    machine_state &get_machine_state() {
        return state;
    }

    void retrieve_state() {
        state.proc = get_processor_state();
    }

    void install_state() {
        set_processor_state(state.proc);
    }

    memory_image &get_memory() { return state.memory; }

    void tick(unsigned t) {
        unused(t);
    }

    fast_u8 on_read_access(fast_u16 addr) {
        assert(addr < memory_image::size);
        return state.memory.bytes[addr];
    }

    void on_write_access(fast_u16 addr, fast_u8 n) {
        assert(addr < memory_image::size);
        state.memory.bytes[addr] = n;
    }

    fast_u8 on_input(fast_u16 addr) {
        const fast_u8 default_value = 0xbf;
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

    void run() {
        install_state();

        // Execute some portion of instructions.
        for(unsigned i = 0; i != 5000; ++i)
            base::step();

        retrieve_state();
    }

    bool handle_active_int() {
        install_state();
        bool int_initiated = base::handle_active_int();
        retrieve_state();
        return int_initiated;
    }

    PyObject *set_on_input_callback(PyObject *callback) {
        PyObject *old_callback = on_input_callback;
        on_input_callback = callback;
        return old_callback;
    }

protected:
    processor_state get_processor_state() {
        processor_state state;

        state.bc = get_bc();
        state.de = get_de();
        state.hl = get_hl();
        state.af = get_af();
        state.ix = get_ix();
        state.iy = get_iy();

        state.alt_bc = get_alt_bc();
        state.alt_de = get_alt_de();
        state.alt_hl = get_alt_hl();
        state.alt_af = get_alt_af();

        state.pc = get_pc();
        state.sp = get_sp();
        state.ir = get_ir();
        state.memptr = get_memptr();

        state.iff1 = get_iff1() ? 1 : 0;
        state.iff2 = get_iff2() ? 1 : 0;
        state.int_mode = get_int_mode();
        state.index_rp_kind = static_cast<least_u8>(get_index_rp_kind());

        return state;
    }

    void set_processor_state(const processor_state &state) {
        set_bc(state.bc);
        set_de(state.de);
        set_hl(state.hl);
        set_af(state.af);
        set_ix(state.ix);
        set_iy(state.iy);

        set_alt_bc(state.alt_bc);
        set_alt_de(state.alt_de);
        set_alt_hl(state.alt_hl);
        set_alt_af(state.alt_af);

        set_pc(state.pc);
        set_sp(state.sp);
        set_ir(state.ir);
        set_memptr(state.memptr);

        set_iff1(state.iff1);
        set_iff2(state.iff2);
        set_int_mode(state.int_mode);
        set_index_rp_kind(static_cast<z80::index_regp>(state.index_rp_kind));
    }

private:
    machine_state state;
    PyObject *on_input_callback = nullptr;
};

struct object_instance {
    PyObject_HEAD
    z80_machine machine;
};

static inline object_instance *cast_object(PyObject *p) {
    return reinterpret_cast<object_instance*>(p);
}

static inline z80_machine &cast_machine(PyObject *p) {
    return cast_object(p)->machine;
}

PyObject *get_state_image(PyObject *self, PyObject *args) {
    auto &state = cast_machine(self).get_machine_state();
    return PyMemoryView_FromMemory(reinterpret_cast<char*>(&state),
                                   sizeof(state), PyBUF_WRITE);
}

PyObject *get_memory(PyObject *self, PyObject *args) {
    auto &memory = cast_machine(self).get_memory();
    return PyMemoryView_FromMemory(reinterpret_cast<char*>(memory.bytes),
                                   sizeof(memory), PyBUF_WRITE);
}

static PyObject *set_on_input_callback(PyObject *self, PyObject *args) {
    PyObject *new_callback;
    if(!PyArg_ParseTuple(args, "O:set_callback", &new_callback))
        return nullptr;

    if(!PyCallable_Check(new_callback)) {
        PyErr_SetString(PyExc_TypeError, "parameter must be callable");
        return nullptr;
    }

    auto &machine = cast_machine(self);
    PyObject *old_callback = machine.set_on_input_callback(new_callback);
    Py_XINCREF(new_callback);
    Py_XDECREF(old_callback);
    Py_RETURN_NONE;
}

PyObject *run(PyObject *self, PyObject *args) {
    auto &machine = cast_machine(self);
    machine.run();
    if(PyErr_Occurred())
        return nullptr;
    Py_RETURN_NONE;
}

PyObject *handle_active_int(PyObject *self, PyObject *args) {
    bool int_initiated = cast_machine(self).handle_active_int();
    return PyBool_FromLong(int_initiated);
}

PyMethodDef methods[] = {
    {"get_state_image", get_state_image, METH_NOARGS,
     "Return a MemoryView object that exposes the internal state of the "
     "simulated machine."},
    {"get_memory", get_memory, METH_NOARGS,
     "Return a MemoryView object that exposes the memory of the simulated "
     "machine."},
    {"set_on_input_callback", set_on_input_callback, METH_VARARGS,
     "Set a callback function handling reading from ports."},
    {"run", run, METH_NOARGS,
     "Run emulator until one or several events are signaled."},
    {"handle_active_int", handle_active_int, METH_NOARGS,
     "Attempts to initiate a masked interrupt."},
    { nullptr }  // Sentinel.
};

PyObject *object_new(PyTypeObject *type, PyObject *args, PyObject *kwds) {
    if(!PyArg_ParseTuple(args, ":Spectrum48Base.__new__"))
        return nullptr;

    auto *self = cast_object(type->tp_alloc(type, /* nitems= */ 0));
    if(!self)
      return nullptr;

    auto &machine = self->machine;
    ::new(&machine) z80_machine();
    return &self->ob_base;
}

void object_dealloc(PyObject *self) {
    auto &object = *cast_object(self);
    object.machine.~z80_machine();
    Py_TYPE(self)->tp_free(self);
}

static PyTypeObject type_object = {
    PyVarObject_HEAD_INIT(&PyType_Type, 0)
    "z80._z80._Z80Machine",
                                // tp_name
    sizeof(object_instance),    // tp_basicsize
    0,                          // tp_itemsize
    object_dealloc,             // tp_dealloc
    0,                          // tp_print
    0,                          // tp_getattr
    0,                          // tp_setattr
    0,                          // tp_reserved
    0,                          // tp_repr
    0,                          // tp_as_number
    0,                          // tp_as_sequence
    0,                          // tp_as_mapping
    0,                          // tp_hash
    0,                          // tp_call
    0,                          // tp_str
    0,                          // tp_getattro
    0,                          // tp_setattro
    0,                          // tp_as_buffer
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
                                // tp_flags
    "Z80 Machine Emulator",     // tp_doc
    0,                          // tp_traverse
    0,                          // tp_clear
    0,                          // tp_richcompare
    0,                          // tp_weaklistoffset
    0,                          // tp_iter
    0,                          // tp_iternext
    methods,                    // tp_methods
    nullptr,                    // tp_members
    0,                          // tp_getset
    0,                          // tp_base
    0,                          // tp_dict
    0,                          // tp_descr_get
    0,                          // tp_descr_set
    0,                          // tp_dictoffset
    0,                          // tp_init
    0,                          // tp_alloc
    object_new,                 // tp_new
    0,                          // tp_free
    0,                          // tp_is_gc
    0,                          // tp_bases
    0,                          // tp_mro
    0,                          // tp_cache
    0,                          // tp_subclasses
    0,                          // tp_weaklist
    0,                          // tp_del
    0,                          // tp_version_tag
    0,                          // tp_finalize
};

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

    if(PyType_Ready(&type_object) < 0)
        return nullptr;
    Py_INCREF(&type_object);

    // TODO: Check the returning value.
    PyModule_AddObject(m, "_Z80Machine",
                       &type_object.ob_base.ob_base);
    return m;
}
