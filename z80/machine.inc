
/*  Z80 Machine Emulation Module for Python.
    https://github.com/kosarev/z80

    Copyright (C) 2019 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

struct __attribute__((packed)) object_state {
    least_u8 c = 0;
    least_u8 b = 0;
    least_u8 e = 0;
    least_u8 d = 0;

    least_u8 l = 0;
    least_u8 h = 0;
    least_u8 f = 2;  // TODO
    least_u8 a = 0;

    least_u16 pc = 0;
    least_u16 sp = 0;
    least_u16 wz = 0;
    least_u16 last_read_addr = 0;

    least_u32 ticks_to_stop = 0;  // Null means no limit.

#if defined(I8080_MACHINE)
    least_u8 iff = 0;
    least_u8 int_disabled = 0;
    least_u8 halted = 0;
    least_u8 padding = 0;
#elif defined(Z80_MACHINE)
    least_u8 ixl = 0;
    least_u8 ixh = 0;
    least_u8 iyl = 0;
    least_u8 iyh = 0;

    least_u8 alt_c = 0;
    least_u8 alt_b = 0;
    least_u8 alt_e = 0;
    least_u8 alt_d = 0;

    least_u8 alt_l = 0;
    least_u8 alt_h = 0;
    least_u8 alt_f = 0;
    least_u8 alt_a = 0;

    least_u8 r = 0;
    least_u8 i = 0;
    least_u8 iff1 = 0;
    least_u8 iff2 = 0;

    least_u8 int_disabled = 0;
    least_u8 halted = 0;
    least_u8 int_mode = 0;
    least_u8 irp_kind = 0;
#else
#error Unknown machine!
#endif

    least_u8 memory[z80::address_space_size] = {};
};

#if defined(I8080_MACHINE)
class machine_object
    : public z80::machine_state<
        machine<z80::i8080_executor<
            z80::i8080_decoder<z80::root<machine_object>>>,
        object_state>>
{};

class disasm : public disasm_base<z80::i8080_disasm<disasm>>
{};
#elif defined(Z80_MACHINE)
class machine_object
    : public z80::machine_state<
        machine<z80::z80_executor<
            z80::z80_decoder<z80::root<machine_object>>>,
        object_state>>
{};

class disasm : public disasm_base<z80::z80_disasm<disasm>> {
public:
    void on_disassemble() {
        base::on_disassemble();
        if(base::get_iregp_kind() != z80::iregp::hl)
            base::on_disassemble();
    }
};
#else
#error Unknown machine!
#endif

struct object_instance {
    PyObject_HEAD
    machine_object machine;
};

static inline object_instance *cast_object(PyObject *p) {
    return reinterpret_cast<object_instance*>(p);
}

static inline machine_object &cast_machine(PyObject *p) {
    return cast_object(p)->machine;
}

static PyObject *get_state_view(PyObject *self, PyObject *args) {
    auto &state = cast_machine(self).get_state();
    return PyMemoryView_FromMemory(reinterpret_cast<char*>(&state),
                                   sizeof(state), PyBUF_WRITE);
}

static PyObject *mark_addrs(PyObject *self, PyObject *args) {
    unsigned addr, size, marks;
    if(!PyArg_ParseTuple(args, "III", &addr, &size, &marks))
        return nullptr;

    cast_machine(self).mark_addrs(addr, size, marks);
    Py_RETURN_NONE;
}

static PyObject *set_input_callback(PyObject *self, PyObject *args) {
    PyObject *new_callback;
    if(!PyArg_ParseTuple(args, "O:set_callback", &new_callback))
        return nullptr;

    if(!PyCallable_Check(new_callback)) {
        PyErr_SetString(PyExc_TypeError, "parameter must be callable");
        return nullptr;
    }

    auto &machine = cast_machine(self);
    PyObject *old_callback = machine.set_input_callback(new_callback);
    Py_XINCREF(new_callback);
    Py_XDECREF(old_callback);
    Py_RETURN_NONE;
}

static PyObject *run(PyObject *self, PyObject *args) {
    auto &machine = cast_machine(self);
    z80::events_mask::type events = machine.on_run();
    if(PyErr_Occurred())
        return nullptr;
    return Py_BuildValue("i", events);
}

#if defined(Z80_MACHINE)
static PyObject *on_handle_active_int(PyObject *self, PyObject *args) {
    bool int_initiated = cast_machine(self).on_handle_active_int();
    return PyBool_FromLong(int_initiated);
}
#endif  // defined(Z80_MACHINE)

static PyObject *disasm_func(PyObject *self, PyObject *args) {
    PyObject *image;
    if(!PyArg_ParseTuple(args, "S:image", &image))
        return nullptr;

    auto image_size = std::min<Py_ssize_t>(PyBytes_GET_SIZE(image),
                                           max_instr_size);
    const char *image_bytes = PyBytes_AS_STRING(image);

    disasm dis;
    dis.set_instr_code(reinterpret_cast<const least_u8*>(image_bytes),
                       image_size);
    dis.on_disassemble();

    decref_guard output(PyUnicode_FromString(dis.get_output()));
    if(!output)
        return nullptr;

    decref_guard size(PyLong_FromSsize_t(dis.get_num_of_consumed_bytes()));
    if(!size)
        return nullptr;

    return Py_BuildValue("OO", output.get(), size.get());
}

static PyMethodDef methods[] = {
    {"get_state_view", get_state_view, METH_NOARGS,
     "Return a MemoryView object that exposes the internal state of the "
     "emulated machine."},
    {"mark_addrs", mark_addrs, METH_VARARGS,
     "Mark a range of memory bytes as ones that require custom "
     "processing on reading, writing or executing them."},
    {"set_input_callback", set_input_callback, METH_VARARGS,
     "Set a callback function handling reading from ports."},
    {"run", run, METH_NOARGS,
     "Run emulator until one or several events are signaled."},
#if defined(Z80_MACHINE)
    {"on_handle_active_int", on_handle_active_int, METH_NOARGS,
     "Attempts to initiate a masked interrupt."},
#endif  // defined(Z80_MACHINE)
    {"_disasm", disasm_func, METH_VARARGS | METH_STATIC,
     "Disassembles passed string of bytes."},
    { nullptr }  // Sentinel.
};

static PyObject *object_new(PyTypeObject *type, PyObject *args,
                            PyObject *kwds) {
    auto *self = cast_object(type->tp_alloc(type, /* nitems= */ 0));
    if(!self)
      return nullptr;

    auto &machine = self->machine;
    ::new(&machine) machine_object();
    return &self->ob_base;
}

static void object_dealloc(PyObject *self) {
    auto &object = *cast_object(self);
    object.machine.~machine_object();
    Py_TYPE(self)->tp_free(self);
}

static PyTypeObject type_object = {
    PyVarObject_HEAD_INIT(&PyType_Type, 0)

#if defined(I8080_MACHINE)
    "z80._z80._I8080Machine",   // tp_name
#elif defined(Z80_MACHINE)
    "z80._z80._Z80Machine",     // tp_name
#else
#error Unknown machine!
#endif

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

#if defined(I8080_MACHINE)
    "i8080 Machine Emulator",   // tp_doc
#elif defined(Z80_MACHINE)
    "Z80 Machine Emulator",     // tp_doc
#else
#error Unknown machine!
#endif

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
