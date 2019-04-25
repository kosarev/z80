
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
    least_u8 f = 0;
    least_u8 a = 0;

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

    least_u16 pc = 0;
    least_u16 sp = 0;
    least_u16 wz = 0;
    least_u16 last_read_addr = 0;

    least_u8 r = 0;
    least_u8 i = 0;
    least_u8 iff1 = 0;
    least_u8 iff2 = 0;

    least_u8 int_mode = 0;
    least_u8 index_rp_kind = 0;
    least_u8 int_disabled = 0;
    least_u8 halted = 0;

    least_u8 memory[z80::address_space_size] = {};
};

class machine_object
    : public machine<z80::machine_state<z80::z80_executor<
                         z80::z80_decoder<z80::root<machine_object>>>>,
                     object_state>
{};

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

static PyObject *get_state_image(PyObject *self, PyObject *args) {
    auto &state = cast_machine(self).get_state();
    return PyMemoryView_FromMemory(reinterpret_cast<char*>(&state),
                                   sizeof(state), PyBUF_WRITE);
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

static PyObject *on_handle_active_int(PyObject *self, PyObject *args) {
    bool int_initiated = cast_machine(self).on_handle_active_int();
    return PyBool_FromLong(int_initiated);
}

static PyMethodDef methods[] = {
    {"get_state_image", get_state_image, METH_NOARGS,
     "Return a MemoryView object that exposes the internal state of the "
     "emulated machine."},
    {"set_input_callback", set_input_callback, METH_VARARGS,
     "Set a callback function handling reading from ports."},
    {"run", run, METH_NOARGS,
     "Run emulator until one or several events are signaled."},
    {"on_handle_active_int", on_handle_active_int, METH_NOARGS,
     "Attempts to initiate a masked interrupt."},
    { nullptr }  // Sentinel.
};

static PyObject *object_new(PyTypeObject *type, PyObject *args,
                            PyObject *kwds) {
    if(!PyArg_ParseTuple(args, ":_Z80Machine.__new__"))
        return nullptr;

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
