
import typing

class _Z80Machine:
    def get_state_view(self) -> memoryview:
        ...

    def mark_addrs(self, addr: int, size: int, marks: int) -> None:
        ...

    def unmark_addrs(self, addr: int, size: int, marks: int) -> None:
        ...

    def set_read_callback(self, callback: typing.Callable[[int], int]) -> (
            typing.Callable[[int], int]):
        ...

    def set_write_callback(self,
                           callback: typing.Callable[[int], None]) -> (
            typing.Callable[[int], None]):
        ...

    def set_input_callback(self, callback: typing.Callable[[int], int]) -> (
            typing.Callable[[int], int]):
        ...

    def set_output_callback(self,
                            callback: typing.Callable[[int], None]) -> (
            typing.Callable[[int], None]):
        ...

    # TODO: Fix types.
    def set_reti_callback(self,
                          callback: typing.Callable[[int], None]) -> (
            typing.Callable[[int], None]):
        ...

    # TODO: Fix types.
    def set_get_int_vector_callback(
        self,
        callback: typing.Callable[[int], None]) -> (
            typing.Callable[[int], None]):
        ...

    def run(self) -> int:
        ...

    def on_handle_active_int(self) -> None:
        ...

    @staticmethod
    def _disasm(b: bytes) -> tuple[str, int]:
        ...


class _I8080Machine:
    def get_state_view(self) -> memoryview:
        ...

    def mark_addrs(self, addr: int, size: int, marks: int) -> None:
        ...

    def unmark_addrs(self, addr: int, size: int, marks: int) -> None:
        ...

    def set_read_callback(self, callback: typing.Callable[[int], int]) -> (
            typing.Callable[[int], int]):
        ...

    def set_write_callback(self,
                           callback: typing.Callable[[int], None]) -> (
            typing.Callable[[int], None]):
        ...

    def set_input_callback(self, callback: typing.Callable[[int], int]) -> (
            typing.Callable[[int], int]):
        ...

    def set_output_callback(self,
                            callback: typing.Callable[[int], None]) -> (
            typing.Callable[[int], None]):
        ...

    # TODO: Fix types.
    def set_reti_callback(self,
                          callback: typing.Callable[[int], None]) -> (
            typing.Callable[[int], None]):
        ...

    # TODO: Fix types.
    def set_get_int_vector_callback(
        self,
        callback: typing.Callable[[int], None]) -> (
            typing.Callable[[int], None]):
        ...

    def run(self) -> int:
        ...

    @staticmethod
    def _disasm(b: bytes) -> tuple[str, int]:
        ...
