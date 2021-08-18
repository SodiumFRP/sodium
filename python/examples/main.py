from pathlib import Path
import sys
import threading
from threading import Event, Thread
from time import sleep
import tkinter as tk
from tkinter import ttk
from tkinter.scrolledtext import ScrolledText
from typing import Any, Callable, Dict, Optional, Sequence, Tuple, TypeVar

sys.path.append(str(Path(__file__).parent.parent))
from sodiumfrp import Cell, CellLoop, CellSink, Stream, StreamSink, Transaction
from sodiumfrp.time import TimerSystem

T = TypeVar("T")

def main() -> None:
    timer_system = TimerSystem()
    ui = build_ui()
    style = ttk.Style(ui["root"])

    def initial_transaction() -> None:
        themes_combobox = sodium_wrap_combobox(
            ui["themes_combobox"],
            items=Cell.constant(style.theme_names()))
        themes_combobox["choice"].listen(
            lambda t: update_ui(ui["root"], style.theme_use, t))

        just_button = sodium_wrap_button(ui["just_button"])

        async_request_button_enabled: CellLoop[bool] = CellLoop()
        async_request_button = sodium_wrap_button(
            ui["async_request_button"],
            enabled=async_request_button_enabled)
        def async_request() -> str:
            sleep(1.0)
            return "Result of async request"
        async_request_result = async_request_button["clicked"] \
            .map(lambda _: request_async(async_request)) \
            .hold(Stream.never()) \
            .switch()
        async_request_button_enabled.loop(
            Stream.or_else_(
                async_request_button["clicked"].map_to(False),
                async_request_result.map_to(True),
            ).hold(True)
        )

        timer_button_text: CellLoop[str] = CellLoop()
        timer_button = sodium_wrap_button(
            ui["timer_button"],
            text=timer_button_text)
        timer_running = timer_button["clicked"] \
            .accum(False, lambda _, s: not s)
        timer_button_text.loop(
            timer_running.map(
                lambda x: "Star timer" if not x else "Stop timer")
        )
        timer_events = timer_button["clicked"] \
            .map(lambda _:
                periodic_timer(timer_system, 300, while_=timer_running)) \
            .hold(Stream.never()) \
            .switch()

        append_log = Stream.or_else_(
            just_button["clicked"].map_to("Just a message"),
            async_request_result,
            timer_events.map(lambda x: f"Timer event: {x}"),
        )

        log_text = sodium_wrap_text(ui["log_text"],
            insert=append_log.map(lambda msg: (tk.END, f"{msg}\n", True)))

    Transaction.run(initial_transaction)

    ui["root"].mainloop()

def build_ui() -> Dict[str, Any]:
    root = tk.Tk()
    root.title("Hello, World!")
    style = ttk.Style(root)

    root_frame = ttk.Frame(root)
    root_frame.pack(padx=7, pady=7, expand=True, fill=tk.BOTH)

    themes_frame = ttk.Frame(root_frame)
    themes_frame.pack(fill=tk.BOTH)

    theme_label = ttk.Label(themes_frame, text="Theme:")
    theme_label.pack(side=tk.LEFT, padx=3, pady=3, fill=tk.BOTH)

    themes_combobox = ttk.Combobox(themes_frame, state="readonly")
    themes_combobox.pack(
        side=tk.LEFT, padx=3, pady=3, expand=True, fill=tk.BOTH)

    controls_frame = ttk.Frame(root_frame)
    controls_frame.pack(fill=tk.BOTH)

    just_button = ttk.Button(controls_frame, text="Just a button")
    just_button.pack(side=tk.LEFT, padx=3, pady=3, expand=True, fill=tk.BOTH)

    async_request_button = ttk.Button(controls_frame, text="Async request")
    async_request_button.pack(
        side=tk.LEFT, padx=3, pady=3, expand=True, fill=tk.BOTH)

    timer_button = ttk.Button(controls_frame, text="Timer")
    timer_button.pack(side=tk.LEFT, padx=3, pady=3, expand=True, fill=tk.BOTH)

    log_label = ttk.Label(root_frame, text="Log:")
    log_label.pack(padx=3, pady=3, fill=tk.BOTH)

    log_text = ScrolledText(
        root_frame, width=60, height=10, state=tk.DISABLED)
    log_text.pack(padx=3, pady=3, expand=True, fill=tk.BOTH)

    return {
        "root": root,
        "style": style,
        "themes_combobox": themes_combobox,
        "just_button": just_button,
        "async_request_button": async_request_button,
        "timer_button": timer_button,
        "log_text": log_text,
    }

def sodium_wrap_command(widget: tk.Widget) -> Stream:
    stream: StreamSink = StreamSink()
    def callback() -> None:
        Transaction.post(lambda: stream.send(None))
    widget["command"] = callback
    return stream

def sodium_wrap_event(widget: tk.Widget, event: str) -> Stream:
    stream: StreamSink = StreamSink()
    def callback(*args: Any) -> None:
        Transaction.post(lambda: stream.send(args))
    widget.bind(event, callback)
    return stream

def sodium_wrap_button(
        button: tk.Button,
        enabled: Cell[bool] = None,
        text: Cell[str] = None) -> Dict:
    clicked = sodium_wrap_command(button)

    if enabled is not None:
        def set_state(x: bool) -> None:
            button["state"] = tk.NORMAL if x else tk.DISABLED
        enabled.listen(lambda x: update_ui(button, set_state, x))

    if text is not None:
        def set_text(text_: str) -> None:
            button["text"] = text_
        text.listen(lambda text_: update_ui(button, set_text, text_))

    return  {
        "clicked": clicked,
    }

def sodium_wrap_text(
        widget: tk.Text,
        insert: Stream[Tuple[int, str]]) -> Dict:

    def insert_(args: Tuple) -> None:
        index = args[0]
        text = args[1]
        was_disabled = widget["state"] == tk.DISABLED
        if was_disabled:
            widget["state"] = tk.NORMAL
        widget.insert(index, text)
        if was_disabled:
            widget["state"] = tk.DISABLED
        if len(args) > 2:
            scroll = args[2]
            if scroll:
                widget.see(index)

    insert.listen(lambda x: update_ui(widget, insert_, x))

    return {}

def sodium_wrap_combobox(
        widget: ttk.Combobox,
        items: Cell[Sequence[str]]) -> Dict:

    def set_items(values: Sequence[str]) -> None:
        widget["values"] = values
    items.listen(lambda items_: update_ui(widget, set_items, items_))

    choice = sodium_wrap_event(widget, "<<ComboboxSelected>>") \
        .map(lambda _: widget.get())

    return {
        "choice": choice,
    }

def request_async(request: Callable[[], T]) -> Stream[T]:
    """
    Perform request on a separate thread. Return a stream, that posts
    either the result of the request or an exception thrown by the request.
    """
    stream: StreamSink[T] = StreamSink()
    def thread() -> None:
        try:
            result = request()
        except Exception as exc:
            result = exc
        Transaction.post(lambda: stream.send(result))
    Thread(target=thread).start()
    return stream.once()

def update_ui(widget: tk.Widget, func: Callable, *args: Any) -> None:
    """
    Make UI updates thread-safe, by scheduling them on the main thread
    whenever we are not on the main thread.
    """
    if threading.current_thread() == threading.main_thread():
        func(*args)
    else:
        event = Event()
        def wrapper() -> None:
            try:
                func(*args)
            finally:
                event.set()
        widget.after_idle(wrapper)
        # Wait until UI update finished to ensure consistency between
        # model and view
        event.wait()

def periodic_timer(
        timer_system: TimerSystem,
        period_ms: int,
        while_: Cell[bool]) -> Stream[int]:
    t0 = timer_system.time_ms().sample()

    def update_alarm(t: int) -> Optional[int]:
        flag = while_.sample()
        return t + period_ms if flag else None

    alarm_time: CellLoop[Optional[int]] = CellLoop()
    alarm = timer_system.at(alarm_time)
    alarm_time.loop(alarm.map(update_alarm).hold(t0 + period_ms))

    return alarm \
        .snapshot(while_, lambda a, w: a if w else None) \
        .filter(lambda x: x is not None)

if __name__ == "__main__":
    main()
