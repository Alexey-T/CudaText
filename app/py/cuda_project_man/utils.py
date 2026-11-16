import traceback

from cudatext import msg_status


def log_error(context):
    """Report the exception currently being handled (call this only
    from an except block): the full traceback goes to the console,
    and a one-line hint to the status bar. Never raises, so it is safe
    in paint callbacks and thread bodies."""
    text = str(context)
    tb = traceback.format_exc()
    try:
        print(text + '\n' + tb)
    except Exception:
        # intentional silence: printing must never shadow the error
        pass
    try:
        msg_status(text + ' (traceback in console)')
    except Exception:
        # intentional silence: the console line is the important part
        pass


def option_font_size(value):
    """Validated int of an 'ui_font_size'/'edit_font_size' option
    value: a whole number of points in the sane range 5..40. Empty /
    non-numeric / out-of-range -> None, meaning "not set": the
    controls keep the engine's default fonts, exactly the behavior of
    the versions before these options existed."""
    try:
        v = int(str(value).strip())
    except (TypeError, ValueError):
        return None
    return v if 5 <= v <= 40 else None


def font_row_h(base_h, size, ppi=96):
    """Height of a one-line control that must show a `size`-point
    font: `base_h` as-is when no size is set (the standard GUI height
    of such a control), otherwise at least the text line height
    (size x ppi/72 px per point plus ~30% leading) plus the usual
    inner padding - a box with an overridden font GROWS with its font
    and is never left too small for the text.

    The default ppi=96 is the DESIGN-px space of dlg_proc layouts
    (unscaled coordinates that DLG_SCALE scales once at the end): a
    font point is the same at every DPI, so 96/72 px-per-point is the
    right unit there on every screen. The side panel passes its real
    PROC_GET_SYSTEM_PPI instead, because it lays out in runtime
    pixels."""
    if not size:
        return base_h
    return max(base_h, int(size * ppi / 72.0 * 1.3) + 8)
