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


def editor_default_metrics():
    """(cell_h, line_pitch) - the REAL rendered sizes, in RUNTIME
    pixels, of a dialog EDITOR control whose font the plugin does NOT
    override. The engine creates every editor control with the USER'S
    EDITOR FONT (proc_customdialog.pas DoControl_ApplyEditorProps:
    Font.Name/Size := EditorOps.OpFontName/OpFontSize), and multiline
    'editor' controls additionally get the user's editor LINE SPACING
    (proc_editor.pas EditorApplyOps: OptSpacingTop/Bottom) - often
    much taller than the GUI 'edit' height (app_proc(
    PROC_GET_GUI_HEIGHT, 'edit')) the layouts otherwise base their
    row heights on: with a big editor font the text touches the box
    edges or gets eaten, and a 5-line box shrinks to 1-2 visible
    lines.

    Measured on the MAIN editor, which renders with the same options:
    the char cell via PROP_CELL_SIZE, the full line pitch (font +
    spacing) via CONVERT_CARET_TO_PIXELS of two lines (skipped when
    word-wrap is on - a wrapped first line would inflate the pitch,
    and then the cell is used as the pitch). Returns (None, None)
    when nothing can be measured (no editor open, API unavailable -
    the stub of the test suites); callers then keep their base
    heights."""
    try:
        from cudatext import (ed, PROP_CELL_SIZE, PROP_WRAP,
                              CONVERT_CARET_TO_PIXELS)
        cell = ed.get_prop(PROP_CELL_SIZE)
        if not cell or int(cell[1]) <= 0:
            return None, None
        cell_h = int(cell[1])
        pitch_h = cell_h
        if not ed.get_prop(PROP_WRAP):
            p0 = ed.convert(CONVERT_CARET_TO_PIXELS, 0, 0)
            p1 = ed.convert(CONVERT_CARET_TO_PIXELS, 0, 1)
            if p0 and p1:
                pitch_h = max(cell_h, int(p1[1]) - int(p0[1]))
        return cell_h, pitch_h
    except Exception:
        # intentional silence: no measurable editor font - the callers
        # keep their base heights (documented above)
        return None, None
