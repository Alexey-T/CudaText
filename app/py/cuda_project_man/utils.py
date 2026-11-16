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


def font_size_to_cell(fs, ppi=96):
    """Estimated char cell height (px) of an `fs`-point font: ~1.25
    line-height, so a 10 pt font is ~17 px at 96 ppi (the calibration
    of the '5 lines ~ 92 px' recent box). For when the pixel cell
    cannot be measured - nothing is RENDERED yet, e.g. while CudaText
    starts up - while the font SIZE (points) is a plain property,
    readable at any time. None for a non-positive size."""
    if not fs or fs <= 0:
        return None
    return int(fs * ppi / 72.0 * 1.25)


def app_font_scale():
    """The app's GLOBAL font scale in percent (100 = default) - the
    scale dialog editor controls render at (they have no per-editor
    zoom; real editors' zoom is PROP_SCALE_FONT instead).
    app_proc(PROC_CONFIG_SCALE_GET, '') gives (ui_scale, font_scale),
    but the shape is not guaranteed everywhere: anything unexpected
    (a bare int, a "ui,font" string, nothing readable at all) safely
    falls back to 100."""
    try:
        from cudatext import app_proc, PROC_CONFIG_SCALE_GET
        v = app_proc(PROC_CONFIG_SCALE_GET, '')
        if isinstance(v, (tuple, list)):
            if len(v) >= 2:
                return int(v[1])
            if v:
                return int(v[0])
        if isinstance(v, int):
            return v
        if isinstance(v, str) and v.strip():
            parts = v.replace(',', ' ').split()
            return int(float(parts[1] if len(parts) > 1 else parts[0]))
    except Exception:
        # intentional silence: no readable scale - assume 100%
        pass
    return 100


#: the smallest pixel cell a REAL rendered editor reports: an EMPTY
#: never-painted editor (the state of the current editor at
#: CudaText's startup, before the session's documents exist) answers
#: PROP_CELL_SIZE with garbage like (4, 4) - observed on a real
#: build with a 15 pt editor font whose real cell is 31 px there.
#: Below the floor a cell counts as "not rendered" (a real 5 pt font
#: at 96 dpi already measures ~8 px with the leading), so such
#: garbage cannot win over the font estimates.
MIN_PLAUSIBLE_CELL = 8


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
    and then the cell is used as the pitch). When nothing is RENDERED
    - the side panel is built at CudaText's startup, when the current
    editor may not exist or may not have painted, and an empty
    never-painted editor even answers PROP_CELL_SIZE with garbage
    like (4, 4), which counts as unavailable below
    MIN_PLAUSIBLE_CELL - the font is read DIRECTLY instead: the SIZE
    via PROP_FONT (a plain property, readable at any time; the engine
    assigns the raw points to every editor control -
    EditorApplyOps/DoControl_ApplyEditorProps) scaled by the font
    scale the engine applies for rendering (the per-editor zoom
    PROP_SCALE_FONT, 0 = not zoomed; else the app's global font scale
    PROC_CONFIG_SCALE_GET - the same arithmetic as the engine's own
    listbox row, AppListboxItemHeight in proc_globdata.pas) -
    yielding an ESTIMATED cell (font_size_to_cell; the pitch then
    equals the cell - the line spacing needs rendering, and only the
    on-demand dialogs need the pitch, never built at startup).
    Returns (None, None) when nothing can be read (no editor open,
    API unavailable - the stub of the test suites); callers then keep
    their base heights."""
    try:
        from cudatext import (ed, app_proc, CONVERT_CARET_TO_PIXELS,
                              PROP_CELL_SIZE, PROP_FONT, PROP_SCALE_FONT,
                              PROP_WRAP, PROC_GET_SYSTEM_PPI)
        cell = ed.get_prop(PROP_CELL_SIZE)
        if not cell or int(cell[1]) < MIN_PLAUSIBLE_CELL:
            # the pixel measures need a RENDERED editor - at
            # CudaText's startup, when the side panel is built, there
            # may be no document yet or none painted - and an EMPTY
            # never-painted editor even answers PROP_CELL_SIZE with
            # garbage like (4, 4) (observed on a real build with a
            # 15 pt editor font; below MIN_PLAUSIBLE_CELL that counts
            # as not rendered, or it would win over the font
            # estimate). The font SIZE is
            # a plain property readable at any time, and the engine
            # scales it for rendering: the per-editor zoom
            # (PROP_SCALE_FONT; 0 = not zoomed) or else the app's
            # global font scale (PROC_CONFIG_SCALE_GET)
            try:
                fs = int(ed.get_prop(PROP_FONT)[1])
                scale = int(ed.get_prop(PROP_SCALE_FONT) or 0)
                if scale <= 0:
                    scale = app_font_scale()
                ppi = int(app_proc(PROC_GET_SYSTEM_PPI, '')) or 96
                cell_h = font_size_to_cell(fs * scale / 100.0, ppi)
            except Exception:
                # intentional silence: no readable font/size/scale -
                # the callers' base heights stay
                cell_h = None
            return cell_h, cell_h
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
