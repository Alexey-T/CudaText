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
