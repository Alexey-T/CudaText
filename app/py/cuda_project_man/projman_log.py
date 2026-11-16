"""Central error reporting for the Project Manager plugin.

Policy (details in readme/doc_for_dev.md): a crash must never be
swallowed silently. Where a handler has a reason to keep the plugin
alive (background thread, paint callback, per-keystroke refresh), it
calls log_error() - the full traceback goes to the console and a
one-line hint to the status bar - so the crash is visible and fixable.
Where no such reason exists, the handler is either narrow (it catches
only the expected error type and falls back on purpose) or removed
entirely (CudaText itself shows uncaught plugin errors in a message
box with the traceback). Places that stay silent ON PURPOSE carry a
'# intentional silence:' comment with the reason.
"""

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
