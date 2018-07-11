import inspect
import logging
import traceback

from IPython.core.ultratb import AutoFormattedTB

from .manager import ExternalIPythonKernelManager


def handle_exception(type, value, tb):
    # Print stack trace.
    info = traceback.format_exception(type, value, tb)
    logging.error(''.join(info))

    # Get local variables.
    while True:
        if not tb.tb_next:
            break
        next_frame = traceback.extract_tb(tb)[1]
        if next_frame.filename.startswith('/'): # not our code
            break
        tb = tb.tb_next
    local_vars = tb.tb_frame.f_locals

    # Start kernel.
    import os
    pid = os.fork()
    if pid == 0:
        open(f"{os.environ['HOME']}/.pynt", 'a').close()
        import IPython
        IPython.start_kernel(user_ns=local_vars)
    os.waitpid(pid, 0)

def register():
    import sys
    sys.excepthook = handle_exception
