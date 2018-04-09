import os

import IPython

EXISTING_KERNEL = True

open(f'{os.environ["HOME"]}/.pynt', 'a').close()
IPython.start_kernel(user_ns={**locals(), **globals(), **vars()})
