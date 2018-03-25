"""

Kernel manager for connecting to a IPython kernel started outside of Jupyter.

Use this kernel manager if you want to connect a Jupyter notebook to a IPython
kernel started outside of Jupyter.

"""

import glob
import os
import os.path
import re

from notebook.services.kernels.kernelmanager import MappingKernelManager
from tornado import gen
from tornado.concurrent import Future
from tornado.ioloop import IOLoop


class ExternalIPythonKernelManager(MappingKernelManager):
    """A Kernel manager that connects to a IPython kernel started outside of Jupyter"""

    def _get_latest_kernel_id(self):
        """Return the ID of the most recent kernel that was launched

        This ID is assumed to be the ID of the kernel which was launched via an
        external IPython process.

        Args:
            runtime_dir (str): the directory where kernel files are stored

        Returns:
            kid (int): the ID of the kernel file which was modified most recently

        >>> self = ExternalIPythonKernelManager()

        """
        conn_fnames = glob.glob(f'{self.connection_dir}/kernel-*.json')
        p = '.*kernel-(?P<kid>\d+).json'
        conn_fnames = [conn_fname for conn_fname in conn_fnames for m in [re.match(p, conn_fname)] if m]
        latest_conn_fname = max(conn_fnames, key=os.path.getctime)
        kid = re.match(p, latest_conn_fname).group('kid')
        return kid

    def _attach_to_latest_kernel(self, kernel_id):
        """Attch to the latest externally started IPython kernel

        Args:
            kernel_id (str): ID for this kernel_id

        >>> self = ExternalIPythonKernelManager()
        >>> self.connection_dir = '/Users/ebanner/Library/Jupyter/runtime'
        >>> self.start_kernel()
        >>> open(f'{self.connection_dir}/.pynt', 'a').close()
        >>> kernel_id, loop = next(iter(self._kernels.items()))

        """
        self.log.info(f'Attaching {kernel_id} to an existing kernel...')
        kernel = self._kernels[kernel_id]
        port_names = ['shell_port', 'stdin_port', 'iopub_port', 'hb_port', 'control_port']
        port_names = kernel._random_port_names if hasattr(kernel, '_random_port_names') else port_names
        for port_name in port_names:
            setattr(kernel, port_name, 0)

        # "Connect" to latest kernel started by an external python process
        kid = self._get_latest_kernel_id()
        connection_fname = f'{self.connection_dir}/kernel-{kid}.json'
        self.log.info(f'Latest kernel = {connection_fname} from dir = {self.connection_dir}')
        kernel.load_connection_file(connection_fname)

    def _should_use_existing(self):
        return os.path.isfile(f'{self.connection_dir}/.pynt')

    @gen.coroutine
    def start_kernel(self, **kwargs):
        """Maybe switch to the most recently started kernel

        Start a new kernel like normal. If `self.runtime_dir/.pynt` exists then
        attach to the most recently started kernel.

        Args:
            Arguments to pass to `MappingKernelManager.start_kernel()`

        >>> self = ExternalIPythonKernelManager()
        >>> self.connection_dir = '/Users/ebanner/Library/Jupyter/runtime'
        >>> __class__ = ExternalIPythonKernelManager
        >>> kwargs = {}

        """
        kernel_id = super(ExternalIPythonKernelManager, self).start_kernel(**kwargs).result()
        self._attach_to_latest_kernel(kernel_id)
        raise gen.Return(kernel_id)

    def restart_kernel(self, kernel_id):
        """Maybe switch to the most recently started kernel

        Restart the kernel like normal. If `self.runtime_dir/.pynt` exists then
        attach to the most recently started kernel.

        TODO Most of this code is copied straight from
        `MappingKernelManager.restart_kernel()`. Figure out what subset of it
        is needed for this to work.

        """
        self._check_kernel_id(kernel_id)
        super(MappingKernelManager, self).restart_kernel(kernel_id)
        kernel = self.get_kernel(kernel_id)
        # return a Future that will resolve when the kernel has successfully restarted
        channel = kernel.connect_shell()
        future = Future()

        def finish():
            """Common cleanup when restart finishes/fails for any reason."""
            if not channel.closed():
                channel.close()
            loop.remove_timeout(timeout)
            kernel.remove_restart_callback(on_restart_failed, 'dead')
            self._attach_to_latest_kernel(kernel_id)

        def on_reply(msg):
            self.log.debug("Kernel info reply received: %s", kernel_id)
            finish()
            if not future.done():
                future.set_result(msg)

        def on_timeout():
            self.log.warning("Timeout waiting for kernel_info_reply: %s", kernel_id)
            finish()
            if not future.done():
                future.set_exception(gen.TimeoutError("Timeout waiting for restart"))

        def on_restart_failed():
            self.log.warning("Restarting kernel failed: %s", kernel_id)
            finish()
            if not future.done():
                future.set_exception(RuntimeError("Restart failed"))

        kernel.add_restart_callback(on_restart_failed, 'dead')
        kernel.session.send(channel, "kernel_info_request")
        channel.on_recv(on_reply)
        loop = IOLoop.current()
        timeout = loop.add_timeout(loop.time() + 30, on_timeout)
        return future
