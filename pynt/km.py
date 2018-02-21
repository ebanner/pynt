import glob
import os
import re

from tornado import gen

from notebook.services.kernels.kernelmanager import MappingKernelManager


class ExistingMappingKernelManager(MappingKernelManager):
    """A KernelManager that connects to an kernel started outside of Jupyter"""

    def _get_latest_kernel_id(self):
        """Return the ID of the most recent kernel that was launched

        This ID is assumed to be the ID of the kernel which was launched via an
        external IPython process.

        Args:
            runtime_dir (str): the directory where kernel files are stored

        Returns:
            kid (int): the ID of the kernel file which was modified most recently

        """
        conn_fnames = glob.glob(f'{self.connection_dir}/kernel-*.json')
        p = '.*kernel-(?P<kid>\d+).json'
        conn_fnames = [conn_fname for conn_fname in conn_fnames for m in [re.match(p, conn_fname)] if m]
        latest_conn_fname = max(conn_fnames, key=os.path.getctime)
        kid = re.match(p, latest_conn_fname).group('kid')
        return kid

    @gen.coroutine
    def start_kernel(self, **kwargs):
        """Attach to the most recently started kernel

        This function is a hack. It spins up a new kernel through the call to
        `super().start_kernel(...)` but then turns its attention to the kernel
        which was started by an external python process. Kernel restarts will
        restart the useless kernel and leave the existing kernel alone.

        Args:
            Arguments to pass to `MappingKernelManager.start_kernel()`

        """
        # start a new kernel with ipykernel
        kernel_id = super().start_kernel(**kwargs).result()

        # "zero-out" the ports connected to that kernel
        kernel = self._kernels[kernel_id]
        port_names = ['shell_port', 'stdin_port', 'iopub_port', 'hb_port', 'control_port']
        port_names = kernel._random_port_names if hasattr(kernel, '_random_port_names') else port_names
        for port_name in port_names:
            setattr(kernel, port_name, 0)

        # "connect" to latest kernel started by an external python process
        kid = self._get_latest_kernel_id()
        connection_fname = f'/Users/ebanner/Library/Jupyter/runtime/kernel-{kid}.json'
        kernel.load_connection_file(connection_fname)

        # py2-compat
        raise gen.Return(kernel_id)
