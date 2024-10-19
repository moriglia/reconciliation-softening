# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright (C) 2024  Marco Origlia

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.
from distutils.core import setup, Extension
from Cython.Build import cythonize
import sys
import os


cysrc = "src/cython/"
flibs = ["build/fortran/lib", os.environ['HOME'] + "/.local/lib/"]

all_extensions = {
    "simtools" : Extension(
        name               = "resoften.simtools",
        sources            = [cysrc + "simtools.pyx"],
        libraries          = ["simtools", "ldpc", "gfortran", "caf_openmpi", "fortran_stdlib", "mvec"],
        library_dirs       = [*flibs]
    ),
    "ldpc" : Extension(
        name               = "resoften.ldpc",
        sources            = [cysrc + "ldpc/ldpc.pyx"],
        libraries          = ["ldpc", "gfortran"],
        library_dirs       = [*flibs]
    )
}


# Parse the command-line argument
only_module = None
if '--only' in sys.argv:
    module_index = sys.argv.index('--only') + 1
    if module_index < len(sys.argv):
        only_module = sys.argv.pop(module_index)
        sys.argv.remove('--only')
        
# Select the specific module to build
if only_module:
    ext_modules = [all_extensions[only_module]]
else:
    ext_modules = list(all_extensions.values())

    
setup(
    name = "resoften",
    description = "Reverse Reconciliation Softening tools",
    packages=["resoften"],
    ext_modules = cythonize(ext_modules, language_level=3)
)
