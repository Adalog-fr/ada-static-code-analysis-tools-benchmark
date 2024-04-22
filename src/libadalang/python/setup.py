#
# Copyright (C) 2014-2022, AdaCore
# SPDX-License-Identifier: Apache-2.0
#

from setuptools import setup, find_packages


setup(
    name='Libadalang',
    version='0.1',
    packages=['libadalang'],
    package_data={
        'libadalang':
            ['*.{}'.format(ext) for ext in ('dll', 'so', 'so.*', 'dylib')]
            + ["py.typed"],
    },
    zip_safe=False,
)
