


from setuptools import setup, find_packages


setup(
    name='GprParser',
    version='0.1',
    packages=['gpr_parser'],
    package_data={
        'gpr_parser':
            ['*.{}'.format(ext) for ext in ('dll', 'so', 'so.*', 'dylib')]
            + ["py.typed"],
    },
    zip_safe=False,
)
