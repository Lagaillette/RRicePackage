from setuptools import setup

setup(name='RRiceBeta',
      version='0.8',
      description='RRice Package',
      url='https://github.com/Lagaillette/RRicePackage/tree/master/Python/RRiceBeta',
      author='Baptiste VAUTRIN',
      author_email='baptiste.vautrin@gmail.com',
      license='ICTLAB',
      packages=['RRiceBeta'],
      install_requires=[
          'requests', 'bs4', 'pandas', 'gzip', 'os' ,'json'

      ],
      zip_safe=False)