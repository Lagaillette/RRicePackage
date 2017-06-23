from setuptools import setup

setup(name='rricebeta',
      version='0.1',
      description='RRice Package',
      url='https://github.com/Lagaillette/RRicePackage/tree/master/Python/RRiceBeta',
      author='Baptiste VAUTRIN',
      author_email='baptiste.vautrin@gmail.com',
      license='ICTLAB',
      packages=['rricebeta'],
      install_requires=[
          'requests', 'bs4', 'pandas', 'gzip', 'os' ,'json'

      ],
      zip_safe=False)