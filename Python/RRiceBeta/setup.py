from setuptools import setup

setup(name='RRiceBeta',
      version='0.6',
      description='RRice Package',
      url='https://github.com/Lagaillette/RRicePackage/tree/master/Python/RRiceBeta',
      author='Baptiste VAUTRIN',
      author_email='baptiste.vautrin@gmail.com',
      license='ICTLAB',
      packages=['RRiceBeta'],
      install_requires=[
          'requests', 'bs4'

      ],
      zip_safe=False)