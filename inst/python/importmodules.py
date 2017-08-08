#!/usr/bin/env python3

import imp
import pip

def install(package):
    pip.main(['install', package])

def main():
    try:
        imp.find_module('pandas')
    except ImportError:
        install("pandas")

    try:
        imp.find_module('bs4')
    except ImportError:
        install("bs4")
    
    try:
        imp.find_module('requests')
    except ImportError:
        install("requests")
    
    try:
        imp.find_module('json')
    except ImportError:
        install("json")

    try:
        imp.find_module('gzip')
    except ImportError:
        install("gzip")
    
    try:
        imp.find_module('lxml')
    except ImportError:
        install("lxml")

    
# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()
