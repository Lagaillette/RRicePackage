#!/usr/bin/env python3

import os
import sys
import stat
import glob
	
#this function change the right of the file into executable
def changeRightPyFile(file):
    if (not(os.access(file, os.X_OK))):
         st = os.stat(file)
         os.chmod(file, st.st_mode | stat.S_IEXEC)

#verify some stuff for the file
def verifGoodFile(file):
    if (len(file) > 0):
        if(os.path.isfile(file) and os.path.exists(file)):
            if (file[-11:] != "__init__.py" and file[-11:] != "__pycache__"):
                return True
            else:
                return False
            
def main():
    #we get the variable path from the R (zzz.R) and we put all the python 
    #files into a list()
    liste1 = glob.glob(path+"/*.py")
    
    #for each element of the list, we have to pass some tests
    #if everything is okay, then we call the function changeRightFile(nameFile)
    for i in liste1:
        if(verifGoodFile(i)):
            changeRightPyFile(i)
            #print(i)


if __name__ == "__main__":
    main()
