
.onLoad <- function(libname, pkgname) {
 
    ##call this function for executable python files
    
    ##If we are not under the os windows
    if (Sys.info()["sysname"] != "Windows"){
        callExecutablePy()
    }
    
    ##This function will allow your computer to download the python modules
    ## that we need
    dlPythonModules()
}
