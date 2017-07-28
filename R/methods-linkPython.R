library(PythonInR)

#' callExecutablePy
#'
#' This function calls test.py which will be allow to all python file to be
#' executable
#' 
#' @import PythonInR
#' @export
#' @rdname callExecutablePy-function
callExecutablePy <- function () {
    path <- system.file("python/rricebeta",
                        "test.py",
                        package = "rRice")
    
    path2 <- system.file("python/rricebeta",
                        package = "rRice")
    
    #print(paste0("R : ",path))
    #capture.output(path, file = "python/appel.txt")
    #modif = readLines(path1, -1)
    #modif[1] = path
    #writeLines(path, path1)
    
    ##put the variable path into python __main__
    # if (Sys.getenv("R_ARCH") == "/x64") {
    #   PythonInR::pyConnect("C:\\Users\\Administrator\\AppData\\Local\\Programs\\Python\\Python35\\python.exe")
    #   
    # }
    # else {
    #   PythonInR::pyConnect("C:\\Users\\Administrator\\AppData\\Local\\Programs\\Python\\Python35-32\\python.exe")
    # }
    # print(PythonInR::pyIsConnected())
    PythonInR::pySet("path", path2, namespace = "__main__")
    PythonInR::pyExecfile(path)
}

