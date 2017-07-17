library(reticulate)

#' changeRightPyFile
#'
#' This function changes the right of the file. This file becomes executable
#' 
#' @param file character
#' @import reticulate
#' @export
#' @rdname changeRightPyFile-function
changeRightPyFile <- function (file) {
    os = reticulate::import("os")
    stat = reticulate::import("stat")
    
    if (!os$access(file, os$X_OK)) {
        st <- os$stat(file)
        os$chmod(file, st$st_mode + stat$S_IEXEC)
    }
}

#test('t.py')


#' verifGoodFile
#'
#' This function verifies that you are not going to change the right to a
#' bad file
#' 
#' @param file character
#' @export
#' @rdname verifGoodFile-function
verifGoodFile <- function (file) {
    ##print(is.na(file))
    if (length(file) > 0) {
        if (!(is.na(file))) {
            if (file != "__init__.py" && file != "__pycache__") {
                return (file)
            }
        }
    }
    
}

#' transformExePy
#'
#' This function will call every python file and will change his chmod in 
#' execuatable. Then we will be able to use python file
#' 
#' @export
#' @rdname transformExePy-function
transformExePy <- function() {
    path <- system.file("python/rricebeta/",
                        package = "rRice")
    
    #print(path)
    f <- list.files(path ,pattern="*.py")
    
    
    # f <- list("run.py",
    #           "Scriptv7_Table.py",
    #           "snpSeek.py",
    #           "ScriptGramene.py",
    #           "ScriptV1.py",
    #           "ScriptV2.py",
    #           "ScriptV8_Oryzabase.py",
    #           "ScriptV6_WebScrapping.py",
    #           "ScriptV5_WebScrapping.py",
    #           "ScriptV4_Gramene_JSON.py",
    #           "ScriptV3.py",
    #           "ScriptV9.py",
    #           "snpSeekAll.py",
    #           "snp-seek.py",
    #           "testPython.py")
    # #print(f)
    # 
    # new <- lapply(1 : length(f), FUN = function(x) verifGoodFile(f[x]))
    # new[sapply(new, is.null)] <- NULL
    # lapply(1 : length(new),
    #        FUN = function(x) changeRightPyFile(paste0(path,new[[x]])))
    
    #print(paste0(path,f[3]))
    #print(f)
    new <- lapply(1 : length(f), FUN = function(x) verifGoodFile(f[x]))
    
    ##Remove all the NULL object from the list
    new[sapply(new, is.null)] <- NULL
    
    #print(new)
    if (length(new) > 0) {
        lapply(1 : length(new),
               FUN = function(x) changeRightPyFile(paste0(path,"/",new[[x]])))
    }
    
}

## TEST
#transformExePy()
