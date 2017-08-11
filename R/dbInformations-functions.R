#' Function to know the list of the databases availables
#'
#' this function return return a list of the databases availables
#' 
#' @return return a list of the databases availables
#' @rdname listDB-function
listDB <- function(){
    result <- data.frame()
    result <- append(result, "RAPDB")
    result <- append(result, "Gramene")
    result <- append(result, "Oryzabase")
    result <- append(result, "IC4R")
    result <- append(result, "Planttfdb")
    result <- append(result, "PLNTFDB")
    result <- append(result, "Funricigenes")
    result <- append(result, "Funricigenes2")
    result <- append(result, "Funricigenes3")
    result <- append(result, "MSU7")
    ##add the new db here like it's done above
    
    return(result)
}

#' Function for see the list of the databases we can call
#'
#' this function will print the databases we can call 
#' 
#' @return print the databases we can call  
#' @export
#' @rdname databasesList-function
#' @examples 
#' databasesList()
databasesList <- function () {
    listDB <- listDB()
    cat("The list of the databases : \n")
    for(i in 1:length(listDB)){
        cat(i,"for",listDB[[i]], "\n")
    }
}

#' Function to know the number of databases availables
#'
#' this function return the number of databases availables
#' 
#' @return return the number of databases availables 
#' @export
#' @rdname databasesAvailables-function
#' @examples 
#' databasesAvailables()
databasesAvailables <- function(){
    return(length(listDB()))
}

#' Function to know the the name of the database associate to the number
#'
#' this function return the name of the database associate to the number
#' 
#' @return return the name of the database associate to the number
#' @rdname changeNumberIntoDBName-function
changeNumberIntoDBName <- function(number){
    listDB <- listDB()
    return(listDB[[number]])
}

#' Function to know the number of the database associate to the name
#'
#' this function return the number of databases availables
#' 
#' @return return the number of databases availables 
#' @rdname changeDBNameIntoNumber-function
changeDBNameIntoNumber <- function(dbName){
    listDB <- listDB()
    i <- 1
    found <- FALSE
    while(!found && i <= length(listDB)){
        if(listDB[[i]] == dbName){
            found <- TRUE
        }
        else{
            i <- i+1
        }
    }
    if(!found){
        stop("the name of the database asked is incorrect")
    }else{
        return(i)
    }
    
}