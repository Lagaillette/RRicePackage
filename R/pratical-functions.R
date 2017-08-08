#' Function that allows to know if a gene already exists or not
#' 
#' @param genes the list of unique genes.
#' @param id the id of the gene we don't want to duplicate.
#' @return Boolean TRUE if the gene exists, FALSE if not.
#' @rdname existsGene-function
existsGene <- function(genes, id){
    found <- FALSE
    i <- 1
    while(!found && i<=length(genes)){
        if (as.character(genes[[i]]@id) == as.character(id)){
            found <- TRUE
        }
        i <- i+1
    }
    return(found)
}

#'Function to know the OS using the package
#'
#' this function allows us to know if the user is using Linux or Windows
#' 
#' @return numeric 0 if windows, 1 if linux
#' @rdname whichOS-function
whichOS <- function(){
    found <- 0
    
    debut = getwd()
    if (substr(debut, 0,1) == '/') ##ubuntu
        found <- 1
    else ##windows
        found <- 0
    
    ##print(found)
    return(found)
}

#' Function checking if the database is already used
#' 
#' @param databases the list of the databases we have
#' @param i the number of the database we want to know if it's already used
#' @return Boolean TRUE if the database is already used, FALSE if not
#' @rdname alreadyUsedDB-function
alreadyUsedDB <- function(databases,i){
    j <- 1
    alreadyUsed <- FALSE
    while(j < i && !alreadyUsed){
        if (as.numeric(databases[j]) == as.numeric(databases[i])){
            alreadyUsed <- TRUE
        }
        else{
            j <- j+1
        }
    }
    return(alreadyUsed)
}

#' Function which returns only the right errors
#'
#' This function will return all the errors like "Bad request",...
#' 
#' @param outPut character
#' @return return only errors we know
#' @rdname returnError-function
returnError <- function (outPut) {
    if (outPut == "Website maintenance")
        print(outPut)
    else if (outPut == "Bad request")
        print(outPut)
    else if (outPut == "Forbidden")
        print(outPut)
    else if (outPut == "Not found")
        print(outPut)
    else if (outPut == "Too Many Requests")
        print(outPut)
    else if (outPut == "Internal Server Error")
        print(outPut)
    else if (outPut == "Service Unavailable")
        print(outPut)
    else if (outPut == "Gateway Timeout")
        print(outPut)
    else if (outPut == "HTTP Version Not Supported")
        print(outPut)
    else if (outPut == "Unknow internet error")
        print(outPut)
    #else if (outPut == "on telecharege")
    #    print(outPut)
        
}

#'Function for a JSON return
#'
#' this function will only return an JSON return which start with "\{"
#' It will allow us to treat the exception error from python 
#' 
#' @param outPut character
#' @return return only string which starts with "\{" -> JSON 
#' @rdname getOutPutJSON-function
getOutPutJSON <- function (outPut) {
    if (identical(substr(outPut,0,1),'{')) {
        return(outPut)
    }
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
    print("Enter the number(s) of the database you want : ")
    print("1 for RAPDB")
    print("2 for GRAMENE")
    print("3 for ORYZABASE")
    print("4 for IC4R")
    print("5 for Planttfdb")
    print("6 for PLNTFDB")
    print("7 for FUNRICEGENES1")
    print("8 for FUNRICEGENS2")
    print("9 for FUNRICEGENS3")
}