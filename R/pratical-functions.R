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
        if (databases[[j]] == databases[[i]]){
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
    if(outPut == "Website maintenance"){
        print(outPut)
    }
        
    if (outPut == "Bad request")
        print(outPut)
    if (outPut == "Forbidden")
        print(outPut)
    if (outPut == "Not found")
        print(outPut)
    if (outPut == "Too Many Requests")
        print(outPut)
    if (outPut == "Internal Server Error")
        print(outPut)
    if (outPut == "Service Unavailable")
        print(outPut)
    if (outPut == "Gateway Timeout")
        print(outPut)
    if (outPut == "HTTP Version Not Supported")
        print(outPut)
    if (outPut == "Unknow internet error")
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
    else if (substr(outPut, nchar(outPut), nchar(outPut)) == "}"){
        return(outPut)
    }
}

#' Function for see the list of the attributes of the class
#'
#' this function will print the attributes of the class
#' 
#' @param class the name of the class you want
#' @return print the attributes of the class
#' @export
#' @rdname getAttributes-function
#' @examples 
#' getAttributes("RAPDB")
getAttributes <- function(class){
    gene <- new(class)
    getAttributesNames(gene)
}

#' Function to create the vector of the attributes you want from a class
#'
#' this function will give you the vector to use selectProperties
#' 
#' @param class the name of the class you want
#' @param attributesVector the attributes you want to extract from the class
#' @return print the attributes of the class
#' @export
#' @rdname createAttributesVector-function
#' @examples 
#' createAttributesVector("RAPDB",c("id"))
createAttributesVector <- function(class, attributesVector){
    result <- purrr::map(attributesVector,function(x, class){
        paste(class, ".", attributesVector, sep = "")
    },class)
    return(result)
}