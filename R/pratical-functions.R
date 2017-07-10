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
    if (substr(debut, 0,1) == '/') #ubuntu
        found <- 1
    else #windows
        found <- 0
    
    print(found)
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