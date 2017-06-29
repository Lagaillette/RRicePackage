###############################################################################
################################ Constructor ##################################

#' Constructor of an Experiment
#' 
#' This constructor allows to create an Experiment object, with a name and a
#' locus list. The locus list is a .txt file, so you have to do a "read.table"
#' of your file into a variable and put this variable as the "locus" parameter
#' in the constructor. 
#'
#' @param name The name of the Experiment
#' @param locus The Table of locus 
#' @return An Experiment
#' @export
#' @aliases addAttribute,Experiment-method
#' @rdname Experiment-class
Experiment <- function(name, locus){
    nbdb <- as.numeric(readline(prompt=
                "Enter the numbers of databases you want to experiment : "))
    databases <- vector(mode='list', length=nbdb)
    date <- (readline(
                 prompt="Enter the date of the experiment (mm/dd/yyyy) : "))
    date <- as.Date(c(date), format =  "%m/%d/%Y")
    genes <- vector(mode='list', length=nbdb)
    for(i in 1:nbdb){
        databases[i] <- as.numeric(
                            readline(
                                prompt = paste("Enter the name (number)",
                                         "of the database you want: ")))
        callDB <- paste("callDB",databases[i],sep="")
        genes[[i]] <- (do.call(callDB, args = list(locus)))
    }
    result <- new("Experiment",
                  name=name,
                  date=date,
                  databases=databases,
                  genes=genes,
                  others=list())
    return(result)
}


###############################################################################
################################# Methods #####################################

setMethod(
    "show",
    signature = "Experiment",
    def = function(object){
        databases <- ""
        for(i in 1:length(object@databases)){
            databases <- paste(databases,object@databases[[i]])
        }
        result <- (paste("your Experiment has as name ",
                         object@name,
                         ",was experimented on ",
                         object@date, 
                         ", concern the database(s) : ",
                         databases,
                         sep = ""))
        if(length(object@others) != 0){
            result <- paste(result,
                            " and has also others attributes : ",
                            sep = "")
            for (i in 1:length(object@others)){
                result <- paste(result,
                                names(object@others[[i]]),
                                "=",
                                object@others[[i]][[1]],
                                "|")
            }
        }
        return(result)
    }
)


setMethod(
    "searchGeneProperty",
    signature = "Experiment",
    def = function(object, search){
        x <- 0
        result <- list()
        for(i in 1:lengths(object@databases)){
            for (j in 1:lengths(object@genes)){
                if(grepl(search, object@genes[[i]][[j]]@description)){
                    result <- append(result, object@genes[[i]][[j]])
                }
            
            }
        }
        return(result)
    }
)

setMethod(
    "searchGeneFmin",
    signature = "Experiment",
    def = function(object, search){
        print(nrow(object@genes))
            result <- list()
            for(i in 1:lengths(object@databases)){
                for (j in 1:lengths(object@genes)){
                    if( as.numeric(object@genes[[i]][[j]]@fmin) ==
                        as.numeric(search)){
                            result <- append(result, object@genes[[i]][[j]])
                  }
              
              }
          }
        return(result)
    }
)

#' @export
#' @aliases updateObject,Experiment-method
#' @rdname Experiment-class
setMethod(
    "updateObject",
    signature = "Experiment",
    def = function(experiment, attribute, value){
        name_setter = paste("set",
                            toupper(substr(attribute, 1, 1)),
                            substr(attribute, 2, nchar(attribute)),
                            sep = "")
        experiment<- do.call(name_setter, list(experiment, value))
        return(experiment)
    }
)

setMethod(
    "getName",
    signature = "Experiment",
    def = function(object){
        return(object@name)
    }
)


setMethod(
    "setName",
    signature = "Experiment",
    def = function(object,name){
        object@name <- name
        return(object)
    }
)


#' @export
#' @aliases addAttribute,Experiment-method
#' @rdname Experiment-class
setMethod(
    "addAttribute",
    signature = "Experiment",
    def = function(object, name, value){
        object@others <- append(object@others, value)
        size <- length(object@others)
        print(size)
        print(length(object@others))
        names( object@others[[size]]) <- name
        return(object)
    }
)


#' @export
#' @aliases deleteAttribute,Experiment-method
#' @rdname Experiment-class
setMethod(
    "deleteAttribute",
    signature = "Experiment",
    def = function(object, name){
        i <- 1
        find <- FALSE
        while(!find && i<=length(object@others)){
            if(names(object@others[[i]]) == name){
                print("I am in")
                object@others <- object@others[-i]
                find <- TRUE
            }
            i <- i+1
        }
        return(object)
    }
)