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
    ## the number of databases available. To increment every time we have
    ## one more database available
    dbAvailables <- 3
    ## to check if the number the user will put is correct or not
    correctNbdb <- FALSE
    while(!correctNbdb){
        nbdb <- as.numeric(
                readline(
                prompt="How many databases do you want to experiment : "))
        if(is.numeric(nbdb) && nbdb > 0 && nbdb <= dbAvailables){
            databases <- vector(mode='list', length=nbdb)
            correctNbdb <- TRUE
        }
        else{
            message("you have not choose a correct number of databases.")
            message("please try again")
        }
    }
    ## Allows to check if the date is good or not
    correctDate <- FALSE
    while(!correctDate){
        date <- (readline(
            prompt="Enter the date of the experiment (mm/dd/yyyy) : "))
        date <- as.Date(c(date), format =  "%m/%d/%Y")
        if(!is.na(date) && format(date, '%Y') > 1900 && format(date, '%Y') <= format(Sys.Date(), '%Y')){
            correctDate <- TRUE
        }
        else{
            message("please write the date in the format asked.")
        }
    }
    ## We create the list which will have the genes of the databases
    genes <- vector(mode='list', length=nbdb)
    i <- 1
    while(i <= nbdb){
        databases[i] <- as.numeric(
                            readline(
                                prompt = paste("Enter the number",
                                         "of the database you want",
                                         "(example : 1 for RAPDB) :")))
        if(databases[i] > 0 && 
           databases[i] <= dbAvailables && 
           !AlreadyUsedDB(databases,i)){
            callDB <- paste("callDB",databases[i],sep="")
            ##genes[[i]] <- (do.call(callDB, args = list(locus)))
            i <- i+1
        }
        else{
            if(AlreadyUsedDB(databases,i)){
                message("the database is already used. Try again")
            }
            else{
                message("this number of database not exists. Try again")
                message("You can put a number between 1 and 3")
            }
            
        }
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