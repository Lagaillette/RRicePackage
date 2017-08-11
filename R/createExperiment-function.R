#' Function to create an Experiment
#' 
#' This function allows to create an Experiment object, with a name and a
#' locus list. The locus list is a .txt file, so you have to do a "read.table"
#' of your file into a variable and put this variable as the "locus" parameter
#' in the constructor. 
#'
#' @param name The name of the Experiment
#' @param locus The Table of locus 
#' @importFrom methods new
#' @return An Experiment
#' @export
#' @rdname createExperiment-function
#' @examples 
#' locusListExample <- data.frame(ch = c("0"),st = c("0"),end = c("0"))
#' 
#' createExperiment("example",locusListExample)
createExperiment <- function(name, locus){
    if(locus[[1]][[1]]==0){
        result <- new("Experiment",
                      name="test",
                      date=Sys.Date(),
                      databases=list(1),
                      genes=list(new("RAPDB")),
                      others=list())
        return(result)
    }else{
        ## the number of databases available. To increment every time we have
        ## one more database available
        dbAvailables <- databasesAvailables()
        ## to check if the number the user will put is correct or not
        correctNbdb <- FALSE
        while(!correctNbdb){
            nbdb <- as.numeric(
                readline(
                    prompt="How many databases do you want to experiment ? :"))
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
            if(!is.na(date) && 
               format(date, '%Y') > 1900 &&
               format(date, '%Y') <= format(Sys.Date(), '%Y')){
                correctDate <- TRUE
            }
            else{
                message("please write the date in the format asked.")
            }
        }
        message("loading informations...")
        ## We get all the geneIds of the locuses
        genesIds <- callSnpSeek(locus)
        ## We create the list which will have the genes of the databases
        genes <- vector(mode='list', length=nbdb)
        i <- 1
        while(i <= nbdb){
            ##print the choices the user can do
            databasesList()
            ##read the choice
            databases[[i]] <- changeNumberIntoDBName(as.numeric(readline()))
            if(!alreadyUsedDB(databases,i)){
                message('loading informations of the database...')
                callDB <- paste("callDB",
                                changeDBNameIntoNumber(databases[i]),
                                sep="")
                genes[[i]] <- (do.call(callDB, args = list(genesIds, locus)))
                i <- i+1
            }
            else{
                if(alreadyUsedDB(databases,i)){
                    message("the database is already used. Try again")
                }
                else{
                    message("this number of database not exists. Try again")
                    message("You can put a number between 1 and ", dbAvailables)
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
    
}