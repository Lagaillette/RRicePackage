#####################################################################
############################## Class ################################


setClass(
  #name of the class
  "Experiment",

  #attributes of the class
  representation = representation(name = "character", date = "Date", databases = "list", genes = "list", others = "list")
)


#####################################################################
########################### Constructor #############################


Experiment <- function(name){
  nbdb <- as.numeric(readline(prompt="Enter the numbers of databases you want to experiment : "))
  databases <- vector(mode='list', length=nbdb)
  date <- (readline(prompt="Enter the date of the experiment (mm/dd/yyyy) : "))
  date <- as.Date(c(date), format =  "%m/%d/%Y")
  genes <- vector(mode='list', length=nbdb)
  for(i in 1:nbdb){
    databases[i] <- as.numeric(readline(prompt="Enter the name (number) of the database you want: "))
    callDB <- paste("callDB",databases[i],sep="")
    genes[[i]] <- do.call(callDB, args = list(databases[i]))
  }
  result <- new("Experiment", name = name, date = date, databases = databases, genes = genes, others = list())
  return(result)
}


#####################################################################
############################ Methods ################################

setMethod("show",
          signature = "Experiment",
          def = function(object){
            databases <- ""
            for(i in 1:length(object@databases)){
              databases <- paste(databases,object@databases[[i]])
            }
            result <- (paste("your Experiment has as name ", object@name, ",was experimented on ", object@date, ", concern the database(s) : ", databases, sep = ""))
            if(length(object@others) != 0){
              result <- paste(result, " and has also others attributes : ", sep = "")
              for (i in 1:length(object@others)){
                result <- paste(result, names(object@others[[i]]), "=", object@others[[i]][[1]], "|")
              }
            }
            return(result)
          }
)


setMethod("search_gene_property",
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

setMethod("search_gene_fmin",
          signature = "Experiment",
          def = function(object, search){
            print(nrow(object@genes))
            result <- list()
            for(i in 1:lengths(object@databases)){
              for (j in 1:lengths(object@genes)){
                if(as.numeric(object@genes[[i]][[j]]@fmin) == as.numeric(search)){
                  result <- append(result, object@genes[[i]][[j]])
                }
                
              }
            }
            return(result)
          }
)



setMethod("update_object",
          signature = "Experiment",
          def = function(experiment, attribute, value){
            name_setter = paste("set", "_", attribute, sep = "")
            experiment<- do.call(name_setter, list(experiment, value))
            return(experiment)
          }
)

setMethod("get_name",
          signature = "Experiment",
          def = function(object){
            return(object@name)
          }
)
setMethod("set_name",
          signature = "Experiment",
          def = function(object,name){
            object@name <- name
            return(object)
          }
)

setMethod("add_attribute",
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

setMethod("delete_attribute",
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