#####################################################################
############################## Class ################################


setClass(
  #name of the class
  "Experiment",

  #attributes of the class
  representation = representation(name = "character", date = "character", databases = "vector", genes = "vector")
)


#####################################################################
########################### Constructor #############################


Experiment <- function(name){
  nbdb <- as.numeric(readline(prompt="Enter the numbers of databases you want to experiment : "))
  databases <- vector(mode='list', length=nbdb)
  date <- as.character(Sys.Date())
  genes <- vector(mode='list', length=nbdb)
  for(i in 1:nbdb){
    databases[i] <- as.numeric(readline(prompt="Enter the name (number) of the database you want: "))
    genes[[i]] <- call(databases[i])
  }
  result <- new("Experiment", name = name, date = date, databases = databases, genes = genes)
  return(result)
}


#####################################################################
############################ Methods ################################
