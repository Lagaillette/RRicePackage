
#the class for the properties
setClass(
  #name of the class
  "Property",
  
  #attributes of the class
  slots = list(description = "character")
)

#the class for the genes
setClass(
  #name of the class
  "Gene",
  
  #attributes of the class
  slots = list(start = "numeric", end = "numeric", name = "character", strand = "factor", properties = "vector")
)

#the class for the genes
setClass(
  #name of the class
  "Gene2",
  
  #attributes of the class
  slots = list(uniquename = "factor", msU7name = "factor", fgeneshName = "factor", rappredname = "factor", fmin = "numeric", fmax = "numeric",contig = "factor",iricname = "factor", strand = "factor", description = "character")
)



#the class for the chromosomes
setClass(
  #name of the class
  "Chromosome",
  
  #attributes of the class
  slots = list(name = "character")
)

#the class for the locus
setClass(
  #name of the class
  "Locus",
  
  #attributes of the class
  representation = representation(start = "numeric", end = "numeric", genes = "vector", chromosome = "numeric")
)

setClass("LocusChild",
         slots = list(autre = 2),
         contains = "Locus")


#the class for the experiments
setClass(
  #name of the class
  "Experiment",
  
  #attributes of the class
  slots = list(name = "character", date = "character", locus = "Locus")
)

#-----------------------------------

#Methods


#Generic Methods 

setGeneric(name = "ExistLocusOrNot",
           def = function(start){standardGeneric("ExistLocusOrNot" )}
)

setGeneric(name = "importGeneDatas",
           def = function(database){standardGeneric("importGeneDatas" )}
)

setGeneric(name = "importLocusDatas",
           def = function(database){standardGeneric("importLocusDatas" )}
)

setGeneric(name = "setname",
           def = function(object, name){standardGeneric("setname" )}
)
#Methods 

setMethod("setname",
          signature = "Locus",
          function(object, name){
            object@name <- name
          }
)
setMethod("show",
          "Locus",
          function(object){
            cat ("*** Class Locus , start is " , object@start, " end is", object@end , " and it's chromosome is ", object@chromosome)
          }
)

setMethod(f="importGeneDatas",
          signature = "numeric",
          definition = function(database){
            csv <- read.csv("./test/outputRAPDB.csv")
            genes <- vector(mode='list', length=nrow(csv))
            for ( i in 1:nrow(csv) ) {
              genes[i] <- new ("Gene2", uniquename = factor(csv[[1]][[i]]), msU7name = factor(csv[[9]][[i]]), fgeneshName = factor(csv[[4]][[i]]), rappredname = factor(csv[[5]][[i]]), fmin = csv[[6]][[i]], fmax = csv[[2]][[i]],contig = factor(csv[[7]][[i]]),iricname = factor(csv[[8]][[i]]), strand = factor(csv[[10]][[i]]), description = as.character(csv[[11]][[i]]))
            }
            return(genes)
          }
  
)


call <- function(database){
  if(database == 1){
    csv <- read.csv("./test/outputRAPDB.csv")
    genes <- vector(mode='list', length=nrow(csv))
    for ( i in 1:nrow(csv) ) {
      genes[i] <- new ("Gene2", uniquename = csv[[1]][[i]], msU7name = csv[[9]][[i]], fgeneshName = csv[[4]][[i]], rappredname = csv[[5]][[i]], fmin = csv[[6]][[i]], fmax = csv[[2]][[i]],contig = csv[[7]][[i]],iricname = csv[[8]][[i]], strand = csv[[10]][[i]], description = as.character(csv[[11]][[i]]))
    }
    return(genes)
  }
}

setMethod(f="importLocusDatas",
          signature = "numeric",
          definition = function(database){
            table <- read.table("./test/text.txt")
            datas <- vector(mode='list', length=57)
            for ( i in 1:nrow(test) ) {
              datas[i] <- new ("Locus", start = table[[2]][[i]], end = table[[3]][[i]], chromosome = table[[1]][[i]])
            }
            return(datas)
          }
          
)

setMethod(f="ExistLocusOrNot",
          signature = "numeric",
          definition = function(start){
            result <- FALSE
            for (i in 1:length(datas)){
              if(datas[[i]]@start == start){
                result <- TRUE
              }
            }
            return(result)
          }
)

#test


