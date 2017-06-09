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
  slots = list(start = "numeric", end = "numeric", genes = "vector", chromosome = "numeric")
)

#the class for the experiments
setClass(
  #name of the class
  "Experiment",
  
  #attributes of the class
  slots = list(name = "character", date = "character", locus = "Locus")
)

#-----------------------------------

#Methods

setMethod("show",
          "Locus",
          function(object){
            cat ("*** Class Locus , start is " , object@start, " end is", object@end , " and it's chromosome is ", object@chromosome)
          })

setGeneric(name = "ExistLocus",
           def = function(start,list_locus){standardGeneric("ExistLocus" )}
)

setMethod(f="ExistLocus",
          signature = "numeric",
          definition = function(start,list_locus){
            result <- FALSE
            for (i in 1:length(list_locus)){
              if(list_locus[i] == start){
                result <- TRUE
              }
            }
            return(result)
          }
)

#-----------------------------------

#test recovery datas from .txt in vector
test <- read.table("./test/text.txt")

datas <- vector(mode='list', length=57)
test[[1]][[1]]
for ( i in 1:nrow(test) ) {
  datas[i] <- new ("Locus", start =test[[2]][[i]], end=test[[3]][[i]], chromosome=test[[1]][[i]])
}

datas

showMethods(class="Locus")

ExistLocus(6612124,datas)

