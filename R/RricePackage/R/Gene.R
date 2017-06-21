
#####################################################################
############################## Class ################################


setClass(
  #name of the class
  "Gene",

  #attributes of the class
  slots = list(uniquename = "character", locus = "data.frame", others = "list")
)


#####################################################################
########################### Constructor #############################


Gene <- function(uniquename, locus){
  result <- new("Gene", uniquename = uniquename, locus = locus, others = list())
  return(result)
}


#####################################################################
############################ Methods ################################

setMethod("add_attribute",
          signature = "Gene",
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
          signature = "Gene",
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
