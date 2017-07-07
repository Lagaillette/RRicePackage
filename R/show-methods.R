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

