#' An S4 class to represent an Experiment class.
#'
#' Some details about this class and my plans for it in the body.
#'
#' @slot name The name of the experiment
#' @slot date The date of the experiment
#' @slot databases the databases which concern the object we are studying 
#' @slot genes The list of genes we avec download.
#' @slot others The others attributes the user can add and delete
#' @name Experiment-class
#' @rdname Experiment-class
#' @docType class
#' @exportClass Experiment
#' @examples 
#' exp <- new(Class="Experiment", date=Sys.Date(), name="test")
setClass(
    ##name of the class
    "Experiment",
    
    ##attributes of the class
    representation(name = "character",
                   date = "Date",
                   databases = "list",
                   genes = "list",
                   others = "list"),
    
    prototype( others = list()),
    
    validity = checkExperiment <- function(object){
        ## To update when a DB is added. The number of databases we can call
        nbDB <- 6
        errors <- character()
        yearDate <- object@date
        if(length(object@databases)<0 || length(object@databases)>nbDB){
          msg <- paste("You have to have at least one database",
                       "and less than", nbDB, sep=" ")
          errors <- c(errors, msg)
        }
        if (yearDate > Sys.Date()) {
          msg <- paste("The year has to be bellow", 
                       "the current year.")
          errors <- c(errors, msg)
        }
        if(length(errors) == 0) TRUE else errors
    }

  
)

#' An S4 class to represent a Gene.
#'
#' A gene contains a lot of details that depend on the database it belongs.
#' But every genes have informations in common. So we put all these informations
#' into a global class "Gene" from which the others specific genes classes will
#' inherit.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @name Gene-class
#' @rdname Gene-class
setClass(
    ##name of the class
    "Gene",
    
    ##attributes of the class
    representation(id = "character",
               genesIDs = "list",
               locus = "data.frame",
               others = "list"),
    
    prototype( others = list())
)




#' An S4 class to represent the first database Gene.
#'
#' This gene is specific to the database 1 that is the database "RAPDB". So it
#' contains all the informations we can have about one gene with the "RAPDB"
#' database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot rapDBGeneNameSynonym an identificator
#' @slot rapDBGeneSymbolSynonym an identificator
#' @slot cgsnlGeneName an identificator
#' @slot cgsnlGeneSymbol an identificator
#' @slot oryzabaseGeneNameSynonym an identificator
#' @slot oryzabaseGeneSymbolSynonym an identificator
#' @slot position the position of the gene into the chromosome
#' @slot description a description of the gene
#' @name GeneDB1-class
#' @rdname GeneDB1-class
#' @exportClass GeneDB1
#' @examples
#' gene <- new("GeneDB1")
setClass(
    ##name of the class
    "GeneDB1",
    
    ##attributes of the classnumeric
    slots = list(rapDBGeneNameSynonym = "character",
                 rapDBGeneSymbolSynonym = "character",
                 cgsnlGeneName = "character",
                 cgsnlGeneSymbol = "character",
                 oryzabaseGeneNameSynonym = "character",
                 oryzabaseGeneSymbolSynonym = "character",
                 position = "data.frame",
                 description = "character"),
    
    contains = "Gene"
)


#' An S4 class to represent the third database Gene.
#'
#' This gene is specific to the database 3 that is the database "Oryzabase".
#' So it contains all the informations we can have about one gene with the
#' "Oryzabase" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot traitGeneId id
#' @slot cgsnlGeneSymbol id
#' @slot GeneSymbolSynonim id
#' @slot cgsnlSymbolSynonim id
#' @slot GeneNameSynonim id
#' @slot proteinName id
#' @slot allele id
#' @slot chromosomeNumber id
#' @slot explanation id
#' @slot traitClass id
#' @slot rapID id
#' @slot grameneId id
#' @slot arm id
#' @slot locate id
#' @slot geneOntology id
#' @slot traitOntology id
#' @slot plantOntology id
#' @name GeneDB3-class
#' @rdname GeneDB3-class
#' @exportClass GeneDB3
#' @examples 
#' gene <- new("GeneDB3")
setClass(
    ##name of the class
    "GeneDB3",

    ##attributes of the classnumeric
    slots = list(traitGeneId = "character",
                 cgsnlGeneSymbol = "character",
                 GeneSymbolSynonim = "character",
                 cgsnlSymbolSynonim = "character",
                 GeneNameSynonim = "character",
                 proteinName = "character",
                 allele = "character",
                 chromosomeNumber = "numeric",
                 explanation = "character",
                 traitClass = "character",
                 rapID = "character",
                 grameneId = "character",
                 arm = "character",
                 locate = "character",
                 geneOntology = "character",
                 traitOntology = "character",
                 plantOntology = "character"
    ),

    contains = "Gene"
)


#' An S4 class to represent the seven database Gene.
#'
#' This gene is specific to the database 7 that is the database "IC4R". So it
#' contains all the informations we can have about one gene with the "IC4R"
#' database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot experimentName the experiment name
#' @slot devStage devStage
#' @slot experimentId the experiment id,
#' @slot variety the variety,
#' @slot projectId the project id,
#' @slot tissue the tissue,
#' @slot exprValue the expr value
#' @name GeneDB7-class
#' @rdname GeneDB7-class
#' @exportClass GeneDB7
#' @examples
#' gene <- new("GeneDB7")
setClass(
    ##name of the class
    "GeneDB7",
    
    ##attributes of the classnumeric
    slots = list(experimentName = "list",
                 devStage = "list",
                 experimentId = "list",
                 variety = "list",
                 projectId = "list",
                 tissue = "list",
                 exprValue = "list"),
    
    contains = "Gene"
)

#' An S4 class to represent the Gene database number 9.
#'
#' This gene is specific to the database 9 that is the database "???". So it
#' contains all the informations we can have about one gene with the "???"
#' database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot family the family of the gene
#' @name GeneDB9-class
#' @rdname GeneDB9-class
#' @exportClass GeneDB9
#' @examples
#' gene <- new("GeneDB9")
setClass(
    ##name of the class
    "GeneDB9",
    
    ##attributes of the classnumeric
    slots = list(family = "character"),
    
    contains = "Gene"
)

#' An S4 class to represent the Gene database number 10
#'
#' This gene is specific to the database 10 that is the database "???". So it
#' contains all the informations we can have about one gene with the "???"
#' database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot symbol the gene symbol
#' @name GeneDB10-class
#' @rdname GeneDB10-class
#' @exportClass GeneDB10
#' @examples
#' gene <- new("GeneDB10")
setClass(
    ##name of the class
    "GeneDB10",
    
    ##attributes of the classnumeric
    slots = list(symbol = "character"),
    
    contains = "Gene"
)