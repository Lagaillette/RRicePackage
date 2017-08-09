#' An S4 class to represent an Experiment class.
#'
#' Some details about this class and my plans for it in the body.
#'
#' @slot name The name of the experiment
#' @slot date The date of the experiment
#' @slot databases the databases which concern the object we are studying 
#' @slot genes The list of genes we avec download.
#' @slot properties The properties you want to keep
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
                   properties = "list",
                   others = "list"),
    
    prototype( others = list()),
    
    validity = checkExperiment <- function(object){
        ## To update when a DB is added. The number of databases we can call
        nbDB <- 10
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
#' @name RAPDB-class
#' @rdname RAPDB-class
#' @exportClass RAPDB
#' @examples
#' gene <- new("RAPDB")
setClass(
    ##name of the class
    "RAPDB",
    
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
#' @name Oryzabase-class
#' @rdname Oryzabase-class
#' @exportClass Oryzabase
#' @examples 
#' gene <- new("Oryzabase")
setClass(
    ##name of the class
    "Oryzabase",

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


#' An S4 class to represent Gene database IC4R.
#'
#' This gene is specific to the database "IC4R". So it
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
#' @name IC4R-class
#' @rdname IC4R-class
#' @exportClass IC4R
#' @examples
#' gene <- new("IC4R")
setClass(
    ##name of the class
    "IC4R",
    
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

#' An S4 class to represent the Gene database Funricigenes 2.
#'
#' This gene is specific to the database Funricigenes. So it
#' contains all the informations we can have about one gene with the
#' "Funricigenes 2" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot family the family of the gene
#' @name PLNTFDB-class
#' @rdname PLNTFDB-class
#' @exportClass PLNTFDB
#' @examples
#' gene <- new("PLNTFDB")
setClass(
    ##name of the class
    "PLNTFDB",
    
    ##attributes of the classnumeric
    slots = list(family = "character"),
    
    contains = "Gene"
)

#' An S4 class to represent the Gene database Funricigenes
#'
#' This gene is specific to the database Funricigenes. So it
#' contains all the informations we can have about one gene with the
#' "Funricigenes" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot symbol the gene symbol
#' @name Funricigenes-class
#' @rdname Funricigenes-class
#' @exportClass Funricigenes
#' @examples
#' gene <- new("Funricigenes")
setClass(
    ##name of the class
    "Funricigenes",
    
    ##attributes of the classnumeric
    slots = list(symbol = "character"),
    
    contains = "Gene"
)

#' An S4 class to represent the Gene database Funricigenes 2
#'
#' This gene is specific to the database Funricigenes 2. So it
#' contains all the informations we can have about one gene with the
#' "Funricigenes 2" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot symbol the gene symbol
#' @slot name name
#' @name Funricigenes2-class
#' @rdname Funricigenes2-class
#' @exportClass Funricigenes2
#' @examples
#' gene <- new("Funricigenes2")
setClass(
    ##name of the class
    "Funricigenes2",
    
    ##attributes of the classnumeric
    slots = list(symbol = "character", name = "character"),
    
    contains = "Gene"
)

#' An S4 class to represent the Gene database Funricigenes 3
#'
#' This gene is specific to the database Funricigenes 3. So it
#' contains all the informations we can have about one gene with the
#' "Funricigenes 3" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot symbol the gene symbol
#' @slot keyword keyword
#' @slot title title
#' @name Funricigenes3-class
#' @rdname Funricigenes3-class
#' @exportClass Funricigenes3
#' @examples
#' gene <- new("Funricigenes3")
setClass(
    ##name of the class
    "Funricigenes3",
    
    ##attributes of the classnumeric
    slots = list(symbol = "character", 
                 keyword = "character", 
                 title = "character"),
    
    contains = "Gene"
)

#' An S4 class to represent the Gene database Gramene
#'
#' This gene is specific to the database Gramene. So it
#' contains all the informations we can have about one gene with the
#' "Gramene" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot description description of the gene
#' @slot biotype the biotype of the gene
#' @slot taxonId id
#' @slot systemName system name
#' @slot dbType type of the db
#' @slot geneIdx id
#' @slot location location 
#' @name Gramene-class
#' @rdname Gramene-class
#' @exportClass Gramene
#' @examples
#' gene <- new("Gramene")
setClass(
    ##name of the class
    "Gramene",
    
    ##attributes of the classnumeric
    slots = list(description = "character",
                 biotype = "character",
                 taxonId = "character",
                 systemName = "character",
                 dbType = "character",
                 geneIdx = "character",
                 location = "data.frame"),
    
    contains = "Gene"
)

#' An S4 class to represent the Gene database MSU
#'
#' This gene is specific to the database MSU So it
#' contains all the informations we can have about one gene with the
#' "MSU" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot protein protein
#' @slot genomicSequence genomic sequence
#' @slot cds cds
#' @name MSU-class
#' @rdname MSU-class
#' @exportClass MSU
#' @examples
#' gene <- new("MSU")
setClass(
    ##name of the class
    "MSU",
    
    ##attributes of the classnumeric
    slots = list(protein = "character",
                 genomicSequence = "character",
                 cds = "character"),
    
    contains = "Gene"
)

#' An S4 class to represent the Gene database PLANTTFDB
#'
#' This gene is specific to the database PLANTTFDB So it
#' contains all the informations we can have about one gene with the
#' "PLANTTFDB" database.
#'
#' @slot id The id of the gene. Many genes can have the same id because 
#' there are the same in spite of the different attributes they have.
#' @slot genesIDs the list of diferent ids it have we will use with the 
#' databases.
#' @slot locus The locus the gene belongs.
#' @slot others The others attributes the user want to add or delete.
#' @slot family family
#' @name PLANTTFDB-class
#' @rdname PLANTTFDB-class
#' @exportClass PLANTTFDB
#' @examples
#' gene <- new("PLANTTFDB")
setClass(
    ##name of the class
    "PLANTTFDB",
    
    ##attributes of the classnumeric
    slots = list(family = "character"),
    
    contains = "Gene"
)