library(jsonlite)

#on relie au fichier methods-GeneDB1.R to call function GeneDB1
source("rRice/R/methods-GeneDB1.R")

command ="python3"

#' This function calls Scriptv7 from python
#'
#' Cette fonction permet à l'utilisateur de chercher si le RAPID existe dans 
#' Oryzabase.txt
#' @param RAPID
#' @keywords 
#' @export
#' @examples
#' Appel_Scriptv7("Os06g0654600")
Appel_Scriptv7 <- function (RAPID) {
  
    #chemin pour n'importe quel utilisateur
    debut = getwd()
    path = "/rRice/inst/Python/rricebeta/rricebeta/Scriptv7_Table.py"
    path2script = paste(c(debut,path), collapse = '')

    # Build up args in a vector
    # RAPID_valide = "Os06g0654600" -> exemple valide
    args = c(RAPID)

    # Add path to script as first arg
    allArgs = c(path2script, args)

    Routput = system2(command, args=allArgs, stdout=TRUE)

    #Appelé à chaque fois qu'elle rencontrera un print
    print(Routput)
}


#' This function calls run.py
#'
#' This function will call for each locus in the list, the run.py script and
#' python will return the list of the genes which are present in the locus and
#' in the DB1. All these locus will be stocked in liste_genes
#' @param Locus list
#' @keywords 
#' @export
#' @examples
#' callDB1(Liste de locus)
CallDB1 <- function (locusListe) {
  
    debut = getwd()
    path = "/rRice/inst/Python/rricebeta/rricebeta/run.py"
    path2script = paste(c(debut,path), collapse = '')
    
    liste_genes <- data.frame()
  
    for (i in 1 : nrow(locusListe)) {
        ch = as.character(locusListe[i,1])
        start = as.character(locusListe[i,2])
        end = as.character(locusListe[i,3])
        
        #print(paste(ch," ",start," ",end))
    
        #print(paste(class(ch)," ",class(start)," ",class(end)))
    
        #appel du script python run.py avec les attributs (chx, start, end, DB) 
        #-> tous les attributs doivent etre en chaine de carac
        args = c(ch, start, end, "1")
        allArgs = c(path2script, args)
        Routput = system2(command, args=allArgs, stdout=TRUE)
        print(Routput)
        #print(Routput)
        test <- fromJSON(Routput)
        
        #for (j in 1:length(test)){
        #    print(paste("numero",j," -> ",test[j][1]))
        #}
        
        id_rec = test["ID"]
        position = test["Position"]
        rap_symbole = test["RAP-DB Gene Symbol Synonym(s)"]
        cgsnl_name = test["CGSNL Gene Name"]
        ory_gene_symbole = test["Oryzabase Gene Symbol Synonym(s)"]
        description = test["Description"]
        rap_name = test["RAP-DB Gene Name Synonym(s)"]
        ory_gene_name = test["Oryzabase Gene Name Synonym(s)"]
        cgsnl_gene = test["CGSNL Gene Symbol"]
        
        position <- as.character(position) 
        pos1 <- strsplit(position, ":")
        pos2 <- pos1[[1]][[2]]
        pos3 <- strsplit(pos2,"[..]")
        
        positiondata <- data.frame(ch=c(as.character(pos1[[1]][[1]])),
                                   st=c(as.character(pos3[[1]][[1]])),
                                   end=c(as.character(pos3[[1]][[3]])))
        
        #prochaine étape -> créer objet et mettre tous les attributs dedans
        #créé un nouvel objet gene
        newgene <- GeneDB1(as.character(id_rec),
                           locusListe[i,],
                           as.character(rap_name),
                           as.character(rap_symbole),
                           as.character(cgsnl_name),
                           as.character(cgsnl_gene),
                           as.character(ory_gene_name),
                           as.character(ory_gene_symbole),
                           data.frame(ch=c(pos1[[1]][[1]]),st=c(pos3[[1]][[1]]),end=c(pos3[[1]][[3]])),
                           as.character(description))
        
        liste_genes <- append(liste_genes,newgene)
        
    }
    return (liste_genes)
}

#phase de test
#data <- data.frame(ch = c("1"),
#                   st = c("5671734"),
#                   end = c("6337629"))

#print(data)

#CallDB1(data)

