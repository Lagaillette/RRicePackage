library(jsonlite)

command ="python3"

#exemple RAPID = "Os06g0654600"
#RAPID = "Os06g0654600"

#pour appeler le fichier SCriptv7_Table.py -> BESOIN de l'attribut "RAPID"

#' Fonction qui appel le Scriptv7 de python
#'
#' Cette fonction permet à l'utilisateur de chercher si le RAPID existe dans Oryzabase.txt
#' @param RAPID
#' @keywords 
#' @export
#' @examples
#' Appel_Scriptv7("Os06g0654600")
Appel_Scriptv7 <- function (RAPID) {
  
#chemin pour n'importe quel utilisateur
debut = getwd()
path2script = paste(c(debut,"/RRicePackage/inst/Python/rricebeta/rricebeta/Scriptv7_Table.py"), collapse = '')

# Build up args in a vector
# RAPID_valide = "Os06g0654600" -> exemple valide
args = c(RAPID)

# Add path to script as first arg
allArgs = c(path2script, args)

Routput = system2(command, args=allArgs, stdout=TRUE)

#Appelé à chaque fois qu'elle rencontrera un print
print(Routput)
}

#Appel_Scriptv7(RAPID)


#Ma fonction CallDB1 

#' Fonction qui appel le run.py
#'
#' Description
#' @param Liste de locus
#' @keywords 
#' @export
#' @examples
#' Appel_Scriptv7(Liste de locus)
CallDB1 <- function (locusListe) {
  
  debut = getwd()
  path2script = paste(c(debut,"/RRicePackage/inst/Python/rricebeta/rricebeta/run.py"), collapse = '')
  
  for (i in 1 : nrow(locusListe)) {
    ch = as.character(locusListe[i,1])
    start = as.character(locusListe[i,2])
    end = as.character(locusListe[i,3])
    
    #print(paste(class(ch)," ",class(start)," ",class(end)))
    
    #appel du script python run.py avec les attributs (chx, start, end, DB) -> tous les attributs doivent etre en chaine de carac
    args = c(ch, start, end, "1")
    allArgs = c(path2script, args)
    Routput = system2(command, args=allArgs, stdout=TRUE)
    #print(Routput)
    test <- fromJSON(Routput)
    
    
    #for (i in 1:length(test)){
    #  print(paste("numero",i," -> ",test[i][1]))
    #}
    
    id = test["ID"]
    position = test["Position"]
    rap_symbole = test["RAP-DB Gene Symbol Synonym(s)"]
    cgsnl_name = test["CGSNL Gene Name"]
    ory_gene_symbole = test["Oryzabase Gene Symbol Synonym(s)"]
    description = test["Description"]
    rap_name = test["RAP-DB Gene Name Synonym(s)"]
    ory_gene_name = test["Oryzabase Gene Name Synonym(s)"]
    cgsnl_gene = test["CGSNL Gene Symbol"]
    
    #cat(paste("\n",id,"\n", position,"\n",rap_symbole,"\n",cgsnl_name,"\n",ory_gene_symbole,"\n",description,"\n",rap_name,"\n",ory_gene_name,"\n",cgsnl_gene))
    
    #prochaine étape -> créer objet et mettre tous les attributs dedans
  }
}

