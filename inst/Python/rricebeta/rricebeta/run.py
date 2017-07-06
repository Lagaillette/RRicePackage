import os
import sys
import requests
import gzip
import csv
import pandas as pd
path = os.path.dirname(__file__)
sys.path.append(os.path.join(os.path.dirname(__file__), "lib"))
import snpSeek as snpSeek
import Scriptv7_Table as rapdb
import ScriptGramene as gramene
import ScriptV8_Oryzabase as oryzabase
import snpSeekAll as snpSeekAll

def formatPathToFile(nameFile):
    # on supprime le dernier char tant qu'on n'a pas rencontré '/'
    pathToFile = os.path.dirname(__file__)
    while not (pathToFile.endswith('/')):
        pathToFile = pathToFile[0:-1]

    pathToFile += 'resources/'+nameFile
    return pathToFile



def main():
    pathScript = sys.argv[0]
    contig = sys.argv[1]
    if len(contig)<2:
        contig =  'chr0'+contig # test if for 10 - 11 - 12
    else:
        contig =  'chr'+contig
    start = sys.argv[2]
    end = sys.argv[3]
    db = sys.argv[4]

    dataSnp = snpSeek.snpSeek(contig, start, end)
    hashmap = dataSnp[0]
    #geneID(contig, start, end, hashmap[], hashmap["raprepName"])

    if(db == "1"):
        if(hashmap["raprepName"]):
            dataRapdb = rapdb.rapdb(hashmap["raprepName"])
            print(dataRapdb)
        else:
            print("empty")

    elif(db == "2"):
        if(hashmap["raprepName"]):
            dataGramene = gramene.gramene(hashmap["raprepName"])
            print(dataGramene)
        else:
            print("empty")

    # Faire try
    elif(db == "3"):
        try:
            dataOryzabase = oryzabase.oryzabaseRapId(hashmap["raprepName"])
            print(hashmap["raprepName"])
            print("OK")
            print(dataOryzabase)
        except:
            #Si vide ya erreur
            print("no rap id")
            try:
                rapdbCGSNL = rapdb.rapdb(hashmap["raprepName"])
                dataOryzabase = oryzabase.oryzabaseCGSNL(rapdbCGSNL["CGSNL Gene Name"])
                print(dataOryzabase)
            except:
                print("not found")

    # Ecriture fichier
    elif(db == "4"):
        url = "http://rapdb.dna.affrc.go.jp/download/archive/RAP-MSU_2017-04-14.txt.gz"
        filename = url.split("/")[-1]

        # Give the name of the file without .gz
        pathToFile = formatPathToFile(filename[:-3])

        if (not os.path.isfile(pathToFile)):
            # Fetch the file by the url and decompress it
            r = requests.get(url)
            decompressedFile = gzip.decompress(r.content)
            # Create the file .txt
            with open(pathToFile, "w") as f:
                f.write(decompressedFile)
                f.close()
        newFile = formatPathToFile("geneID.txt")
        with open(newFile, "a") as f:
            # Import file tab-delimited
            try:
                array = pd.read_csv(pathToFile, sep="\t", header=None)
            except:
                array = pd.DataFrame()
            # Named columns
            array.columns = ["RAP", "LOC"]

            # Find the line corresponding to the entered RAP ID (Select LOC FROM LOC where RAP = RapID)
            data = array.loc[array['RAP'] == hashmap["raprepName"]]
            data.loc[:, 'iricname'] = hashmap['iricname']

            # Store the corresponding LOC ID and split the string
            print(data['iricname'])
            data.to_csv(f, sep='\t')

            f.close()

    # Plage chromosome
    # Cree le fichier fileID.txt
    elif(db == "5"):
        snpSeekAll.snpSeekAll("Os12:1..27,531,856")
        snpSeekAll.snpSeekAll("Os02:1..35,937,250")
        snpSeekAll.snpSeekAll("Os03:1..36,413,819")
        snpSeekAll.snpSeekAll("Os04:1..35,502,694")
        snpSeekAll.snpSeekAll("Os05:1..29,958,434")
        snpSeekAll.snpSeekAll("Os06:1..31,248,787")
        snpSeekAll.snpSeekAll("Os07:1..29,697,621")
        snpSeekAll.snpSeekAll("Os08:1..28,443,022")
        snpSeekAll.snpSeekAll("Os09:1..23,012,720")
        snpSeekAll.snpSeekAll("Os10:1..23,207,287")
        snpSeekAll.snpSeekAll("Os11:1..29,021,106")
        snpSeekAll.snpSeekAll("Os12:1..27,531,856")

    # Return the SnpSeek Call
    elif(db == "6"):
        return hashmap


# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()