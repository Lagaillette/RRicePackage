import os
import sys
import requests
import gzip
import pandas as pd
path = os.path.dirname(__file__)
sys.path.append(os.path.join(os.path.dirname(__file__), "lib"))
import snpSeek as snpSeek
import Scriptv7_Table as rapdb
import ScriptGramene as gramene
import ScriptV8_Oryzabase as oryzabase

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
    start = sys.argv[2]
    end = sys.argv[3]
    db = sys.argv[4]
    print(db)

    dataSnp = snpSeek.snpSeek(contig, start, end)
    hashmap = dataSnp[0]
    print(dataSnp)
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

    elif(db == "3"):
        dataOryzabase = oryzabase.oryzabaseRapId(hashmap["raprepName"])
        if(not dataOryzabase.empty):
            print("OK")
            print(dataOryzabase)
        else:
            print("no rap id")
            rapdbCGSNL = rapdb.rapdb(hashmap["raprepName"])
            dataOryzabase = oryzabase.oryzabaseCGSNL(rapdbCGSNL["CGSNL Gene Name"])
            print(dataOryzabase)

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
            with open(pathToFile, "wb") as f:
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
            data['iricname'] = hashmap['iricname']

            # Store the corresponding LOC ID and split the string
            print(data.to_csv)
            data.to_csv(f, sep='\t')

            f.close()





# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()