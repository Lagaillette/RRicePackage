import os
import sys
path = os.path.dirname(__file__)
sys.path.append(os.path.join(os.path.dirname(__file__), "lib"))
import snpSeek as snpSeek
import Scriptv7_Table as rapdb
import ScriptGramene as gramene
import ScriptV8_Oryzabase as oryzabase

def main():
    pathScript = sys.argv[0]
    contig = sys.argv[1]
    start = sys.argv[2]
    end = sys.argv[3]
    db = sys.argv[4]
    print(db)

    dataSnp = snpSeek.snpSeek(contig, start, end)
    hashmap = dataSnp[0]
    if(db == "1"):
        if(hashmap["raprepName"]):
            dataRapdb = rapdb.rapdb(hashmap["raprepName"])
            print(dataRapdb)
        else:
            print("empty")

    elif(db == "2"):
        dataGramene = gramene.gramene(hashmap["raprepName"])
        print(dataGramene)

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




# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()