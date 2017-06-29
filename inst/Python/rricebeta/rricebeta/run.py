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
    print(dataSnp)
    if(db == "1"):
        rapdb.rapdb(dataSnp[0]["raprepName"])
        jsonRapdb = rapdb.rapdb(dataSnp[0]["raprepName"])
        print(jsonRapdb)

    elif(db == "2"):
        jsonGramene = gramene.gramene(dataSnp[0]["raprepName"])
        print(jsonGramene)

    elif(db == "3"):
        oryzabase.oryzabase()




# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()