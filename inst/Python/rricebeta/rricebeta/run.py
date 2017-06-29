import os
import sys
path = os.path.dirname(__file__)
sys.path.append(os.path.join(os.path.dirname(__file__), "lib"))
from rricebeta import snpSeek as snpSeek
from rricebeta import Scriptv7_Table as rapdb

def main():
    pathScript = sys.argv[0]
    contig = sys.argv[1]
    start = sys.argv[2]
    end = sys.argv[3]
    db = sys.argv[4]

    dataSnp = snpSeek.snpSeek(contig, start, end)
    print(dataSnp)
    print(dataSnp[0]["raprepName"])
    rapdb.rapdb(dataSnp[0]["raprepName"])
    jsonRapdb = rapdb.rapdb(dataSnp[0]["raprepName"])
    return jsonRapdb


# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()