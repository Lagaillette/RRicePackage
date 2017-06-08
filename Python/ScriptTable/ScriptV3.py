# Imports
import urllib.request
import pandas as pd
from pandas.io.common import EmptyDataError

import gzip
import requests
import io

#If not URL specify, there is a default URL
def loadFileURL(url="http://rapdb.dna.affrc.go.jp/download/archive/RAP-MSU_2017-04-14.txt.gz"):

    # local file in the same folder
    #url = "http://rapdb.dna.affrc.go.jp/download/archive/RAP-MSU_2017-04-14.txt.gz"

    # Give the entire name of the file with the extension .gz
    filename = url.split("/")[-1]

    # Give the name of the file without .gz
    uncompressName = filename[:-3]

    # Fetch the file by the url and decompress it
    r = requests.get(url)
    decompressedFile = gzip.decompress(r.content)

    # Create the file .txt
    with open(uncompressName, "wb") as f:
        f.write(decompressedFile)
        f.close()

    # Return the name of the created file
    return uncompressName


def rapToLoc(RAPID):

    uncompressName = loadFileURL("http://rapdb.dna.affrc.go.jp/download/archive/RAP-MSU_2017-04-14.txt.gz")
    # Use the previous created file (.txt)
    with open(uncompressName, "r+b") as file:

        # Import file tab-delimited
        try:
            array = pd.read_csv(file, sep="\t", header=None)
        except EmptyDataError:
            array = pd.DataFrame()
        # Named columns
        array.columns = ["RAP", "LOC"]

        # Save the entered RAP ID
        RapID = input('RAP ID : ')
        print("\n")

        # Find the line corresponding to the entered RAP ID (Select LOC FROM LOC where RAP = RapID)
        data = array.loc[array['RAP'] == RapID]

        # Store the corresponding LOC ID and split the string
        result = data['LOC'].str.split(',').str

        # reinitialize i
        i = 0
        # Loop for printing the result
        while (i < int(result.len())):
            print("LOC ID : " + result[i].values, end=' ', flush=True)
            i = i + 1

        file.close()
