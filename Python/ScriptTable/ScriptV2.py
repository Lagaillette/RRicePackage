# Imports
import urllib.request
import pandas as pd
from pandas.io.common import EmptyDataError

import gzip
import requests
import io


#local file in the same folder
url = "http://rapdb.dna.affrc.go.jp/download/archive/RAP-MSU_2017-04-14.txt.gz"

#headers of the file
headers = {'RAP': 'LOC'}

#Give the entire name of the file with the extension .gz
filename = url.split("/")[-1]

#Give the name of the file without .gz
uncompressName = filename[:-3]

#Fetch the file by the url and decompress it
r = requests.get(url, headers=headers)
decompressedFile = gzip.decompress(r.content)


with open(uncompressName, "r+b") as f:
    f.write(decompressedFile)

    # Import file tab-delimited
    try:
        array = pd.read_csv(f, sep="\t", header=None)
    except EmptyDataError:
        array = pd.DataFrame()
    # Named columns
    array.columns = ["RAP", "LOC"]

    while 1:
        choice = input('\n\n1Choose your conversion :\n 1)RAP to LOC\n 2)LOC to RAP\n 3)Exit\n')

        if (choice == '1'):

            # Save the entered RAP ID
            RapID = input('RAP ID : ')
            print("\n")

            # Find the line corresponding to the entered RAP ID
            data = array.loc[array['RAP'] == RapID]

            # Print the corresponding LOC ID
            print(data["LOC"])

        elif (choice == '2'):

            # Save the entered RAP ID
            RapID = input('LOC ID : ')
            print("\n")

            # Find the line corresponding to the entered RAP ID
            data = array.loc[array['LOC'] == RapID]

            # Print the corresponding LOC ID
            print(data["RAP"])

        elif (choice == '3'):

            # End of the loop
            raise SystemExit(0)


        else:
            print("Error ! Choose 1, 2 or 3")

    f.close()






