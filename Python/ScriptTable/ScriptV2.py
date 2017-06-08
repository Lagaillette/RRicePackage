# Imports
import urllib.request
import pandas as pd
from pandas.io.common import EmptyDataError

import gzip
import requests
import io

#Init result
result = 0
i = 0

#local file in the same folder
url = "http://rapdb.dna.affrc.go.jp/download/archive/RAP-MSU_2017-04-14.txt.gz"

#Give the entire name of the file with the extension .gz
filename = url.split("/")[-1]

#Give the name of the file without .gz
uncompressName = filename[:-3]

#Fetch the file by the url and decompress it
r = requests.get(url)
decompressedFile = gzip.decompress(r.content)

# Create the file .txt
with open(uncompressName, "wb") as f:
    f.write(decompressedFile)
    f.close()

#Use the previous created file (.txt)
with open(uncompressName, "r+b") as file:

    # Import file tab-delimited
    try:
        array = pd.read_csv(file, sep="\t", header=None)
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

            # Store the corresponding LOC ID and split the string
            result = data['LOC'].str.split(',').str

        elif (choice == '2'):

            # Save the entered RAP ID
            RapID = input('LOC ID : ')
            print("\n")

            # Find the line corresponding to the entered RAP ID
            data = array.loc[array['LOC'] == RapID]

            # Store the corresponding RAP ID and split the string
            result = data['RAP'].str.split(',').str

        elif (choice == '3'):

            # End of the loop
            raise SystemExit(0)


        else:
            print("Error ! Choose 1, 2 or 3")

        #Loop for printing the result
        while (i<int(result.len())):
            print(result[i].values, end=' ', flush=True)
            i = i +1



    f.close()






