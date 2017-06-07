# Imports
import pandas as pd
import gzip
import requests

#local file in the same folder
file = 'RAP-MSU.txt'
#Import file tab-delimited
array = pd.read_csv(file, sep="\t", header = None)
#Named columns
array.columns = ["RAP", "LOC"]


while 1:
    choice = input('\n\n1Choose your conversion :\n 1)RAP to LOC\n 2)LOC to RAP\n 3)Exit\n')

    if(choice == '1'):

        #Save the entered RAP ID
        RapID = input('RAP ID : ')
        print("\n")

        #Find the line corresponding to the entered RAP ID
        data = array.loc[array['RAP'] == RapID]

        #Print the corresponding LOC ID
        print(data["LOC"])

    elif (choice == '2'):

        #Save the entered RAP ID
        RapID = input('LOC ID : ')
        print("\n")

        #Find the line corresponding to the entered RAP ID
        data = array.loc[array['LOC'] == RapID]

        #Print the corresponding LOC ID
        print(data["RAP"])

    elif (choice == '3'):

        #End of the loop
        raise SystemExit(0)


    else:
        print("Error ! Choose 1, 2 or 3")