# Imports
import pandas as pd
import urllib.request

#local file in the same folder
file = 'RAP-MSU.txt'
#Import file tab-delimited
array = pd.read_csv(file, sep="\t", header = None)
#Named columns
array.columns = ["RAP", "LOC"]

#Save the entered RAP ID
RapID = input('RAP ID : ')
print("\n")

#Find the line corresponding to the entered RAP ID
data = array.loc[array['RAP'] == RapID]

#Print the corresponding LOC ID
print(data["LOC"])