import os
import pandas as pd
from pandas.io.common import EmptyDataError
import gzip
from bs4 import BeautifulSoup
import requests


class ScriptV5:

    #constructor
    def __init__(self):
        self.url = ""
        self.nameFile = ""
        self.pathToFile = os.getcwd() + '/' + self.nameFile

    def loadFileURL(self):

        """
        Download the file located in the rapdb download page

        """

        # URL of the web page to analyze
        html_page = requests.get("http://rapdb.dna.affrc.go.jp/download/irgsp1.html")
        soup = BeautifulSoup(html_page.content, "lxml")

        # Find links
        for link in soup.findAll('a'):
            linkfound = link.get('href')

            # Choose the file which begun by RAP-MSU
            if ("./archive/RAP-MSU_" in linkfound):
                # Format the URL
                self.url = "http://rapdb.dna.affrc.go.jp/download" + linkfound[1:]

        #Give the name of the file with the extension .gz
        filename = self.url.split("/")[-1]

        # Give the name of the file without .gz
        uncompressName = filename[:-3]

        # Fetch the file by the url and decompress it
        r = requests.get(self.url)
        decompressedFile = gzip.decompress(r.content)

        # Create the file .txt
        with open(uncompressName, "wb") as f:
            f.write(decompressedFile)
            f.close()

        # Store the name of the created file
        self.nameFile = uncompressName

        print("File created")


    def rapToLoc(self, RAPID):

        """
        Give the LOC ID that correspond to the entered RAP ID

        :param RAPID: the RAP ID that you want the corresponding LOC ID
        :type RAPID: String
        """

        # Use the previous created file (.txt)
        with open(self.nameFile, "r+b") as file:

            # Import file tab-delimited
            try:
                array = pd.read_csv(file, sep="\t", header=None)
            except EmptyDataError:
                array = pd.DataFrame()
            # Named columns
            array.columns = ["RAP", "LOC"]

            # Find the line corresponding to the entered RAP ID (Select LOC FROM LOC where RAP = RapID)
            data = array.loc[array['RAP'] == RAPID]

            # Store the corresponding LOC ID and split the string
            result = data['LOC'].str.split(',').str

            # reinitialize i
            i = 0
            # Loop for printing the result
            while (i < int(result.len())):
                print("LOC ID : " + result[i].values, end=' ', flush=True)
                i = i + 1
            print("\n")

            file.close()


    def locToRap(self, LOCID):
        """
        Give the RAP ID that correspond to the entered LOC ID

        :param LOCID: The LOC ID that you want te corresponding RAP ID
        :type LOCID: String
        """

        # Use the previous created file (.txt)
        with open(self.nameFile, "r+b") as file:

            # Import file tab-delimited
            try:
                array = pd.read_csv(file, sep="\t", header=None)
            except EmptyDataError:
                array = pd.DataFrame()
            # Named columns
            array.columns = ["RAP", "LOC"]

            # Find the line corresponding to the entered LOC ID (Select rap From RAP where LOC like LOCID)
            data = array[array['LOC'].str.contains(LOCID)]

            # Print the corresponding LOC ID
            print("RAP ID : " + data["RAP"].values, end=' ', flush=True)
            print("\n")

            file.close()

    def informations(self):
        """
            Print the informations of the class
        """
        print("Name : "+self.nameFile)
        print("URL : " +self.url)
