import os
import pandas as pd
from pandas.io.common import EmptyDataError
import gzip
from bs4 import BeautifulSoup
import requests


class ScriptV6:


    #constructor

    def __init__(self):
        """
            Test if the file already exists.
                If yes, it just prints informations.
                If no, it downloads the file on RapDB and prints informations
        """
        self.url = ""
        self.nameFile = ""
        self.pathToFile = ""

        self.findFileOnRapdb()
        # If the file already exists
        if(self.existFile() == True):
            print("File already exist :\nName : "+self.nameFile+"\nPath : "+self.pathToFile+"\n")
        # If the file doesn't exists
        else:
            self.loadFileURL()
            print("File downloaded :\nName : " + self.nameFile + "\nPath : " + self.pathToFile + "\nDonwloaded at : " + self.url+"\n")

    def findFileOnRapdb(self):

        # URL of the web page to analyze
        """
            Fetch the link of the most recent file RAP-MSU
            Initialize attributes at the creation of the class
        """
        html_page = requests.get("http://rapdb.dna.affrc.go.jp/download/irgsp1.html")
        soup = BeautifulSoup(html_page.content, "lxml")

        # Find links
        for link in soup.findAll('a'):
            linkfound = link.get('href')

            # Choose the file which begun by RAP-MSU
            if ("./archive/RAP-MSU_" in linkfound):
                # Format the URL
                self.url = "http://rapdb.dna.affrc.go.jp/download" + linkfound[1:]

        #Give the name of the file with the extension .gz (RAP*****.txt.gz)
        filename = self.url.split("/")[-1]

        # Initialize the name of the file without .gz ((RAP*****.txt)
        self.nameFile = filename[:-3]

        # Initialize the path where the file is located
        self.pathToFile = os.getcwd() + '/' + self.nameFile


    def loadFileURL(self):

        """
        Download the file located in the rapdb download page

        """

        # Fetch the file by the url and decompress it
        r = requests.get(self.url)
        decompressedFile = gzip.decompress(r.content)

        # Create the file .txt
        with open(self.nameFile, "wb") as f:
            f.write(decompressedFile)
            f.close()



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


    def existFile(self):
        return(os.path.isfile(self.pathToFile))


    def informations(self):
        """
            Print the informations of the class
        """
        print("Name : "+self.nameFile)
        print("Path : " + self.pathToFile)
        print("URL : " +self.url)
