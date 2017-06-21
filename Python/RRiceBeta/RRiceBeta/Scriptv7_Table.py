import requests
from bs4 import BeautifulSoup
import pandas as pd
import os


class ScriptV7():

    def __init__(self):
        self.pathToFile = ""

    # It's working
    def rapToLoc(self, RAPID):

        html_page = requests.get("http://rapdb.dna.affrc.go.jp/tools/search/run?keyword="+RAPID+"&submit=Search&id=on&size=10")
        soup = BeautifulSoup(html_page.content, "lxml")
        result = soup.find('tr', attrs={"class": "result"})
        hashmap = {}
        try:
            id = result.find('td', attrs={"class": "c01"}).a.contents[0]
        except:
            print("Empty error")
            id = RAPID
        try:
            description = result.find('td', attrs={"class": "c02"}).contents[0]
        except:
            print("Empty error")
            description = ""
        try:
            position = result.find('td', attrs={"class": "c03"}).a.contents[0]
        except:
            print("Empty error")
            position=""
        try:
            RAP_symbol = result.find('td', attrs={"class": "c04"}).contents[0]
        except:
            print("Empty error")
            RAP_symbol=""
        try:
            RAP_name = result.find('td', attrs={"class": "c05"}).contents[0]
        except:
            print("Empty error")
            RAP_name=""
        try:
            CGSNL_symbol = result.find('td', attrs={"class": "c06"}).contents[0]
        except:
            print("Empty error")
            CGSNL_symbol=""
        try:
            CGSNL_name = result.find('td', attrs={"class": "c07"}).contents[0]
        except:
            print("Empty error")
            CGSNL_name=""
        try:
            Oryzabase_symbol = result.find('td', attrs={"class": "c08"}).contents[0]
        except:
            print("Empty error")
            Oryzabase_symbol=""
        try:
            Oryzabase_name = result.find('td', attrs={"class": "c09"}).contents[0]
        except:
            print("Empty error")
            Oryzabase_name=""


        hashmap = {"ID" : id,
                   "Description" : description,
                   "Position" : position,
                   "RAP-DB Gene Symbol Synonym(s)" : RAP_symbol,
                   "RAP-DB Gene Name Synonym(s)" : RAP_name,
                   "CGSNL Gene Symbol" : CGSNL_symbol,
                   "CGSNL Gene Name" : CGSNL_name,
                   "Oryzabase Gene Symbol Synonym(s)" : Oryzabase_symbol,
                   "Oryzabase Gene Name Synonym(s)" : Oryzabase_name
                   }

        print(hashmap['CGSNL Gene Name'])
        print(len(hashmap))
        pathToFile = '../resources/OryzabaseGeneListEn.txt'
        if(self.existFile(pathToFile) == False):
            self.loadFileURL(pathToFile, "https://shigen.nig.ac.jp/rice/oryzabase/gene/download?classtag=GENE_EN_LIST")
        else:
            print("File already exist")
        print("Find file OK")
        self.oryzabase(hashmap)
        return hashmap





    def oryzabase(self, hashmap):
        # Import file tab-delimited
        try:
            array = pd.read_csv("../resources/OryzabaseGeneListEn.txt", sep="\t", encoding='utf-8')
            #array = pd.read_csv(self.file, sep="\t", names=['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology'])

        except NameError:
            array = pd.DataFrame()

        #array.columns = ['Trait Id', 'CGSNL Gene Symbol', 'Gene symbol synonym(s)', ' CGSNL Gene Name', 'Gene name synonym(s)', 'Protein Name', 'Allele', 'Chromosome No.', 'Explanation', 'Trait Class', 'RAP ID', 'GrameneId', 'Arm', 'Locate(cM)', 'Gene Ontology', 'Trait Ontology', 'Plant Ontology']
        if (hashmap["CGSNL Gene Name"]):
            print("Find by CGSNL Gene Name")
            data = array.loc[array['CGSNL Gene Name'] == hashmap["CGSNL Gene Name"]]
        elif (hashmap["ID"]):
            print("Find by RAP ID")
            data = array.loc[array['RAP ID'] == hashmap["ID"]]
        else:
            print("Empty")
        print(data)
        return data


    def loadFileURL(self, nameFile, url):

        """
        Download the file located in the rapdb download page

        """

        # Fetch the file by the url and decompress it
        r = requests.get(url)

        # Create the file .txt
        with open(nameFile, "wb") as f:
            f.write(r.content)
            print("File created")
            f.close()


    def existFile(self, pathToFile):
        """

        :return: return True if the file already exist, else return False
        :rtype: Bool
        """
        return (os.path.isfile(pathToFile))