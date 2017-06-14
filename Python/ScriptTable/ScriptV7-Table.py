import requests
from bs4 import BeautifulSoup


class RRice():

    def __init__(self):
        self.id = ""
        self.description = ""
        self.position = ""
        self.RAP_symbol = ""
        self.RAP_name = ""
        self.CGSNL_symbol = ""
        self.CGSNL_name = ""
        self.Oryzabase_symbol = ""
        self.Oryzabase_name = ""

    # It's working
    def rapToLoc(self, RAPID):

        html_page = requests.get("http://rapdb.dna.affrc.go.jp/tools/search/run?keyword="+RAPID+"&submit=Search&id=on&size=10")
        soup = BeautifulSoup(html_page.content, "lxml")
        result = soup.find('tr', attrs={"class": "result"})

        self.id = result.find('td', attrs={"class": "c01"}).a.contents[0]
        print("ID : " + self.id)
        self.description = result.find('td', attrs={"class": "c02"}).contents[0]
        print("Description : "+self.description)
        self.position = result.find('td', attrs={"class": "c03"}).a.contents[0]
        print("Position : "+self.position)
        self.RAP_symbol = result.find('td', attrs={"class": "c04"}).contents
        print(self.RAP_symbol)
        self.RAP_name = result.find('td', attrs={"class": "c05"}).contents
        print(self.RAP_name)
        self.CGSNL_symbol = result.find('td', attrs={"class": "c06"}).contents[0]
        print("CGSNL Gene Symbol : "+self.CGSNL_symbol)
        self.CGSNL_name = result.find('td', attrs={"class": "c07"}).contents[0]
        print("CGSNL Gene Name : "+self.CGSNL_name)
        self.Oryzabase_symbol = result.find('td', attrs={"class": "c08"}).contents[0]
        print("Oryzabase Gene Symbol Synonym(s) : "+self.Oryzabase_symbol)
        self.Oryzabase_name = result.find('td', attrs={"class": "c09"}).contents[0]
        print("Oryzabase Gene Name Synonym(s) : "+self.Oryzabase_name)

    # It doesn't work because of null tab
    def informations(self):
        print(
              "ID : "+self.id+
              "\nDescription : "+self.description+
              "\nPosition : "+self.position+
              "\nRAP-DB Gene Symbol Synonym(s) : " + self.RAP_symbol+
              "\nRAP-DB Gene Name Synonym(s) : " + self.RAP_name+
              "\nCGSNL Gene Symbol : "+self.CGSNL_symbol+
              "\nCGSNL Gene Name : "+self.CGSNL_name+
              "\nOryzabase Gene Symbol Synonym(s) : "+self.Oryzabase_symbol+
              "\nOryzabase Gene Name Synonym(s) : "+self.Oryzabase_name)





