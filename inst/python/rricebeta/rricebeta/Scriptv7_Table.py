#!/usr/bin/env python3

import helper
import requests
from bs4 import BeautifulSoup
import os
import sys
import json

# declaration des parametres au tout debut du main

def formatPathToFile(nameFile):
    # on supprime le dernier char tant qu'on n'a pas rencontré '/'
    pathToFile = os.path.dirname(__file__)
    while not (pathToFile.endswith('/')):
        pathToFile = pathToFile[0:-1]

    pathToFile += 'resources/'+nameFile
    return pathToFile


def existFile(pathToFile):
    """

    :return: return True if the file already exist, else return False
    :rtype: Bool
    """
    return (os.path.isfile(pathToFile))

def rapdb(RAPID):
    #Parameters
    # RAPID_valide = "Os06g0654600"
    #End parameters
    try:
        html_page = requests.get("http://rapdb.dna.affrc.go.jp/tools/search/run?keyword=" + RAPID + "&submit=Search&id=on&size=10")
    except requests.exceptions.RequestException:
        print("Access denied or no internet connection")
        sys.exit(1)

    soup = BeautifulSoup(html_page.content, "lxml")
    result = soup.find('tr', attrs={"class": "result"})
    hashmap = {}
    try:
        rapid = result.find('td', attrs={"class": "c01"}).a.contents[0]
    except:
        print("Error : empty ID")
        rapid = RAPID
    try:
        description = result.find('td', attrs={"class": "c02"}).contents[0]
    except:
        print("Error : empty description")
        description = ""
    try:
        position = result.find('td', attrs={"class": "c03"}).a.contents[0]
    except:
        print("Error : empty position")
        position = ""
    try:
        RAP_symbol = result.find('td', attrs={"class": "c04"}).contents[0]
    except:
        print("Error : empty RAP symbol")
        RAP_symbol = ""
    try:
        RAP_name = result.find('td', attrs={"class": "c05"}).contents[0]
    except:
        print("Error : empty RAP_name")
        RAP_name = ""
    try:
        CGSNL_symbol = result.find('td', attrs={"class": "c06"}).contents[0]
    except:
        print("Error : empty CGSNL_symbol")
        CGSNL_symbol = ""
    try:
        CGSNL_name = result.find('td', attrs={"class": "c07"}).contents[0]
    except:
        print("Error : empty CGSNL_name")
        CGSNL_name = ""
    try:
        Oryzabase_symbol = result.find('td', attrs={"class": "c08"}).contents[0]
    except:
        print("Error : empty Oryzabase_symbol")
        Oryzabase_symbol = ""
    try:
        Oryzabase_name = result.find('td', attrs={"class": "c09"}).contents[0]
    except:
        print("Error : empty Oryzabase_name")
        Oryzabase_name = ""

    hashmap = {"ID": rapid,
               "Description": description,
               "Position": position,
               "RAP-DB Gene Symbol Synonym(s)": RAP_symbol,
               "RAP-DB Gene Name Synonym(s)": RAP_name,
               "CGSNL Gene Symbol": CGSNL_symbol,
               "CGSNL Gene Name": CGSNL_name,
               "Oryzabase Gene Symbol Synonym(s)": Oryzabase_symbol,
               "Oryzabase Gene Name Synonym(s)": Oryzabase_name
               }

    return json.dumps(hashmap)



