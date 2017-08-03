import os
import requests

def existFile(pathToFile):
    """
    pathToFile: String
    :return: return True if the file already exist, else return False
    """
    return (os.path.isfile(pathToFile))


def formatPathToFile(nameFile):
    """
    nameFile: String
    :return: return the entire path to the file
    """

    # remove char until '/'
    pathToFile = os.path.dirname(__file__)
    while not (pathToFile.endswith('/')):
        pathToFile = pathToFile[0:-1]

    pathToFile += 'resources/'+nameFile
    return pathToFile


def loadFileURL(nameFile, url):

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