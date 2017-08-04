import os
import requests
import sys

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


def connectionError(link):
    try:
        html_page = requests.get(link, allow_redirects=False)

        if (html_page.status_code == 302 or html_page.status_code == 307):
            print("Website maintenance")
            sys.exit(1)

        elif (html_page.status_code == 400):
            print("Bad request")
            sys.exit(1)


        elif (html_page.status_code == 403):
            print("Forbidden")
            sys.exit(1)


        elif (html_page.status_code == 404):
            print("Not found")
            sys.exit(1)


        elif (html_page.status_code == 429):
            print("Too Many Requests")
            sys.exit(1)


        elif (html_page.status_code == 500):
            print("Internal Server Error")
            sys.exit(1)


        elif (html_page.status_code == 503):
            print("Service Unavailable")
            sys.exit(1)


        elif (html_page.status_code == 504):
            print("Gateway Timeout")
            sys.exit(1)


        elif (html_page.status_code == 505):
            print("HTTP Version Not Supported")
            sys.exit(1)


        elif (html_page.status_code > 299):
            print("Unknow internet error")
            sys.exit(1)

        else:
            return html_page

    except requests.exceptions.RequestException:
        print("Unknow internet error")
        sys.exit(1)

    except requests.exceptions.Timeout:
        print("Timeout")
        sys.exit(1)


def connectionErrorPost(link, data):

    try:
        headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.0; WOW64; rv:24.0) Gecko/20100101 Firefox/24.0'}
        html_page = requests.post(link, data=data, headers=headers)


        if (html_page.status_code == 302 or html_page.status_code == 307):
            print("Website maintenance")
            sys.exit(1)

        elif (html_page.status_code == 400):
            print("Bad request")
            sys.exit(1)


        elif (html_page.status_code == 403):
            print("Forbidden")
            sys.exit(1)


        elif (html_page.status_code == 404):
            print("Not found")
            sys.exit(1)


        elif (html_page.status_code == 429):
            print("Too Many Requests")
            sys.exit(1)


        elif (html_page.status_code == 500):
            print("Internal Server Error")
            sys.exit(1)


        elif (html_page.status_code == 503):
            print("Service Unavailable")
            sys.exit(1)


        elif (html_page.status_code == 504):
            print("Gateway Timeout")
            sys.exit(1)


        elif (html_page.status_code == 505):
            print("HTTP Version Not Supported")
            sys.exit(1)


        elif (html_page.status_code > 299):
            print("Unknow internet error")
            sys.exit(1)

        else:
            return html_page

    except requests.exceptions.RequestException:
        print("Unknow internet error")
        sys.exit(1)

    except requests.exceptions.Timeout:
        print("Timeout")
        sys.exit(1)
