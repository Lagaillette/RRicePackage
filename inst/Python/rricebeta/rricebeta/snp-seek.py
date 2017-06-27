import requests
import re
import os
import pprint
import json, csv, sys

params = {}
params["contig"] = "1"
params["start"] = "527906"
params["end"] = "842359"

def main(params):
    Log =  open('log.txt', 'w')
    url = 'http://snp-seek.irri.org/ws/genomics/gene/osnippo/'
    u = ''
    model = '&model=rap\n' #'&model=msu7\n'
    data = []

    """"
    with open('region.csv', 'rU') as input:
        for line in input:
            line = re.sub('\\r|\\n', '', line)
            idx = re.sub('\t|;|\n', '', line)
            params[idx] = re.split('\t|;|\n', line)
    input.close()
    """
    """
    params dictionnaire ressemble a ca
    params = {}
    params["contig"] = "1"
    params["start"] = "527906"
    params["end"] = "842359"
    
    """

    if len(params["contig"])<2:
        contig =  'chr0'+params["contig"] # test if for 10 - 11 - 12
    else:
        contig =  'chr'+params["contig"]

    Log.write(url + contig + '?' + 'start='+ params["start"] + '&end='+ params["end"] + '&model=msu7\n')
    try:
        #u = urllib.urlopen(url + contig + '?' + 'start='+ start + '&end='+ end+'&model=msu7\n')
        urlFind = url + contig + '?' + 'start='+ params["start"] + '&end='+ params["end"] +'&model=msu7'
        r = requests.get(urlFind)
        # encodage en bytes et pas en string  d'ou le decode
        data = json.loads(r.content.decode('UTF-8'))
    except:
        Log.write(url + contig + '?' + 'start='+ params["start"] + '&end='+ params["end"] +'&model=msu7\n')
        pass
    locus = contig + ':' + params["start"] + '-' + params["end"]

    #retourne un tableau
    print(data)


# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main(params)