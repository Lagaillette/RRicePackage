#!/usr/bin/env python3

import requests
import re
import os
import pprint
import json

def snpSeek(contig, start, end):

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


    Log.write(url + contig + '?' + 'start='+ start + '&end='+ end + '&model=msu7\n')
    try:
        #u = urllib.urlopen(url + contig + '?' + 'start='+ start + '&end='+ end+'&model=msu7\n')
        urlFind = url + contig + '?' + 'start='+ start + '&end='+ end +'&model=msu7'
        r = requests.get(urlFind)
        # encodage en bytes et pas en string  d'ou le decode
        data = json.loads(r.content.decode('UTF-8'))
    except:
        Log.write(url + contig + '?' + 'start='+ start + '&end='+ end +'&model=msu7\n')
        pass
    locus = contig + ':' + start + '-' + end

    #retourne un tableau
    return data
