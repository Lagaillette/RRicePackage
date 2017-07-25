#!/usr/bin/env python3

import requests
import re
import os
import pprint
import json
import csv


def snpSeekAll(string):
    print(string)
    # string look like : Os01:1..43,270,923
    contig = 'chr'+string[2:4]
    start = string[5]
    print(start)
    end = string[8:].split(",")
    end = end[0] + end[1] + end[2]
    print("c parti")
    Log = open('log.txt', 'w')
    url = 'http://snp-seek.irri.org/ws/genomics/gene/osnippo/'
    u = ''
    model = '&model=rap\n'  # '&model=msu7\n'
    data = []

    Log.write(url + contig + '?' + 'start=' + start + '&end=' + end + '&model=msu7\n')
    try:
        # u = urllib.urlopen(url + contig + '?' + 'start='+ start + '&end='+ end+'&model=msu7\n')
        urlFind = url + contig + '?' + 'start=' + start + '&end=' + end + '&model=msu7'
        r = requests.get(urlFind)
        # encodage en bytes et pas en string  d'ou le decode
        data = json.loads(r.content.decode('UTF-8'))
    except:
        Log.write(url + contig + '?' + 'start=' + start + '&end=' + end + '&model=msu7\n')
        pass
    locus = contig + ':' + start + '-' + end

    print(data)

    # retourne un tableau
    with open("fileid.txt", 'a') as myfile:
        wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
        wr.writerow(data)
        myfile.close

