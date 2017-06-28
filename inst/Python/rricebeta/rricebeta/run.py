import sys

def main():
    pathScript = sys.argv[0]
    contig = sys.argv[1]
    start = sys.argv[2]
    end = sys.argv[3]
    db = sys.argv[4]


# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()