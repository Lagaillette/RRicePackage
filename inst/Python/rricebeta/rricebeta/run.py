import sys

def main():
    pathScript = sys.argv[0]
    contig = sys.argv[1]
    start = sys.argv[2]
    end = sys.argv[3]

    # Fetch database
    for arg in sys.argv[4:]:
        print(arg)
        # Appel des scripts ici en fonction de la db precisee


# Pour eviter que le script soit execute lors d'un simple import
if __name__ == "__main__":
    main()