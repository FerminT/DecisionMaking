
import fit_meta_d_MLE
import csv
import numpy as np
from trials2counts import trials2counts
from fit_meta_d_MLE import fit_meta_d_MLE

def main():
    print("Ingresá el path a la carpeta de los resultados")
    path = input()
    print("Cantidad de trials:")
    nTrials = int(input())
    print("Cantidad de niveles de confianza:")
    nRatings = int(input())
    stimID, response, rating_BE, rating_RCE = parseCSV(path, nTrials)
    nR_S1_BE, nR_S2_BE   = trials2counts(stimID, response, rating_BE, nRatings, 1)
    nR_S1_RCE, nR_S2_RCE = trials2counts(stimID, response, rating_RCE, nRatings, 1)
    print(nR_S1_BE, nR_S2_BE)
    fit_BE = fit_meta_d_MLE(nR_S1_BE, nR_S2_BE)
    print(fit_BE)


def parseCSV(path, nTrials):
    path = path + "\simulacion1.csv"

    stimID     = np.empty(nTrials)
    response   = np.empty(nTrials)
    rating_BE  = np.empty(nTrials)
    rating_RCE = np.empty(nTrials)
    with open(path, newline='') as csvfile:
        streamReader = csv.DictReader(csvfile)
        i = 0
        for row in streamReader:
            stimID[i]     = row['estimulo']
            response[i]   = row['respuesta']
            rating_BE[i]  = row['confianza_BE']
            rating_RCE[i] = row['confianza_RCE']
            i += 1

    return stimID, response, rating_BE, rating_RCE

main()