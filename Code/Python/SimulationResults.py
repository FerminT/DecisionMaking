
import fit_meta_d_MLE
import csv
import numpy as np
from trials2counts import trials2counts
from fit_meta_d_MLE import fit_meta_d_MLE
from fit_rs_meta_d_MLE import fit_rs_meta_d_MLE

def main():
    print("Ingresá el path a la carpeta donde están los resultados de las simulaciones")
    path_sim = input()
    print("Cantidad de simulaciones:")
    nSimulaciones = int(input())
    print("Cantidad de trials:")
    nTrials = int(input())
    print("Cantidad de niveles de confianza:")
    nRatings = int(input())
    print("Ingresá el path a la carpeta donde se van a guardar las métricas")
    path_res = input()
    for nroSimulacion in range(1, nSimulaciones + 1):
        stimID, response, rating_BE, rating_RCE = parseCSV(path_sim, nTrials, nroSimulacion)
        nR_S1_BE, nR_S2_BE   = trials2counts(stimID, response, rating_BE, nRatings, 1) # El 1 es para agregar padding y que no hayan categorías en cero
        nR_S1_RCE, nR_S2_RCE = trials2counts(stimID, response, rating_RCE, nRatings, 1)
        fit_BE          = fit_meta_d_MLE(nR_S1_BE, nR_S2_BE)
        fit_RCE_general = fit_meta_d_MLE(nR_S1_RCE, nR_S2_RCE)
        fit_RCE         = fit_rs_meta_d_MLE(nR_S1_RCE, nR_S2_RCE)
        writeToCSV(path_res, fit_BE, fit_RCE_general, fit_RCE, nRatings, nroSimulacion)


def parseCSV(path, nTrials, nroSimulacion):
    path = path + "\\simulacion" + str(nroSimulacion) + ".csv"

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

def writeToCSV(path, datos_BE, datos_RCE_general, datos_RCE, nRatings, nroSimulacion):
    path = path + "\\metricas" + str(nroSimulacion) + ".csv"

    with open(path, 'w', newline='') as csvfile:
        fieldnames = ['rule', 'dp', 'meta_dp', 'meta_dp_rS1', 'meta_dp_rS2']
        for i in range(2):
            for j in range(nRatings - 1):
                fieldnames.append('HR2_rS' + str(i + 1) + '_' + str(j + 1))
            for k in range(nRatings - 1):
                fieldnames.append('FAR2_rS' + str(i + 1) + '_' + str(k + 1))

        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        writer.writeheader()

        rowValue = {}
        rowValue['rule']    = 'BE'
        rowValue['dp']      = datos_BE['da']
        rowValue['meta_dp'] = datos_BE['meta_da']
        for i in range(2):
            for j in range(nRatings - 1):
                rowValue['HR2_rS' + str(i + 1) + '_' + str(j + 1)] = datos_BE['obs_HR2_rS' + str(i + 1)][j]
            for k in range(nRatings - 1):
                rowValue['FAR2_rS' + str(i + 1) + '_' + str(k + 1)] = datos_BE['obs_FAR2_rS' + str(i + 1)][k]

        writer.writerow(rowValue)

        rowValue = {}
        rowValue['rule']        = 'RCE'
        rowValue['dp']          = datos_RCE_general['da']
        rowValue['meta_dp']     = datos_RCE_general['meta_da']
        rowValue['meta_dp_rS1'] = datos_RCE['meta_da_rS1']
        rowValue['meta_dp_rS2'] = datos_RCE['meta_da_rS2']
        for i in range(2):
            for j in range(nRatings - 1):
                rowValue['HR2_rS' + str(i + 1) + '_' + str(j + 1)] = datos_RCE_general['obs_HR2_rS' + str(i + 1)][j]
            for k in range(nRatings - 1):
                rowValue['FAR2_rS' + str(i + 1) + '_' + str(k + 1)] = datos_RCE_general['obs_FAR2_rS' + str(i + 1)][k]

        writer.writerow(rowValue)        

main()