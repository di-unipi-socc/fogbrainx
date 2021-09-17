# speedup medio 
#   time
#   infs
# tempo medio
#   cr
#   nocr
# infs medio
#   cr
#   nocr
# %migrazioni medie
#   cr
#   nocr

import os
import sys
import shutil
import numpy as np
import json

def filter_data(values):
    return values
    #values.remove(max(values))
    #values.remove(min(values))
    #values = list(filter(lambda a: float(a) > 1.0e-300, values))
    #values = list(filter(lambda a: float(a) < 1.0e+300, values))
    #return values

def filter_global_data(values):
    return values
    #values = np.delete(values, [np.argmax(values), np.argmin(values)])
    #values = list(filter(lambda a: float(a) > 1.0e-300, values))
    #values = list(filter(lambda a: float(a) < 1.0e+300, values))
    #return values

def is_outlier(points, thresh=300): #3.5
    """
    Returns a boolean array with True if points are outliers and False 
    otherwise.

    Parameters:
    -----------
        points : An numobservations by numdimensions array of observations
        thresh : The modified z-score to use as a threshold. Observations with
            a modified z-score (based on the median absolute deviation) greater
            than this value will be classified as outliers.

    Returns:
    --------
        mask : A numobservations-length boolean array.

    References:
    ----------
        Boris Iglewicz and David Hoaglin (1993), "Volume 16: How to Detect and
        Handle Outliers", The ASQC Basic References in Quality Control:
        Statistical Techniques, Edward F. Mykytka, Ph.D., Editor. 
    """
    if len(points.shape) == 1:
        points = points[:,None]
    median = np.median(points, axis=0)
    diff = np.sum((points - median)**2, axis=-1)
    diff = np.sqrt(diff)
    med_abs_deviation = np.median(diff)

    modified_z_score = 0.6745 * diff / med_abs_deviation

    return modified_z_score > thresh

def folder_statistics(folder):
    all_values = []#np.array([])
    for filename in os.listdir(folder):
        with open(folder+"/"+filename, "r") as f:
            values = f.readlines()
            values = list(map(lambda v: float(v), values))
            #values = filter_data(values)
            all_values = np.concatenate((all_values,values))
            #all_values += values    

    initial_len = len(all_values)
    #final_len = initial_len
    #all_values = filter_global_data(all_values)
    
    all_values = all_values[~is_outlier(all_values)]
    final_len = len(all_values)

    if "infs" in folder or "time" in folder:
        print(folder, initial_len-final_len)


    std = np.std(all_values)
    mean = np.average(all_values)
    """
    tot = 0
    cnt = 0
    
    for x in all_values:
        cnt+=1
        tot += x
    cnt -= 2
    tot -= np.amax(all_values)
    tot -= np.amin(all_values)
    print(folder)
    print(cnt, tot/cnt, mean)
    """

    return {
        "avg": mean,
        "min": np.amin(all_values),
        "max": np.amax(all_values),
        "std": std,
        "var": np.var(all_values),
        "median": np.median(all_values),
        #"rsd": std/mean
        #"all": all_values
        "deleted": initial_len-final_len,
    }

def file_statistics(file):
    with open(file, "r") as f:
        values = f.readlines()
        all_values = list(map(lambda v: float(v), values))
        #all_values = filter_data(values)

    #all_values = all_values[~is_outlier(all_values)]
    std = np.std(all_values)
    mean = np.mean(all_values)
    return {
        "avg": mean,
        "min": np.amin(all_values),
        "max": np.amax(all_values),
        "std": std,
        "var": np.var(all_values),
        "median": np.median(all_values),
        #"rsd": std/mean
        #"all": all_values
    }

def commits_statistics(folder):
    commits = {}
    for file in os.listdir(folder):
        commit = file.split(".")[0]
        commits[commit] = file_statistics(folder+file)

    return commits

def parse_raw_data(basefolder):
    
    data = {}

    for f in os.listdir(basefolder):
        current = basefolder+"/"+f
        if os.path.isdir(current):
            data[f] = parse_raw_data(current)
        else:
            return {
                    "aggregate": folder_statistics(basefolder+"/"),
                    "per_commit": commits_statistics(basefolder+"/")
                }
    
    return data


def create_folder(name):
    try:
        os.mkdir(name)
    except:
        pass

def delete_folder(name):
    shutil.rmtree(name, ignore_errors=True)

if __name__ == "__main__":

    data = parse_raw_data("./raw/"+str(sys.argv[1]))

    create_folder("./results")
    folder = "./results/"+str(sys.argv[1])
    create_folder(folder)
    with open(folder+"/analysis.json", "w") as f:
        f.write(json.dumps(data))

    
