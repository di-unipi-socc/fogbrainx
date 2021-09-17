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

def folder_statistics(folder):
    all_values = np.array([])
    for filename in os.listdir(folder):
        with open(folder+"/"+filename, "r") as f:
            values = f.readlines()
            values = np.array(values).astype(float)
            all_values = np.concatenate((all_values,values))

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
    }

def file_statistics(file):
    with open(file, "r") as f:
        all_values = f.readlines()
        all_values = np.array(all_values).astype(float)

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

def analyse(folder):
    data = parse_raw_data(folder)
    return data
    
def save_analysis(data, folder):
    create_folder("./results")
    folder = str(folder)
    folders = folder.split("/")
    folder = "./results/"
    create_folder(folder)
    for f in folders:
        folder += f+"/"
        create_folder(folder)
    with open(folder+"/analysis.json", "w") as f:
        f.write(json.dumps(data))

if __name__ == "__main__":
    data = analyse("./raw/"+str(sys.argv[1]))
    save_analysis(data, sys.argv[1])

    

    
