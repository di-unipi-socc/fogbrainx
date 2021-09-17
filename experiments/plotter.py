
import os
import sys
import json
import itertools
import operator

import matplotlib.pyplot as plt

def create_folder(name):
    try:
        os.mkdir(name)
    except:
        pass

def delete_file(name):
    try:
        os.remove(name)
    except:
        pass

def plot_sequence(data, property, attribute, value, path, scale="linear", starter=""):
    key = property+"-"+attribute+"-"+value

    filename = path+"/"+starter+key+".png"

    if scale != "linear":
        filename = path+"/"+starter+key+"-"+scale+".png"

    #plt.title(key)
    delete_file(filename)

    values = []

    for l in data:
        if l != "0.6" and l != "1.0" and l != "0.8":
            for n in data[l]:
                if n != "2560":
                    values.append(((int(float(l)*100)),int(n),float(data[l][n][property][attribute]["aggregate"][value])))

    values.sort()
    groups = []
    for key,group in itertools.groupby(values,operator.itemgetter(0)):
        groups.append(list(group))

    nodes = []
    for ls in groups:
        records = []    
        for el in ls:
            label = el[0]
            nodes.append(el[1])
            records.append(el[2])
        
        plt.plot(records, label=label, marker='o', linestyle='dashed')

        
    
    current_handles, current_labels = plt.gca().get_legend_handles_labels()
    sorted_handles = list(reversed(current_handles))
    sorted_labels = list(reversed(current_labels))
    sorted_labels = map(lambda l: str(l)+"%", sorted_labels)
    plt.legend(sorted_handles,sorted_labels)
    

    nodes = sorted(list(dict.fromkeys(nodes)))

    plt.rcParams.update({'font.size': 14})

    plt.xlabel("Infrastructure Size")

    if attribute == "infs":
        plt.ylabel("Inferences")
    elif attribute == "time":
        plt.ylabel("Execution Time (s)")
    elif attribute == "migrationsperc":
        plt.ylabel("Migrations (%)")

    if property == "speedup":
        plt.ylabel("Speed-up")

    plt.yscale(scale)
    plt.xticks(range(len(nodes)), nodes)
    plt.tight_layout()
    plt.savefig(filename, dpi=199)
    plt.clf()

"""    
    plt.plot(ls, label=l, marker='o', linestyle='dashed')

    nodes = sorted(nodes)

    current_handles, current_labels = plt.gca().get_legend_handles_labels()
    sorted_handles = list(reversed(current_handles))
    sorted_labels = list(reversed(current_labels))
    plt.legend(sorted_handles,sorted_labels)

    plt.xlabel("nodes")
    plt.xticks(range(len(nodes)), nodes)
    plt.tight_layout()
    plt.savefig(filename, dpi=199)
    plt.clf()
"""

def plot_histogram(values, property, attribute, value, path, starter):
    key = property+"-"+attribute+"-"+value+"-histogram"

    filename = path+"/"+starter+key+".png"

    #plt.title(key)
    delete_file(filename)

    plt.hist(values,bins=100)

    plt.tight_layout()
    plt.savefig(filename, dpi=199)
    plt.clf()

if __name__ == "__main__":

    folder = "./results/"+str(sys.argv[1])
    try:
        starter = str(sys.argv[2])+"-"
    except:
        starter = ""

    with open(folder+"/analysis.json", "r") as f:
        data = json.loads(f.read())

    keys = set()

    for l in data:
        for n in data[l]:
            for p in data[l][n]:
                for a in data[l][n][p]:
                    for v in data[l][n][p][a]["aggregate"]:
                        keys.add((p,a,v))

    for k in keys:
        if k[2] == "all":
            #plot_histogram(data[l][n][p][a]["aggregate"][v], k[0], k[1], k[2], folder, starter)
            pass
        else:
            plot_sequence(data, k[0], k[1], k[2], folder, "linear", starter)
            if k[2] == "avg":
                plot_sequence(data, k[0], k[1], k[2], folder, "log", starter)

    if starter != "":
        if starter == "h-":
            folder = "./results/heuristic"
            create_folder(folder)
        elif starter == "e-":
            folder = "./results/exhaustive"
            create_folder(folder)

        for k in keys:
            if k[2] != "all":
                if k[2] == "avg" and (k[0] == "speedup" or (k[1] == "infs" or k[1] == "time" or k[1] == "migrationsperc")):
                    plot_sequence(data, k[0], k[1], k[2], folder, "linear", starter)
                    if k[2] == "avg":
                        plot_sequence(data, k[0], k[1], k[2], folder, "log", starter)

    
