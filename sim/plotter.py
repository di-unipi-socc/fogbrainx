
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

def plot_cumulative_sequence(data, property, attribute, folder):
    key = "cumulative-"+property+"-"+attribute

    filename = folder+"/"+key+".png"
    plt.title(key)
    delete_file(filename)

    for d in data:
        values_cr = []
        values_nocr = []
        for l in d:
            try:
                values_cr.append((int(l),float(d[l]["cr"][property]["aggregate"][attribute])))
                values_nocr.append((int(l),float(d[l]["nocr"][property]["aggregate"][attribute])))
            except:
                pass

        values_cr.sort()
        values_nocr.sort()
        x = []
        y_cr = []
        y_nocr = []
        for k,v in values_cr:
            x.append(k)
            y_cr.append(v)
        for _,v in values_nocr:
            y_nocr.append(v)

        plt.plot(y_cr, label=(d["fogBrain-version"]+"-"+d["placer"]).replace("../placers/","").replace("../",""), marker='o', linestyle='dashed')
        plt.plot(y_nocr, label=("exhaustive-"+d["placer"]).replace("../placers/","").replace("../",""), marker='o', linestyle='dashed')

    current_handles, current_labels = plt.gca().get_legend_handles_labels()
    sorted_handles = list(reversed(current_handles))
    sorted_labels = list(reversed(current_labels))
    plt.legend(sorted_handles,sorted_labels)

    plt.xlabel("infrastructure dynamicity (%)")

    plt.xticks(range(len(x)), x)
    plt.tight_layout()
    plt.savefig(filename, dpi=199)
    plt.clf()

def plot_sequence(data, value, property, attribute, folder):
    key = value+"-"+property+"-"+attribute

    filename = folder+"/"+key+".png"
    plt.title(key)
    delete_file(filename)

    for d in data:
        values = []
        for l in d:
            try:
                values.append((int(l),float(d[l][value][property]["aggregate"][attribute])))
            except:
                pass

        values.sort()
        x = []
        y = []
        for k,v in values:
            x.append(k)
            y.append(v)

        if value == "nocr":
            plt.plot(y, label=("exhaustive-"+d["placer"]).replace("../placers/","").replace("../",""), marker='o', linestyle='dashed')
        else:
            plt.plot(y, label=(d["fogBrain-version"]+"-"+d["placer"]).replace("../placers/","").replace("../",""), marker='o', linestyle='dashed')

    current_handles, current_labels = plt.gca().get_legend_handles_labels()
    sorted_handles = list(reversed(current_handles))
    sorted_labels = list(reversed(current_labels))
    plt.legend(sorted_handles,sorted_labels)

    plt.xlabel("infrastructure dynamicity (%)")

    plt.xticks(range(len(x)), x)
    plt.tight_layout()
    plt.savefig(filename, dpi=199)
    plt.clf()

def plot_analysis(data, folder):

    if not isinstance(data, list):
        data = [data]

    folder = "./results/"+folder+"/plots"
    create_folder(folder)

    for v,d1 in data[0]["0"].items():
        for p,d2 in d1.items():
                for a,d3 in d2["aggregate"].items():
                    plot_sequence(data, v, p, a, folder)

    for p,d2 in data[0]["0"]["cr"].items():
        for a,d3 in d2["aggregate"].items():
            plot_cumulative_sequence(data, p, a, folder)


def multi_plot(steps):
    folder = "multi/"
    create_folder("./results/"+folder)
    folder = folder+str(steps)+"/"
    create_folder("./results/"+folder)

    data = []
    for fb in os.listdir("./results"):
        if fb != "multi-plots":
            for p in os.listdir("./results/"+fb):
                try:
                    with open("./results/"+fb+"/"+p+"/"+str(steps)+"/analysis.json", "r") as f:
                        data.append(json.loads(f.read()))
                except:
                    pass
    plot_analysis(data, folder)

def plot_assessment(data, folder):
    
    if not isinstance(data, list):
        data = [data]

    folder = "./results/"+folder+"/plots"
    create_folder(folder)

    base = data[0]
    l = []

    for n,d0 in base.items():
        try:
            for p,d1 in d0.items():
                for v,d2 in d1.items():
                    for a,d3 in d2.items():
                        for d in d3["aggregate"].keys():
                            l.append((v,a,d))
        except:
            pass
    
    l = sorted(list(set([ tuple(t) for t in l ])))
    for d_tmp in data:
        for v,a,d in l:
            multi_plot_assessment(d_tmp, v, a, d, folder)

def multi_plot_assessment(data, v, a, d, folder):
    key = v+"-"+a+"-"+d

    filename = folder+"/"+key+".png"
    plt.title(key)
    delete_file(filename)

    nodes = []
    values = []
    probs = []
    for n,data0 in data.items():
        try:
            for l,_ in data0.items():
                    values.append((int(l),int(n),float(data[n][l][v][a]["aggregate"][d])))
                    nodes.append(int(n))
                    probs.append(int(l))
        except:
                pass

    nodes = sorted(list(dict.fromkeys(nodes)))
    probs = sorted(list(dict.fromkeys(probs)))
    values = sorted(values)

    tmp = []

    for l in probs:
        tmp = []
        for n in nodes:
            try:
                for l1,n1,val in values:
                    if l1 == l and n1 == n:
                        tmp.append(val)
            except:
                pass
        plt.plot(tmp, label=str(l)+"%", marker='o', linestyle='dashed')

    current_handles, current_labels = plt.gca().get_legend_handles_labels()
    sorted_handles = list(reversed(current_handles))
    sorted_labels = list(reversed(current_labels))
    plt.legend(sorted_handles,sorted_labels)

    plt.xlabel("nodes")
    plt.tight_layout()
    plt.xticks(range(len(nodes)), nodes)
    plt.savefig(filename, dpi=199)
    plt.clf()
                    
if __name__ == "__main__":

    with open("./results/fbX2Official/placer/70/analysis.json", "r") as f:
        plot_assessment(json.loads(f.read()), "fbX2Official/placer/70")
    #multi_plot(sys.argv[1])
    """
    with open("./results/"+str(sys.argv[1])+"/analysis.json", "r") as f:
        data = json.loads(f.read())
    plot_analysis(data, sys.argv[1])
    """