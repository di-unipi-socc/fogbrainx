import sys
import configparser

import sim as s
import engine as e
import analyser as a
import plotter as p

def parse_node(node, values):
    return "node("+node+", "+str(values["sw"]).replace("'","")+", "+str(values["hw"])+", "+str(values["things"]).replace("'","")+")."

def parse_link(link, values):
    endpoints = link.split("-")
    return "link("+endpoints[0]+", "+endpoints[1]+", "+str(values["lat"])+", "+str(values["up"])+")."

def parse_infra(infra):
    s = ":-dynamic link/4.\n:-dynamic node/4.\n\n"
    for n,v in infra["nodes"].items():
        s+=parse_node(n,v)+"\n"
    s+="\n"
    for l,v in infra["links"].items():
        s+=parse_link(l,v)+"\n"

    return s

def add_links(infra):
    for node in infra.get_nodes():
        for node1 in infra.get_nodes():
            if node != node1:
                nic = node.get_active_nics()[0]
                nic1 = node1.get_active_nics()[0]
                infra.add_link_from_nics(node, nic, node1, nic1)            

def make_infra():
    infra = s.DynamicInfra()

    infra.add_template("AWSNIC", {
        "#down": {'#dynamic': {'avg': 150, 'std': 20, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#up": {'#dynamic': {'avg': 150, 'std': 20, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#lat": {'#dynamic': {'avg': 100, 'std': 20, 'min': 0, '#function': 'limited_gaussian_dist'}}, 
        '#failure': {'#failure': 0.01},  
    })

    infra.add_template("AzureNIC", {
        "#down": {'#dynamic': {'avg': 100, 'std': 10, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#up": {'#dynamic': {'avg': 100, 'std': 10, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#lat": {'#dynamic': {'avg': 30, 'std': 5, 'min': 0, '#function': 'limited_gaussian_dist'}}, 
        '#failure': {'#failure': 0.01},
    })

    infra.add_template("UnipiNIC", {
        "#down": {'#dynamic': {'avg': 800, 'std': 200, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#up": {'#dynamic': {'avg': 800, 'std': 200, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#lat": {'#dynamic': {'avg': 3, 'std': 2, 'min': 0, '#function': 'limited_gaussian_dist'}}, 
        '#failure': {'#failure': 0.01},
    })

    infra.add_template("ADSL10NIC", {
        "#down": {'#dynamic': {'avg': 9.8, 'std': 0.12, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#up": {'#dynamic': {'avg': 0.856, 'std': 0.009, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#lat": {'#dynamic': {'avg': 27.7, 'std': 0.7, 'min': 0, '#function': 'limited_gaussian_dist'}}, 
        '#failure': {'#failure': 0.01}, 
    })

    infra.add_template("VDSL100NIC", {
        "#down": {'#dynamic': {'avg': 91.8, 'std': 1.6, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#up": {'#dynamic': {'avg': 20.6, 'std': 0.055, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#lat": {'#dynamic': {'avg': 16.4, 'std': 1.2, 'min': 0, '#function': 'limited_gaussian_dist'}}, 
        '#failure': {'#failure': 0.01}, 
    })

    infra.add_template("VDSL20NIC", {
        "#down": {'#dynamic': {'avg': 17.7, 'std': 0.128, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#up": {'#dynamic': {'avg': 0.872, 'std': 0.216, 'min': 0, '#function': 'limited_gaussian_dist'}},
        "#lat": {'#dynamic': {'avg': 14.7, 'std': 4.1, 'min': 0, '#function': 'limited_gaussian_dist'}}, 
        '#failure': {'#failure': 0.01},  
    })

    infra.add_template("AWSNode", {
        "hw": {'#dynamic': {'max': 1024, 'avg': 896, 'std': 128, 'min': 0, '#function': 'limited_gaussian_dist'}}, #da sperimentare
        "sw": {'#value': ["java", "go", "mongodb", "dotnet", "dotnet", "mySQL", "rabbitmq", "nodejs"]}, 
        "things": {'#value': []}, 
        '#failure': {'#failure': 0.0005}, 
        '#NICs': {"AWSLink": { "#template": "AWSNIC" }}
    })

    infra.add_template("AzureNode", {
        "hw": {'#dynamic': {'max': 4096, 'avg': 3584, 'std': 512, '#function': 'limited_gaussian_dist'}},
        "sw": {'#value': ["java", "go", "mongodb", "dotnet", "dotnet", "mySQL", "rabbitmq", "nodejs"]}, 
        "things": {'#value': []}, 
        '#failure': {'#failure': 0.001}, 
        '#NICs': {"AzureLink": { "#template": "AzureNIC" }}
    })

    infra.add_template("UnipiVM", {
        "hw": {'#dynamic': {'max': 2048, 'avg': 1792, 'std': 256, '#function': 'limited_gaussian_dist'}},
        "sw": {'#value': ["java", "go", "mongodb", "dotnet", "dotnet", "mySQL", "rabbitmq", "nodejs"]},
        "things": {'#value': []}, 
        '#failure': {'#failure': 0.01}, 
        '#NICs': {"UnipiLink": { "#template": "UnipiNIC" }}
    })

    infra.add_template("RaspberryPi", {
        "hw": {'#dynamic': {'max': 1024, 'avg': 896, 'std': 128, '#function': 'limited_gaussian_dist'}},
        "sw": {'#value': ["java", "nodejs", "rabbitmq"]}, 
        "things": {'#value': ["user"]}, 
        '#failure': {'#failure': 0.05}, 
        '#NICs': {
            "ADSL10": {"#template": "ADSL10NIC" },
            "VDSL100": {"#template": "VDSL100NIC" },
            "VDSL20": {"#template": "VDSL20NIC" },
            "UnipiLink": {"#template": "UnipiNIC" },
        }
    })

    #2 sem 2019 Tuscany https://misurainternet.it/valori_statistici/?csrfmiddlewaretoken=ylfmKFeZEAPxkH3Q7N9AJXL5MUW0kmtn60mfQDxGNKfkcHUtryQbBpzPyW9wGiCN&operatore=vdf004&regione=09&anno=2019&periodo=semestre_ii

    infra.add_node("f1", "AWSNode", details={"#activeNICs": ["AWSLink"]})
    infra.add_node("f2", "AWSNode", details={"#activeNICs": ["AWSLink"]})

    infra.add_node("g1", "AzureNode", details={"#activeNICs": ["AzureLink"]})
    infra.add_node("g2", "AzureNode", details={"#activeNICs": ["AzureLink"]})
    infra.add_node("g3", "AzureNode", details={"#activeNICs": ["AzureLink"]})

    infra.add_node("a", "RaspberryPi", details={"#activeNICs": ["ADSL10"]})
    infra.add_node("b", "RaspberryPi", details={"#activeNICs": ["VDSL100"]})
    infra.add_node("c", "RaspberryPi", details={"#activeNICs": ["VDSL20"]})

    infra.add_node("d1", "RaspberryPi", details={"#activeNICs": ["UnipiLink"]})
    infra.add_node("d2", "RaspberryPi", details={"#activeNICs": ["UnipiLink"]})
    infra.add_node("d3", "RaspberryPi", details={"#activeNICs": ["UnipiLink"]})
    infra.add_node("d4", "RaspberryPi", details={"#activeNICs": ["UnipiLink"]})

    infra.add_node("e", "UnipiVM", details={"#activeNICs": ["UnipiLink"]})

    add_links(infra)

    return infra

def run_infra(infra, steps, seed, prob, folder="./infra/"):
    try:
        import os
        os.mkdir(folder)
    except:
        pass
    cnt = 0
    next(infra.run(1, seed))
    for i in infra.run(steps, seed, prob):
        with open(folder+"infra"+str(cnt)+".pl","w+") as f:
            f.write(parse_infra(i))
        cnt+=1

"""
The default app configuration: 
in case a configuration is not found or 
some data is missing
"""
DEFAULT_CONFIGURATION = { 
    "SEED": None,
    "STEPS": 100,
    "PROB": 1,
    "FOGBRAIN": '../fbX2Official',
    "PLACER": "../placers/placer",
    "MULTI": False
}

def get_config(configuration=None):
    """ Returns a json file containing the configuration to use in the app
    The configuration to be used can be passed as a parameter, 
    otherwise the one indicated by default in config.ini is chosen
    ------------------------------------
    [CONFIG]
    CONFIG = The_default_configuration
    ------------------------------------
    Params:
        - configuration: if it is a string it indicates the configuration to choose in config.ini
    """
    try:
        parser = configparser.ConfigParser()
        if parser.read('config.ini') != []:
            
            if type(configuration) != str: # if it's not a string, take the default one
                configuration = parser["CONFIG"]["CONFIG"]

            print("- Simulation CONFIGURATION: ",configuration)
            configuration = parser._sections[configuration] # get the configuration data

            parsed_configuration = {}
            for k,v in configuration.items(): # Capitalize keys and translate strings (when possible) to their relative number or boolean
                k = k.upper()
                parsed_configuration[k] = v
                try:
                    parsed_configuration[k] = int(v)
                except:
                    try:
                        parsed_configuration[k] = float(v)
                    except:
                        if v == "true":
                            parsed_configuration[k] = True
                        elif v == "false":
                            parsed_configuration[k] = False

            for k,v in DEFAULT_CONFIGURATION.items():
                if not k in parsed_configuration: # if some data are missing enter the default ones
                    parsed_configuration[k] = v

            return parsed_configuration
        else:
            print("- Simulation Configuration File NOT FOUND")
            print("- Simulation RUNNING: Default Configuration")
            return DEFAULT_CONFIGURATION
    except Exception as e:
        print("- Simulation CONFIGURATION ERROR: %s",e)
        print("- Simulation RUNNING: Default Configuration")
        return DEFAULT_CONFIGURATION

def orchestrate(steps, seed, prob, fogbrain, placer):
    print("Starting Simulation")
    print(" steps: ", steps)
    print(" seed: ", seed)
    print(" prob: ", prob)
    print(" fogbrain: ", fogbrain)
    print(" placer: ", placer)
    print("\nMaking Infras")
    infra = make_infra()
    run_infra(infra, steps, seed, prob)

    fogbrain_name = fogbrain.split("/")[-1]
    placer_name = (placer.split("/")[-1])
    folder = fogbrain_name+"/"+placer_name+"/"+str(steps)+"/"+(str(int(prob*100)))

    print("Running Simulation")
    faults = e.main(fogbrain, placer, steps, "./commits/", folder)
    print("Executing Analysis")


    data = a.analyse("./raw/"+folder)
    data["faults"] = faults
    data["fogBrain-version"] = fogbrain
    data["placer"] = placer
    data["steps"] = steps
    data["seed"] = seed
    data["prob"] = prob
    a.save_analysis(data, folder)

def multi_analysis(steps, seed, fogbrain, placer):
    print("Executing Multi Analysis")
    fogbrain_name = fogbrain.split("/")[-1]
    placer_name = (placer.split("/")[-1])
    folder = fogbrain_name+"/"+placer_name+"/"+str(steps)
    data = a.analyse("./raw/"+folder)
    data["fogBrain-version"] = fogbrain
    data["placer"] = placer
    data["steps"] = steps
    data["seed"] = seed
    data["multi"] = True
    a.save_analysis(data, folder)
    print("Plotting")
    p.plot_analysis(data, folder)
    p.multi_plot(steps)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        pass
    else:
        config = get_config()
        if config["MULTI"]:
            for i in range(0, 11):
                print("*** Start Prob ("+str(i*10)+"%)")
                orchestrate(config["STEPS"], config["SEED"], i/10, config["FOGBRAIN"], config["PLACER"])
            multi_analysis(config["STEPS"], config["SEED"], config["FOGBRAIN"], config["PLACER"])
        else:
            orchestrate(config["STEPS"], config["SEED"], config["PROB"], config["FOGBRAIN"], config["PLACER"])
    