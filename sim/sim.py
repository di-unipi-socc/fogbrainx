import random as rnd
import numpy as np
import copy

def deterministic_dist(specs, _):
    return rnd.randrange(specs["min"],specs["max"])

def gaussian_dist(specs, _):
    return np.random.normal(loc=specs["avg"], scale=specs["std"])

def limited_gaussian_dist(specs, _):
    new_value = np.random.normal(loc=specs["avg"], scale=specs["std"])
    if "max" in specs and new_value > float(specs["max"]):
        return float(specs["max"])
    if "min" in specs and new_value < float(specs["min"]):
        return float(specs["min"])
    return new_value
"""   
def positive_int_gaussian_dist(specs, _):
    new_value = int(round(np.random.normal(loc=specs["avg"], scale=specs["std"])))
    if new_value < 0:
        return 0 #-new_value
    return new_value

def left_gaussian_dist(specs, _):
    new_value = np.random.normal(loc=specs["avg"], scale=specs["std"])
    if new_value > float(specs["avg"]):
        return float(specs["avg"])
    return new_value
"""

class Component:

    def __init__(self, name, info={}):
        self._name = name
        self._info = {"#id":name, **info}
        self._dynamic_functions = {}

    def get_id(self):
        return self._name

    def set_attribute(self, name, value):
        self._info[name] = {"#value": value}

    def has(self, name):
        return name in self._info

    def set_failure(self, failure=None, failed=None):
        if "#failure" not in self._info:
            self._info["#failure"] = {
            } 
        if failure is not None:
            self._info["#failure"]["#failure"] = failure
        else:
            self._info["#failure"]["#failure"] = 0

        if failed is not None:
            self._info["#failure"]["#failed"] = failed
        else:
            self._info["#failure"]["#failed"] = False

    def is_failed(self):
        return "#failure" in self._info and "#failed" in self._info["#failure"] and self._info["#failure"]["#failed"]

    def set_dynamic_function(self, name, fun):
        self._dynamic_functions[name] = fun
    
    def _generate_new_value(self, specs, old_value):
        return self._dynamic_functions[specs["#function"]](specs, old_value)

    def set_dynamic_attribute(self, name, fun, info={}, value=None):
        self._info[name] = {
            "#value": value,
            "#dynamic": {
                "#function":fun,
                **info
            }
        }

    def run(self):
        if "#failure" in self._info:
            if rnd.random() <= self._info["#failure"]["#failure"]:
                self._info["#failure"]["#failed"] = True
            else:
                self._info["#failure"]["#failed"] = False

        for att,info in self._info.items():
            if info is not None and "#dynamic" in info:
                if "#value" not in self._info[att]:
                    self._info[att]["#value"] = self._generate_new_value(info["#dynamic"], None)
                else:
                    self._info[att]["#value"] = self._generate_new_value(info["#dynamic"], self._info[att]["#value"])

    def get(self):
        info = None
        if not self.is_failed():
            info = {}
            for att,values in self._info.items():
                if att[0] != "#" and "#value" in values:
                    if issubclass(type(values["#value"]), Component):
                         info[att] = values["#value"].get()
                    info[att] = values["#value"]
        return info

    def info(self):
        return self._info

    def items(self):
        return self._info.items()

    def as_template(self):
        template = copy.deepcopy(self._info)
        del template["#id"]
        del template["#template"]
        return template 

class NIC(Component):
    def __init__(self, name, info={}):
        Component.__init__(self, name, info)
        if "#up" not in self._info:
            self._info["#up"] = {"#value":None}
        if "#down" not in self._info:
            self._info["#down"] = {"#value":None}
        if "#lat" not in self._info:
            self._info["#lat"] = {"#value":None}
        if "#failure" not in self._info:
            self.set_failure(0)

    def get_up(self):
        return self._info["#up"]["#value"]

    def get_down(self):
        return self._info["#down"]["#value"]

    def get_lat(self):
        return self._info["#lat"]["#value"]

class Node(Component):

    def __init__(self, name, info={}):
        Component.__init__(self, name, info)
        self._nics = {}
        if "#NICs" not in self._info:
            self._info["#NICs"] = {}
        else:
            for nic,nic_info in self._info["#NICs"].items():
                self.add_nic(nic, details=nic_info)
        if "#activeNICs" not in self._info:
            self._info["#activeNICs"] = []
        

    def add_nic(self, name, up=None, down=None, lat=None, failure=None, details={}):
        nic = NIC(name, info=details)
        self._nics[name] = nic
        self._info["#NICs"][name] = nic.info()

    def has_nic(self, nic):
        return nic in self._info["#NICs"]

    def get_nic(self, nic):
        return self._info["#NICs"][nic]

    def set_active_nic(self, nic):
        self._info["#activeNICs"].append(nic)

    def get_nics(self):
        return self._nics

    def get_active_nics(self):
        nics = []
        for n in self._nics.values():
            if n.get_id() in self._info["#activeNICs"]:
                nics.append(n)

        return nics

    def run_nic(self, nic):
        if nic in self._info["#activeNICs"]:
            self._nics[nic].run()

    def set_dynamic_function(self, name, fun):
        Component.set_dynamic_function(self, name, fun)
        for n in self._nics.values():
            n.set_dynamic_function(name, fun)

class Link(Component):
    def __init__(self, source, sink, source_nic=None, sink_nic=None, info={}):
        Component.__init__(self, str(source.get_id())+"-"+str(sink.get_id()), info)
        self._info["#source"] = str(source.get_id())
        self._info["#sink"] = str(sink.get_id())
        self._source = source
        self._sink = sink
        self._source_nic = None
        self._sink_nic = None
        if source_nic is not None and sink_nic is not None:
            self._info["#source_nic"] = source_nic.get_id()
            self._info["#sink_nic"] = sink_nic.get_id()
            self._source_nic = source_nic
            self._sink_nic = sink_nic

    def get_source(self):
        return self._source

    def get_sink(self):
        return self._sink

    def get_source_nic(self):
        return self._source.get_nic(self._info["#source_nic"])

    def get_sink_nic(self):
        return self._sink.get_nic(self._info["#sink_nic"])

    def as_template(self):
        template = Component.as_template(self)
        del template["#source"]
        del template["#sink"]
        del template["#source_nic"]
        del template["#sink_nic"]
        return template

    def _connect_nics(self):
        self._source_nic.run()
        self._sink_nic.run()

        source_up = self._source_nic.get_up()
        source_down = self._source_nic.get_down()
        source_lat = self._source_nic.get_lat()
        source_failure = self._source_nic.is_failed()

        sink_up = self._sink_nic.get_up()
        sink_down = self._sink_nic.get_down()
        sink_lat = self._sink_nic.get_lat()
        sink_failure = self._sink_nic.is_failed()

        if source_up < sink_down:
            self.set_attribute("up", source_up)
        else:
            self.set_attribute("up", sink_down)

        if source_down < sink_up:
            self.set_attribute("down", source_down)
        else:
            self.set_attribute("down", sink_up)

        if source_lat > sink_lat:
            self.set_attribute("lat", source_lat)
        else:
            self.set_attribute("lat", sink_lat)

        if source_failure or sink_failure:
            self.set_failure(failed=True)

    def run(self):
        Component.run(self)
        if self._source_nic is not None and self._sink_nic is not None:
            self._connect_nics()


class Infra:

    def __init__(self):
        self._templates = {}
        self._nodes = {}
        self._links = {}

    def add_template(self, name, template):
        self._templates[name] = template

    def add_node(self, name, template=None, details={}):
        if template is not None and template in self._templates:
            info = {"#template":template, **self._templates[template], **details}
        else:
            info = {"#template":None, **details}

        new_info = copy.deepcopy(info)

        if "#NICs" in info:
            for k,att in info["#NICs"].items():
                for att,v in info["#NICs"][k].items():
                    if att == "#template" and v is not None and v in self._templates:
                        new_info["#NICs"][k].update(self._templates[v])

        node = Node(name, copy.deepcopy(new_info))
        self._nodes[name] = node  
        return node

    def add_link_from_nics(self, source, source_nic, sink, sink_nic, template=None, details={}):
        if source.get_id() in self._nodes and sink.get_id() and self._nodes:
            if (source_nic is None and sink_nic is None) or (source.has_nic(source_nic.get_id()) and sink.has_nic(sink_nic.get_id())):
                if template is not None and template in self._templates:
                    info = {"#template":template, **self._templates[template], **details}
                else:
                    info = {"#template":None, **details}

            link = Link(source, sink, source_nic, sink_nic, copy.deepcopy(info))  
            self._links[str(source.get_id())+"-"+str(sink.get_id())] = link
            return link
                    

    def add_link(self, source, sink, template=None, details={}):
        return self.add_link_from_nics(source, None, sink, None, template, details)

    def get(self):
        info = {"nodes":{}, "links":{}}
        for name,node in self._nodes.items():
            info["nodes"][name] = node.info()
        for name,link in self._links.items():
            info["links"][name] = link.info()
        return info

    def get_nodes(self):
        return list(self._nodes.values())

    def get_node(self, node):
        return self._nodes[node]

    def get_links(self):
        return list(self._links.values())

    def get_link(self, link):
        return self._links[link]

class DynamicInfra(Infra):

    def __init__(self):
        Infra.__init__(self)
        self._dynamic_functions = {}
        self.set_dynamic_function("deterministic_dist", deterministic_dist)
        self.set_dynamic_function("gaussian_dist", gaussian_dist)
        self.set_dynamic_function("limited_gaussian_dist", limited_gaussian_dist)
        #self.set_dynamic_function("positive_int_gaussian_dist", positive_int_gaussian_dist)
        #self.set_dynamic_function("left_gaussian_dist", left_gaussian_dist)

    def set_dynamic_function(self, name, fun):
        for n in self._nodes:
            n.set_dynamic_function(name, fun)
        for l in self._links:
            l.set_dynamic_function(name, fun)
        self._dynamic_functions[name] = fun

    def add_node(self, name, template=None, details={}):
        node = Infra.add_node(self, name, template, details)
        for name,fun in self._dynamic_functions.items():
            node.set_dynamic_function(name,fun)
        return node

    def add_link_from_nics(self, source, source_nic, sink, sink_nic, template=None, details={}):
        link = Infra.add_link_from_nics(self, source, source_nic, sink, sink_nic, template, details)
        for name,fun in self._dynamic_functions.items():
            link.set_dynamic_function(name,fun)
        return link


    def get_infra(self):
        info = {"nodes":{}, "links":{}}
        for name,node in self._nodes.items():
            n = node.get()
            if n is not None:
                info["nodes"][name] = n
        for name,link in self._links.items():
            l = link.get()
            if l is not None: 
                info["links"][name] = l
        return info

    def _set_seed(self, seed):
        if seed is not None:
            rnd.seed(seed)
            np.random.seed(seed)
                
    def step(self, seed=None, prob=1):
        
        self._set_seed(seed)

        for node in self._nodes.values():
            if rnd.random() < prob:
                node.run()

        for link in self._links.values():
            if rnd.random() < prob:
                link.run()

        return self.get_infra()

    def run(self, steps=1, seed=None, prob=1):
        
        self._set_seed(seed)

        for _ in range(steps):
            yield self.step(None, prob)

if __name__ == "__main__":
    
    infra = DynamicInfra()

    infra.add_template("fogNode", {
        "hw": {'#dynamic': {'min':0, 'max': 100, '#function': 'deterministic_dist'}},
        "sw": {'#value': ["os","sw1","sw2"]}, 
        "things": {'#value': []}, 
        '#failure': {'#failure': 0.01}, 
    })

    infra.add_template("fogLink", {
        "bw": {'#dynamic': {'min': 500, 'max': 1000, '#function': 'deterministic_dist'}},
        "lat": {'#dynamic': {'min': 5, 'max': 1500, '#function': 'deterministic_dist'}}, 
        '#failure': {'#failure': 0.01}, 
    })

    NODES = 3
    for i in range(NODES):
        infra.add_node("n"+str(i), "fogNode")

    for i in range(NODES):
        for j in range(NODES):
            infra.add_link(infra.get_node("n"+str(i)),infra.get_node("n"+str(j)), "fogLink")

    infra.step(42)
    print(infra.get_infra())
