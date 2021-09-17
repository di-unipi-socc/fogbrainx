class Infra:

    def __init__(self, file):
        self._nodes = {}
        self._links = {}
        self._file = file

    def addNode(self, node, sw, hw, things):
        if node not in self._nodes:
            self._nodes[node] = (sw,hw,things)

    def addLink(self, node1, node2, lat, bw):
        if node1 in self._nodes and node2 in self._nodes and node1 != node2 and (node1,node2) not in self._links:
            self._links[(node1,node2)] = (lat,bw)

    def modifyNode(self, node, sw, hw, things):
        if node in self._nodes:
            self._nodes[node] = (sw,hw,things)

    def modifyLink(self, node1, node2, lat, bw):
        if (node1,node2) in self._links:
            self._links[(node1,node2)] = (lat,bw)

    def removeNode(self, node):
        if node in self._nodes:
            del self._nodes[node]
            for (n1,n2) in self._links.copy():
                if n1 == node or n2 == node:
                    self.removeLink(n1,n2)

    def removeLink(self, node1, node2):
        if (node1,node2) in self._links:
            del self._links[(node1,node2)]

    def getNode(self, node):
        if node in self._nodes:
            (sw, hw, things) = self._nodes[node]
            return ("node("+node+", "+str(sw)+", "+str(hw)+", "+str(things)+").").replace("'","")
        return ""

    def getLink(self, node1, node2):
        if (node1,node2) in self._links:
            (lat, bw) = self._links[(node1,node2)]
            return ("link("+node1+", "+node2+", "+str(lat)+", "+str(bw)+").").replace("'","")
        return ""

    def addNodes(self, basename, number, sw, hw, things):
        nodes = []
        for i in range(number):
            name = basename + str(i)
            self.addNode(name, sw, hw, things)
            nodes.append(name)
        return nodes

    def addLinks(self, nodes1, nodes2, lat, bw):
        for n1 in nodes1:
            for n2 in nodes2:
                if n1 != n2:
                    self.addLink(n1, n2, lat, bw)

    def __str__(self):
        infra = ""
        for n in self._nodes:
            infra += self.getNode(n)+"\n"
        infra+="\n"
        for (n1,n2) in self._links:
            infra += self.getLink(n1,n2)+"\n"
        infra+="\n"
        return infra

    def upload(self, file=None):
        if file is None:
            file = self._file
        with open(file,"w") as f:
            f.write(str(self))

if __name__ == "__main__":

    infra = Infra("infra.pl")

    
    nodesnumber = 400

    CLOUDS = nodesnumber
    ISPS = nodesnumber
    CABINETS = nodesnumber
    ACCESSPOINTS = nodesnumber
    SMARTPHONES = nodesnumber

    clouds = []
    isps = []
    cabinets = []
    accesspoints = []
    smartphones = []

    clouds = infra.addNodes("cloud", CLOUDS, ["ubuntu", "mySQL", "gcc", "make"], "inf", [])
    isps = infra.addNodes("ispdatacentre", ISPS, ["ubuntu", "mySQL"], 50, [])
    cabinets = infra.addNodes("cabinetserver", CABINETS, ["ubuntu", "mySQL"], 20, [])
    accesspoints = infra.addNodes("accesspoint", ACCESSPOINTS, ["ubuntu", "gcc", "make"], 4, [])
    smartphones = infra.addNodes("smartphone", SMARTPHONES, ["android", "gcc", "make"], 8, [])

    infra.addLinks(clouds, clouds, 20, 1000)
    infra.addLinks(clouds, isps, 110, 1000)
    infra.addLinks(clouds, cabinets, 135, 100)
    infra.addLinks(clouds, accesspoints,  148, 20)
    infra.addLinks(clouds, smartphones, 150, 18)

    infra.addLinks(isps, clouds, 110, 1000)
    infra.addLinks(isps, isps, 20, 1000)
    infra.addLinks(isps, cabinets, 25, 500)
    infra.addLinks(isps, accesspoints, 38, 50)
    infra.addLinks(isps, smartphones, 20, 1000)

    infra.addLinks(cabinets, clouds, 135, 100)
    infra.addLinks(cabinets, isps, 25, 500)
    infra.addLinks(cabinets, cabinets, 20, 1000)
    infra.addLinks(cabinets, accesspoints, 13, 50)
    infra.addLinks(cabinets, smartphones, 15, 35)

    infra.addLinks(accesspoints, clouds, 148, 3)
    infra.addLinks(accesspoints, isps, 38, 4)
    infra.addLinks(accesspoints, cabinets, 13, 4)
    infra.addLinks(accesspoints, accesspoints, 10, 50)
    infra.addLinks(accesspoints, smartphones, 2, 70)

    infra.addLinks(smartphones, clouds, 150, 2)
    infra.addLinks(smartphones, isps, 40, 2.5)
    infra.addLinks(smartphones, cabinets, 15, 3)
    infra.addLinks(smartphones, accesspoints, 2, 70)
    infra.addLinks(smartphones, smartphones, 15, 50)
   
    """
    infra.addNode("cloud", ["ubuntu", "mySQL", "gcc", "make"], "inf", [])
    infra.addNode("ispdatacentre", ["ubuntu", "mySQL"], 50, [])
    infra.addNode("cabinetserver", ["ubuntu", "mySQL"], 20, [])
    infra.addNode("accesspoint", ["ubuntu", "gcc", "make"], 4, ["vrViewer"])
    infra.addNode("smartphone", ["android", "gcc", "make"], 8, ["vrViewer"])

    infra.addLink("cloud", "ispdatacentre", 110, 1000)
    infra.addLink("cloud", "cabinetserver", 135, 100)
    infra.addLink("cloud", "accesspoint", 148, 20)
    infra.addLink("cloud", "smartphone", 150, 18 )
    infra.addLink("ispdatacentre", "cloud", 110, 1000)
    infra.addLink("ispdatacentre", "cabinetserver", 25, 500)
    infra.addLink("ispdatacentre", "accesspoint", 38, 50)
    infra.addLink("ispdatacentre", "smartphone", 40, 35)
    infra.addLink("cabinetserver", "cloud", 135, 100)
    infra.addLink("cabinetserver", "ispdatacentre", 25, 500)
    infra.addLink("cabinetserver", "accesspoint", 13, 50)
    infra.addLink("cabinetserver", "smartphone", 15, 35)
    infra.addLink("accesspoint", "cloud", 148, 3)
    infra.addLink("accesspoint", "ispdatacentre", 38, 4)
    infra.addLink("accesspoint", "cabinetserver", 13, 4)
    infra.addLink("accesspoint", "smartphone", 2, 70)
    infra.addLink("smartphone", "cloud", 150, 2)
    infra.addLink("smartphone", "ispdatacentre", 40, 2.5)
    infra.addLink("smartphone", "cabinetserver", 15, 3)
    infra.addLink("smartphone", "accesspoint", 2, 70)
    """

    infra.upload()
