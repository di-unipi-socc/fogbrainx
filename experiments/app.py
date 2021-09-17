class App:

    def __init__(self, app, file):
        self._app = app
        self._services = {}
        self._s2ss = {}
        self._file = file

    def addService(self, service, sw, hw, things):
        if service not in self._services:
            self._services[service] = (sw,hw,things)

    def addS2S(self, service1, service2, lat, bw):
        if service1 in self._services and service2 in self._services and (service1,service2) not in self._s2ss:
            self._s2ss[(service1,service2)] = (lat,bw)

    def modifyService(self, service, sw, hw, things):
        if service in self._services:
            self._services[service] = (sw,hw,things)

    def modifyS2S(self, service1, service2, lat, bw):
        if (service1,service2) in self._s2ss:
            self._s2ss[(service1,service2)] = (lat,bw)

    def removeService(self, service):
        if service in self._services:
            del self._services[service]
            for (s1,s2) in self._s2ss.copy():
                if s1 == service or s2 == service:
                    self.removeS2S(s1,s2)

    def removeS2S(self, service1, service2):
        if (service1,service2) in self._s2ss:
            del self._s2ss[(service1,service2)]

    def getService(self, service):
        if service in self._services:
            (sw, hw, things) = self._services[service]
            return ("service("+service+", "+str(sw)+", "+str(hw)+", "+str(things)+").").replace("'","")
        return ""

    def getS2S(self, service1, service2):
        if (service1,service2) in self._s2ss:
            (lat, bw) = self._s2ss[(service1,service2)]
            return ("s2s("+service1+", "+service2+", "+str(lat)+", "+str(bw)+").").replace("'","")
        return ""

    def __str__(self):
        services = []
        specs = ""
        for s in self._services:
            services.append(s)
            specs += self.getService(s)+"\n"
        specs+="\n"
        for (s1,s2) in self._s2ss:
            specs += self.getS2S(s1,s2)+"\n"
        specs+="\n"
        specs = ("application("+self._app+", "+str(services)+").\n\n").replace("'","")+specs
        return specs

    def upload(self, file=None):
        if file is None:
            file = self._file
        with open(file,"w") as f:
            f.write(str(self))

if __name__ == "__main__":

    app = App('vrApp',"app.pl")
    #app.addService("videoStorage", ["mySQL", "ubuntu"], 16, [])
    app.addService("sceneSelector", ["ubuntu"], 2, [])
    app.addService("vrDriver", ["gcc", "make"], 2, ["vrViewer"])

    #app.addS2S("videoStorage", "sceneSelector", 150, 101)
    #app.addS2S("sceneSelector", "videoStorage", 150, 0.5)
    app.addS2S("sceneSelector", "vrDriver", 20, 8)
    app.addS2S("vrDriver", "sceneSelector", 20, 1)

    app.upload()
