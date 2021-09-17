from app import App

import os

def add_commit(commit, name, DIR="./commits/", BASE_NAME="commit",SEPARATOR="-"):
    add_commit.index = getattr(add_commit, 'index', -1) + 1
    
    file_name = DIR+BASE_NAME+str(add_commit.index)+SEPARATOR+name+".pl"
    
    commit.upload(file_name)
    return file_name
    
def initial_commit():
    app = App("vrApp","app.pl")
    app.addService("videoStorage", ["mySQL", "ubuntu"], 16, [])
    app.addService("sceneSelector", ["ubuntu"], 2, [])
    app.addService("vrDriver", ["gcc", "make"], 2, ["vrViewer"])

    app.addS2S("videoStorage", "sceneSelector", 150, 16)
    app.addS2S("sceneSelector", "videoStorage", 150, 0.5)
    app.addS2S("sceneSelector", "vrDriver", 20, 8)
    app.addS2S("vrDriver", "sceneSelector", 20, 1)
    
    return app

def generate_commits(PATH):
    try:
        os.mkdir(PATH)
    except OSError:
        pass
    
    app = initial_commit()
    add_commit(app, "initial", DIR=PATH)
    
    app.addService("tokensDealer", ["ubuntu", "mySQL"], 20, [])
    app.addService("userProfiler", ["gcc","make"], 2, [])
    app.addS2S("userProfiler", "sceneSelector", 50, 2)
    app.addS2S("sceneSelector", "userProfiler", 50, 2)
    app.addS2S("userProfiler", "tokensDealer", 200, 0.5)
    app.addS2S("tokensDealer", "userProfiler", 200, 1)
    add_commit(app, "added2Services", DIR=PATH)
    
    app.removeService("tokensDealer")
    add_commit(app, "removedService", DIR=PATH)
    
    app.modifyService("videoStorage", ["ubuntu","mySQL"], 30, [])
    app.modifyService("userProfiler", ["gcc","make"], 2, ["vrViewer"])
    add_commit(app, "changed2Services", DIR=PATH)
    
    app.addS2S("userProfiler", "videoStorage", 500, 1)
    app.addS2S("videoStorage", "userProfiler", 500, 1)
    add_commit(app, "added2S2S", DIR=PATH)
    
    app.removeS2S("sceneSelector","userProfiler")
    add_commit(app, "removedS2S", DIR=PATH)
    
    app.modifyS2S("videoStorage", "userProfiler", 200, 1)
    app.modifyS2S("userProfiler", "videoStorage", 200, 2)
    add_commit(app, "changed2S2S", DIR=PATH)
    
    