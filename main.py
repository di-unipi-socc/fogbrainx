from swiplserver import PrologMQI 
import argparse
import time


def getParameters():
    prsr = argparse.ArgumentParser(description='FogBrainX - CLI v.0.1')
    prsr.add_argument('-app', type=str, help='Path to the application specification file')
    prsr.add_argument('-infra', type=str, help='Path to the infrastructure specification file')
    prsdArgs = prsr.parse_args()
    return prsdArgs.app, prsdArgs.infra


def printDict(dict):
    for n in dict:
        print(f"\t{n} -> {dict[n][0]}")


def insert(ms, n, dict):
    if n not in dict:
        dict[n] = [ms]
    else:
        dict[n].append(ms)
    return dict


def unpackP(place):
    dictP = {}
    for p in place:
        n, ms = p['args'][0], p['args'][1]
        insert(ms, n, dictP)
    return dictP


with PrologMQI() as mqi:
    app, infra = getParameters()
    print(f"\nLoading file for {app}, {infra}, ...")
    with mqi.create_thread() as prolog_thread:

        prolog_thread.query(f"consult('fogbrainx.pl').")
        print(f"Loaded fogbrainx...")

        prolog_thread.query(f"consult('{app}').")
        prolog_thread.query(f"consult('{infra}').")
        print(f"Consulted application and infrastructure data.")

        start = time.time()
        prolog_thread.query_async(f"placement(A,P).",find_all=False)
        stop = time.time()
        result = prolog_thread.query_async_result()[0] # return first placement found
        
        if isinstance(result, bool):
            print("\nNo placement found")
        else:
            print(f"\nFound placement for '{result['A']}' application:")
            placement = unpackP(result['P'])
            printDict(placement)

        print(f"\nElapsed time: {stop-start:.6f} s")
        
