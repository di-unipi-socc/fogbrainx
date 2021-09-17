import pyswip as p
import builder
import os
import sys
import shutil

def create_folder(name):
    try:
        os.mkdir(name)
    except:
        pass

def delete_folder(name):
    shutil.rmtree(name, ignore_errors=True)

def setup_raw(nodes, epochs, l):
    nodes = nodes*5
    create_folder("./raw/")
    create_folder("./raw/"+str(epochs))
    create_folder("./raw/"+str(epochs)+"/"+str(l))

    delete_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes))
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes))

    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/cr/")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/nocr/")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/speedup/")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/cr/time")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/cr/infs")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/cr/migrations")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/cr/migrationsperc")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/nocr/time")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/nocr/infs")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/nocr/migrations")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/nocr/migrationsperc")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/speedup/time")
    create_folder("./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)+"/speedup/infs")

def main(placer, nodes, epochs, l, commitchange, path):
    file = './deployment.pl'
    with open(file, 'w') as f:
        f.write(str(""))

    file = './edeployment.pl'
    with open(file, 'w') as f:
        f.write(str(""))

    setup_raw(nodes, epochs, l)

    commits = get_commits(path)
    print("Starting with", str(5*nodes), "nodes (", str(epochs),"epochs )")
    simulation(placer, nodes, commits, epochs, l, commitchange, path)
    
     
def get_commits(path):
    commits = os.listdir(path)
    commits.sort()
    return commits

def my_query(s, prolog):
    q = prolog.query(s)
    result = next(q) 
    return result

def append_raw_data(folder, file, data):
    with open(folder+file, "a+") as f:
        f.write(str(data)+"\n")

def add_raw_data(nodes, epochs, l, current_commit, time_cr, time_nocr, inf_cr, inf_nocr, speedup_time, speedup_inf, migrations_cr, migrations_nocr, migrations_perc_cr, migrations_perc_nocr):

    nodes = nodes*5

    base = "./raw/"+str(epochs)+"/"+str(l)+"/"+str(nodes)

    current_folder_time_cr = base+"/cr/time/"
    current_folder_infs_cr = base+"/cr/infs/"
    current_folder_migrations_cr = base+"/cr/migrations/"
    current_folder_migrations_perc_cr = base+"/cr/migrationsperc/"

    current_folder_time_nocr = base+"/nocr/time/"
    current_folder_infs_nocr = base+"/nocr/infs/"
    current_folder_migrations_nocr = base+"/nocr/migrations/"
    current_folder_migrations_perc_nocr = base+"/nocr/migrationsperc/"
    
    current_folder_time_speedup = base+"/speedup/time/"
    current_folder_infs_speedup = base+"/speedup/infs/"

    current_file = str(current_commit)+".txt"

    append_raw_data(current_folder_time_cr,current_file,time_cr)
    append_raw_data(current_folder_infs_cr,current_file,inf_cr)
    append_raw_data(current_folder_migrations_cr,current_file,migrations_cr)
    append_raw_data(current_folder_migrations_perc_cr,current_file,migrations_perc_cr)

    append_raw_data(current_folder_time_nocr,current_file,time_nocr)
    append_raw_data(current_folder_infs_nocr,current_file,inf_nocr)
    append_raw_data(current_folder_migrations_nocr,current_file,migrations_nocr)
    append_raw_data(current_folder_migrations_perc_nocr,current_file,migrations_perc_nocr)

    append_raw_data(current_folder_time_speedup,current_file,speedup_time)
    append_raw_data(current_folder_infs_speedup,current_file,speedup_inf)


def simulation(placer, nodes, commits, epochs, l, commitchange, path):
    prolog = p.Prolog()
    prolog.consult('fbX2.pl')  
    prolog.consult(placer)
    my_query('set_seed.', prolog)

    app_spec = ""
    current_commit = 0

    builder.builder(nodes)
    my_query('load_infra.', prolog)

    i = 0
    while i < epochs:
        if(i%30 == 0):
            print(i)
        app_spec = commits[current_commit]

        try:

            query_no_cr = "p('" + path + app_spec + "', P, Infs, Time, OldL, NewL, M, NM)"
            no_cr = my_query(query_no_cr, prolog)

            query_cr = "cr('" + path + app_spec + "', "+str(l)+", P, Infs, Time, OldL, NewL, M, NM)"
            cr = my_query(query_cr, prolog)

            migrations_perc_cr = cr["M"]/cr["NewL"]
            migrations_perc_nocr = no_cr["M"]/no_cr["NewL"]

            inferences_cr = cr["Infs"]
            inferences_nocr = no_cr["Infs"]
            time_cr = cr["Time"]
            if (time_cr == 0):
                time_cr = sys.float_info.min
            time_nocr = no_cr["Time"]

            speedup_infs = inferences_nocr/inferences_cr
            speedup_time = time_nocr/time_cr
        
            add_raw_data(nodes, epochs, l, current_commit, time_cr, time_nocr, inferences_cr, inferences_nocr, speedup_time, speedup_infs, cr["M"], no_cr["M"], migrations_perc_cr, migrations_perc_nocr)

            i = i + 1
            if i % commitchange == 0:   
                current_commit = (current_commit + 1) % len(commits)
            
        except StopIteration:
            print("fault")
            #builder.builder(nodes)
            my_query('changeInfra('+str(l)+').', prolog)
            print('loaded')

if __name__ == "__main__":
    main(sys.argv[1], int(sys.argv[2]), int(sys.argv[4]), float(sys.argv[5]), commitchange=int(sys.argv[6]), path="./experiments/commits/")
