import pyswip as p
import os
import sys
import shutil

BASEFOLDER = "./raw/"

def create_folder(name):
    try:
        os.mkdir(name)
    except:
        pass

def delete_folder(name):
    shutil.rmtree(name, ignore_errors=True)

def setup_raw(base):
    folder = str(base)
    folders = folder.split("/")
    base = BASEFOLDER
    create_folder(base)
    create_folder(folder)
    for f in folders:
        base += f+"/"
        create_folder(base)
    
    create_folder(base+"/cr/")
    create_folder(base+"/nocr/")
    create_folder(base+"/speedup/")
    create_folder(base+"/cr/time")
    create_folder(base+"/cr/infs")
    create_folder(base+"/cr/migrations")
    create_folder(base+"/cr/migrationsperc")
    create_folder(base+"/nocr/time")
    create_folder(base+"/nocr/infs")
    create_folder(base+"/nocr/migrations")
    create_folder(base+"/nocr/migrationsperc")
    create_folder(base+"/speedup/time")
    create_folder(base+"/speedup/infs")
    return base

def main(fogbrain, placer, epochs, path, base):
    file = './deployment.pl'
    with open(file, 'w') as f:
        f.write(str(""))

    file = './edeployment.pl'
    with open(file, 'w') as f:
        f.write(str(""))

    commits = get_commits(path)
    commitchange = int(epochs/len(commits))

    new_base = setup_raw(base)

    print("Starting ("+str(epochs)+" epochs)")
    faults = simulation(fogbrain, placer, commits, epochs, commitchange, path, base)
    delete_folder(new_base+"/faults")
    create_folder(new_base+"/faults")
    create_folder(new_base+"/faults/faults")
    append_raw_data(new_base+"/faults/faults/", "faults.txt", faults/epochs)
    return faults

def get_commits(path):
    commits = os.listdir(path)
    commits.sort()
    return commits

def get_infras(path):
    infras = os.listdir(path)
    infras.sort()
    return infras

def my_query(s, prolog):
    q = prolog.query(s)
    result = next(q) 
    return result

def append_raw_data(folder, file, data):
    with open(folder+file, "a+") as f:
        f.write(str(data)+"\n")

def add_raw_data(base, current_commit, time_cr, time_nocr, inf_cr, inf_nocr, speedup_time, speedup_inf, migrations_cr, migrations_nocr, migrations_perc_cr, migrations_perc_nocr):
    base = BASEFOLDER+base
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

def simulation(fogbrain, placer, commits, epochs, commitchange, path, base):
    prolog = p.Prolog()
    prolog.consult(fogbrain)  
    prolog.consult(placer)
    prolog.consult("sim.pl")

    app_spec = ""
    current_commit = 0

    infras = os.listdir("./infra/")

    if len(infras) < epochs:
        print("Error: Not Enough Infras")
        exit(0)

    old_infra = None

    faults = 0

    i = 0
    while i < epochs:
        if(i%10 == 0):
            print(i)

        app_spec = commits[current_commit]
        infra = "infra"+str(i)+".pl"

        i = i + 1
        if i % commitchange == 0:   
            current_commit = (current_commit + 1) % len(commits)

        if old_infra is not None:
            my_query("unload_infra.", prolog)
        
        old_infra = infra

        my_query("load_infra('./infra/"+infra+"').", prolog)

        try: #TRY
            query_no_cr = "p('" + path + app_spec + "', P, Infs, Time, OldL, NewL, M, NM)."
            no_cr = my_query(query_no_cr, prolog)

            query_cr = "cr('" + path + app_spec + "', P, Infs, Time, OldL, NewL, M, NM)."
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
        
            add_raw_data(base, current_commit, time_cr, time_nocr, inferences_cr, inferences_nocr, speedup_time, speedup_infs, cr["M"], no_cr["M"], migrations_perc_cr, migrations_perc_nocr)

        except StopIteration:
            faults += 1
            print("fault ("+str(i-1)+")")
    
    print("Faults: "+str((faults/epochs)*100)+"%")
    return faults
           

if __name__ == "__main__":
    main('../fbX2Official', "../placers/placer", "./commits/", base=BASEFOLDER)