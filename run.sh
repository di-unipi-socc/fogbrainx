#!/bin/bash

runs=1
nodes=("8" "16" "32" "64" "128" "256")
l_ls=("0.1" "0.2" "0.3" "0.4" "0.5")

commitchange=150 # indicates how many epochs before changing commit 20

epochs=$((commitchange*7))

placer=placers/placer

folder=${placer//'/'/'_'}

if [[ $1 = "garr" ]]; then
    rm -r ./results/GARR/$folder/$runs-$epochs -f
else
    rm -r ./results/$folder/$runs-$epochs -f
fi

for n in ${nodes[*]}; do 
    echo ""
    echo "******** Starting experiment" $n" nodes"
    echo ""
    for l in ${l_ls[*]}; do
        echo ""
        echo "******** Starting experiment" $l"%"
        echo ""
        if [[ $1 = "darwin" ]]; then
            python3.9 experiments/experiments.py $placer $n $runs $epochs $l $commitchange
        elif [[ $1 = "wsl" ]]; then
            python3.9 experiments/experiments.py $placer $n $runs $epochs $l $commitchange
        elif [[ $1 = "garr" ]]; then
            python3 experiments/experiments.py $placer $n $runs $epochs $l $commitchange
        elif [[ $1 = "win" ]]; then
            python3 experiments/experiments.py $placer $n $runs $epochs $l $commitchange
        else
            python3.9 experiments/experiments.py $placer $n $runs $epochs $l $commitchange
        fi
        
    done
done

