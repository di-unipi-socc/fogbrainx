averageGCI(0.475). % 0.475 kgCO2/kWh, https://www.iea.org/reports/global-energy-co2-status-report-2019/emissions
kWhPerMB(0.00008). % https://docs.microsoft.com/it-it/learn/modules/sustainable-software-engineering-overview/8-network-efficiency 

% Consumption and CO2 model from: https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6128960

hourlyCost([on((S,V),N)|P],NewCost) :- 
    hourlyCost(P,OldCost),
    service(S,V,_,HW,_), cost(N,C),
    NewCost is OldCost + C * HW.
hourlyCost([],0).

footprint(P,(AllocHW,AllocBW),Energy,Carbon) :-
    deploymentNodes(P,Nodes), 
    hardwareFootprint(Nodes,AllocHW,HWEnergy,HWCarbon),
    networkFootprint(AllocBW,BWEnergy,BWCarbon),
    Energy is HWEnergy + BWEnergy, Carbon is HWCarbon + BWCarbon.

hardwareFootprint([(N,HW)|Ns],AllocHW,Energy,Carbon) :-
    hardwareFootprint(Ns,AllocHW,EnergyNs,CarbonNs),
    hardwareEnergy(N,HW,AllocHW,EnergyN), 
    energySourceMix(N,Sources), hardwareEmissions(Sources,EnergyN,CarbonN),
    Energy is EnergyN+EnergyNs, Carbon is CarbonN+CarbonNs.
hardwareFootprint([],_,0,0).

hardwareEnergy(N,HW,AllocHW,Energy):-
    totHW(N,TotHW), pue(N,PUE), 
    OldL is 100 * (TotHW - HW) / TotHW, energyProfile(N,OldL,OldE),
    findall(H,member((N,H),AllocHW),HWs), sum_list(HWs,PHW), 
    NewL is 100 * (TotHW - HW + PHW) / TotHW, energyProfile(N,NewL,NewE),
    Energy is (NewE - OldE) * PUE. 

hardwareEmissions([(P,S)|Srcs],Energy,Carbon) :-
    hardwareEmissions(Srcs,Energy,CarbSrcs),
    emissions(S,MU), CarbS is P * MU * Energy, Carbon is CarbS + CarbSrcs.
hardwareEmissions([],_,0).

networkFootprint(AllocBW,BWEnergy,BWCarbon) :-
    findall(BW, member((_,_,BW),AllocBW), Flows), sum_list(Flows,TotBW),
    kWhPerMB(K), BWEnergy is 450 * K * TotBW, 
    averageGCI(A), BWCarbon is A * BWEnergy.

deploymentNodes(P,Nodes) :-     
    findall((N,FreeHW), distinct( (member(on(_,N),P), node(N,_,FreeHW,_)) ), Nodes).

rankNodes(RankedNodes):- findall(N, node(N,_,_,_), Ns), nodeEmissions(Ns, NFs), sort(NFs, RankedNodes).

nodeEmissions([N|Ns], [(F,N)|NFs]) :- 
    energySourceMix(N,Sources),
    findall(E, (member((P,S),Sources), emissions(S, MU), E = P*MU), Es), sum_list(Es, F),
    nodeEmissions(Ns,NFs).
nodeEmissions([],[]).