:- use_module(library(lists)).
:- ['placer.pl'].

hwTh(0.2). bwTh(0.2).

averageGCI(0.475). % 0.475 kgCO2/kWh, https://www.iea.org/reports/global-energy-co2-status-report-2019/emissions
kWhPerMB(0.00008). % https://docs.microsoft.com/it-it/learn/modules/sustainable-software-engineering-overview/8-network-efficiency 

% Consumption and CO2 model from: https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6128960

query(placement(App,Placement,Energy,Carbon,Cost)).

placement(A,P,E,C,Cost) :-
   gFogBrain(A,P,E,C), hourlyCost(P,Cost).

hourlyCost([on(S,N)|P],NewCost) :- 
    hourlyCost(P,OldCost),
    service(S,_,HW,_), cost(N,C),
    NewCost is OldCost + C * HW.
hourlyCost([],0).

gFogBrain(A,P,Energy,CarbonEmissions) :- 
    application(A,Services), placement(Services,[],([],[]),P),
    allocatedResources(P,Alloc), footprint(P,Alloc,Energy,CarbonEmissions).

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
    findall((N,FreeHW), (member(on(_,N),P), node(N,_,_,FreeHW,_)), Nodes).

% application(AppId, [ServiceIds]).
application(lightsApp, [mlOptimiser, lightsDriver]).
% service(ServiceId, [SoftwareRequirements], HardwareRequirements, IoTRequirements).
service(mlOptimiser, [mySQL, python, ubuntu], 16, [gpu]).
service(lightsDriver, [ubuntu], 2, [videocamera, lightshub]).
% s2s(ServiceId1, ServiceId2, MaxLatency, MinBandwidth)
s2s(mlOptimiser, lightsDriver, 50, 0.5).
s2s(lightsDriver, mlOptimiser, 20, 16).

% node(NodeId, SoftwareCapabilities, FreeHW, TotHW, IoTCapabilities).
% totHW/2 is the total hardware, needed to compute load and energy consumption
% energyProfile/4 specifies the power source of the node and the possibly non-linear 
%                 function to compute consumption E based on load L
% pue/2 is the pue associated to a node, server, datacentre, etc.
.9999::node(privateCloud,cloudOp,[ubuntu, mySQL, python], 128, [gpu]).
    0.99::access_control(privateCloud).
    cost(privateCloud,0.0016).
    totHW(privateCloud,150).
    energyProfile(privateCloud,L,E) :- E is 0.1 + 0.01*log(L).
    pue(privateCloud,1.9).
    energySourceMix(privateCloud,[(0.3,solar), (0.7,coal)]).
.99::node(accesspoint,ispOp,[ubuntu, mySQL, python], 4, [lightshub, videocamera]).
    0.9::anti_tampering(accesspoint).
    cost(accesspoint,0.003).
    totHW(accesspoint,6).
    energyProfile(accesspoint,L,E) :- E is 0.05 + 0.03*log(L).
    pue(accesspoint,1.5).
    energySourceMix(accesspoint,[(0.8,coal),(0.2,onshorewind)]).
.9::node(edgenode,edgeOp,[ubuntu, python], 8, [gpu, lightshub, videocamera]).
    0.9::anti_tampering(edgenode).
    cost(edgenode,0.005).
    totHW(edgenode,12).
    energyProfile(edgenode,L,E) :- L =< 50, E is 0.08.
    energyProfile(edgenode,L,E) :- L>50, E is 0.1.
    pue(edgenode,1.2).
    energySourceMix(edgenode,[(0.5,coal), (0.5,solar)]).

appOp(appOp).

%%% trust relations declared by appOp
.9::trusts(appOp, edgeOp).  
.9::trusts(appOp, ispOp).

%%% trust relations declared by edgeOp
.7::trusts(edgeOp, cloudOp1).
.8::trusts(edgeOp, cloudOp2).

%%% trust relation declared by cloudOp1
.8::trusts(cloudOp1, cloudOp2).

%%% trust relation declared by cloudOp2
.2::trusts(cloudOp2, cloudOp).

%%% trust relations declared by ispOp  
.8::trusts(ispOp, cloudOp).
.6::trusts(ispOp, edgeOp).


% emissions in CO2 kg/kWh
emissions(gas, 0.610).
emissions(coal, 1.1).
emissions(onshorewind, 0.0097).
emissions(offshorewind, 0.0165).
emissions(solar, 0.05). % https://solarbay.com.au/portfolio-item/how-much-emissions-does-solar-power-prevent/


% link(NodeId1, NodeId2, FeaturedLatency, FeaturedBandwidth).
.99::link(privateCloud, accesspoint, 5, 1000).
.99::link(accesspoint, privateCloud, 5, 1000).
.95::link(accesspoint, edgenode, 5, 20).
.95::link(edgenode, accesspoint, 5, 20).
.98::link(privateCloud, edgenode, 15, 18).
.98::link(edgenode, privateCloud, 15, 18).


subset([],_).
subset([X|Xs],Y) :- member(X,Y), subset(Xs,Y).

dif(A,B) :- A\=B.