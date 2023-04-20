placement(A,P,R) :- 
    application(A,Services), placement(Services,[],([],[]),[],R,P). 

placement([S|Ss],P,(AllocHW,AllocBW),R, Rates, Placement) :-
    rankNodes(RankedNodes), member((_,N),RankedNodes),
    nodeOk(S,N,V,P,AllocHW), linksOk(S,N,[on((S,V),N)|P],AllocBW,R,TmpR), footprintOk([on((S,V),N)|P], TmpR,_),
    placement(Ss,[on((S,V),N)|P],(AllocHW,AllocBW),TmpR, Rates, Placement).
placement([],P,_,R,R,RP) :- reverse(P, RP). 

rankNodes(RankedNodes):- findall(N, node(N,_,_,_), Ns), nodesFootprint(Ns, NFs), sort(NFs, RankedNodes).

nodesFootprint([N|Ns], [(F,N)|NFs]) :- 
    energySourceMix(N,Sources),
    findall(E, (member((P,S),Sources), emissions(S, MU), E = P*MU), Es), sum_list(Es, F),
    nodesFootprint(Ns,NFs).
nodesFootprint([],[]).

nodeOk(S,N,V,P,AllocHW) :-
    service(S,V,SWReqs,HWReqs,IoTReqs),
    node(N,SWCaps,HWCaps,IoTCaps),
    swReqsOk(SWReqs,SWCaps),
    thingReqsOk(IoTReqs,IoTCaps),
    hwOk(N,HWCaps,HWReqs,P,AllocHW).

swReqsOk(SWReqs, SWCaps) :- subset(SWReqs, SWCaps).

thingReqsOk(TReqs, TCaps) :- subset(TReqs, TCaps).

hwOk(N,HWCaps,HWReqs,P,AllocHW) :-
    findall(HW,member((N,HW),AllocHW),HWs), sum_list(HWs, CurrAllocHW),
    findall(HW, (member(on((S1,V),N),P), service(S1,V,_,HW,_)), OkHWs), sum_list(OkHWs, NewAllocHW),  
    hwTh(T), HWCaps >= HWReqs + T - CurrAllocHW + NewAllocHW.

linksOk(S,N,P,AllocBW,Rates,NewRates) :-
    findall(norate(S1S2,N1N2), distinct(relevant(S,N,P,S1S2,N1N2)), NoRates),
    qosOk(NoRates, P, AllocBW, Rates, NewRates).

qosOk([norate((S1,S2),(N1,N2))|NoRates], P, AllocBW, Rates, NewRates) :-
    s2s(S1,S2,R,ReqLat,_), link(N1,N2,FeatLat,FeatBW), bwTh(T),
    findall(BW, member((N1,N2,BW),AllocBW), BWs), sum_list(BWs, CurrAllocBW), 
    findall(BW, s2sOnN1N2((N1,N2), P, [(S1,S2,R)|Rates], BW), OkBWs), sum_list(OkBWs, OkAllocBw), 
    FeatBW  >=  OkAllocBw - CurrAllocBW + T,
    FeatLat =< ReqLat,
    qosOk(NoRates,P,AllocBW,[(S1,S2,R)|Rates],NewRates).
qosOk([],_,_,R,R).

relevant(S,N,P,(S,S2),(N,N2)) :- s2s(S,S2,_,_,_), member(on((S2,_),N2),P), dif(N,N2).
relevant(S,N,P,(S1,S),(N1,N)) :- s2s(S1,S,_,_,_), member(on((S1,_),N1),P), dif(N1,N).

s2sOnN1N2((N1,N2),P,Rs,B) :- s2s(S3,S4,R,_,B), member((S3,S4,R),Rs), member(on(S3,N1),P), member(on(S4,N2),P).

allocatedResources(P,R,(AllocHW,AllocBW)) :- 
    findall((N,HW), (member(on((S,V),N),P), service(S,V,_,HW,_)), AllocHW),
    findall((N1,N2,BW), n2n(P,R,N1,N2,BW), AllocBW).
n2n(P,Rs,N1,N2,ReqBW) :- s2s(S1,S2,R,_,ReqBW), member((S1,S2,R),Rs), member(on((S1,_),N1),P), member(on((S2,_),N2),P), dif(N1,N2).

footprintOk(P,R,Alloc) :-
    allocatedResources(P,R,Alloc), footprint(P,Alloc,Energy,Carbon), 
    targetCarbon(CarbonMax), Carbon =< CarbonMax, targetEnergy(EnergyMax), Energy =< EnergyMax.