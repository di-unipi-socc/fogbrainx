placement(A,P) :- 
    application(A,Services), placement(Services,[],([],[]),P),
    allocatedResources(P,Alloc). 

placement([S|Ss],P,(AllocHW,AllocBW),Placement) :-
    nodeOk(S,N,P,AllocHW), linksOk(S,N,P,AllocBW), 
    placement(Ss,[on(S,N)|P],(AllocHW,AllocBW),Placement).
placement([],P,_,P).

nodeOk(S,N,P,AllocHW) :-
    node(N,NOp,SWCaps,HWCaps,IoTCaps),
    service(S,SWReqs,HWReqs,IoTReqs),
    %appOp(A), trusts2(A,NOp), secure(S,N),
    swReqsOk(SWReqs,SWCaps),
    thingReqsOk(IoTReqs,IoTCaps),
    hwOk(N,HWCaps,HWReqs,P,AllocHW).

secure(S,N) :-
    access_control(N); anti_tampering(N).

trusts(X,X).

trusts2(A,B) :-
    trusts(A,B).
trusts2(A,B) :-
    trusts(A,C),
    trusts2(C,B).

swReqsOk(SWReqs, SWCaps) :- subset(SWReqs, SWCaps).

thingReqsOk(TReqs, TCaps) :- subset(TReqs, TCaps).

hwOk(N,HWCaps,HWReqs,P,AllocHW) :-
    findall(HW,member((N,HW),AllocHW),HWs), sum_list(HWs, CurrAllocHW),
    findall(HW, (member(on(S1,N),P), service(S1,_,HW,_)), OkHWs), sum_list(OkHWs, NewAllocHW),  
    hwTh(T), HWCaps >= HWReqs + T - CurrAllocHW + NewAllocHW.

linksOk(S,N,P,AllocBW) :-
    findall((N1N2,ReqLat), relevant(S,N,P,N1N2,ReqLat), N2Ns), latencyOk(N2Ns),
    findall(N1N2, member((N1N2,ReqLat),N2Ns), N1N2s), bwOk(N1N2s, AllocBW, [on(S,N)|P]). 

latencyOk([((N1,N2),ReqLat)|N2Ns]) :- 
    link(N1,N2,FeatLat,_), FeatLat =< ReqLat, latencyOk(N2Ns).
latencyOk([]).

bwOk([(N1,N2)|N2Ns],AllocBW,P) :-
    link(N1,N2,_,FeatBW),
    findall(BW, member((N1,N2,BW),AllocBW), BWs), sum_list(BWs, CurrAllocBW), 
    findall(BW, s2sOnN1N2((N1,N2), P, BW), OkBWs), sum_list(OkBWs, OkAllocBw), 
    bwTh(T), FeatBW  >=  OkAllocBw - CurrAllocBW + T, 
    bwOk(N2Ns,AllocBW,P).
bwOk([],_,_).

relevant(S,N,P,(N,N2),L) :- s2s(S,S2,L,_), member(on(S2,N2),P), dif(N,N2).
relevant(S,N,P,(N1,N),L) :- s2s(S1,S,L,_), member(on(S1,N1),P), dif(N1,N).

s2sOnN1N2((N1,N2),P,B) :- s2s(S3,S4,_,B), member(on(S3,N1),P), member(on(S4,N2),P).

allocatedResources(P,(AllocHW,AllocBW)) :- 
    findall((N,HW), (member(on(S,N),P), service(S,_,HW,_)), AllocHW),
    findall((N1,N2,BW), n2n(P,N1,N2,BW), AllocBW).
n2n(P,N1,N2,ReqBW) :- s2s(S1,S2,_,ReqBW), member(on(S1,N1),P), member(on(S2,N2),P), dif(N1,N2).