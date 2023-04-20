:-['data.pl', 'placer.pl', 'gfogbrain.pl'].

:- dynamic deployment/4.

:-set_prolog_flag(stack_limit, 32 000 000 000).
:-set_prolog_flag(last_call_optimisation, true).

targetCarbon(0.15).
targetEnergy(1).

fogBrainX(A,Placement,Rates) :- 
    \+ deployment(A,_,_,_), placement(A,Placement,Rates),
    allocatedResources(Placement,Rates,Alloc), 
    assert(deployment(A,Placement,Rates,Alloc)).
fogBrainX(A,NewPlacement,NewRates) :-
    deployment(A,P,_,Alloc),
    newServices(P,NewServices),
    reasoningStep(P,Alloc,NotOkServices,[],Rates,[],OkPlacement), 
    append(NewServices,NotOkServices,ServicesToPlace), 
    placement(ServicesToPlace,OkPlacement,Alloc,Rates,NewRates,NewPlacement),
    allocatedResources(NewPlacement,NewRates,NewAlloc),
    retract(deployment(A,_,_,_)), assert(deployment(A,NewPlacement,NewRates,NewAlloc)).
fogBrainX(A, NewPlacement,NewRates) :-
    deployment(A,_,_,Alloc), application(A, Services),
    placement(Services,[],Alloc,[],NewRates,NewPlacement),
    allocatedResources(NewPlacement,NewRates,NewAlloc),
    retract(deployment(A,_,_,_)), assert(deployment(A,NewPlacement,NewRates,NewAlloc)).

reasoningStep([on((S,V),_)|Ps],(AllocHW,AllocBW),KOs,ROk,StableR,POk,StableP) :-
    \+ service(S,V,_,_,_), 
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,ROk,StableR,POk,StableP).
reasoningStep([on((S,_),N)|Ps],(AllocHW,AllocBW),KOs,ROk,StableR,POk,StableP) :-
    rankNodes(RankedNodes), member((_,N),RankedNodes),
    nodeOk(S,N,V,POk,AllocHW), linksOk(S,N,[on((S,V),N)|POk],AllocBW,ROk,TmpR), footprintOk([on((S,V),N)|POk],TmpR,_), !,
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,TmpR,StableR,[on((S,V),N)|POk],StableP).
reasoningStep([on((S,_),_)|Ps],(AllocHW,AllocBW),[S|KOs],ROk,StableR,POk,StableP) :-
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,ROk,StableR,POk,StableP).
reasoningStep([],_,[],R,R,P,P).

newServices(P, NewServices) :- findall(S, distinct((service(S,_,_,_,_), \+ member(on((S,_),_),P))), NewServices).
