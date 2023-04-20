:-['data.pl', 'placer.pl', 'gfogbrain.pl'].

:- dynamic deployment/4.

:-set_prolog_flag(stack_limit, 32 000 000 000).
:-set_prolog_flag(last_call_optimisation, true).

targetCarbon(0.3).
targetEnergy(1).

fogBrainX(A,Placement) :- 
    \+ deployment(A,_,_,_), placement(A,Placement,Rates),
    allocatedResources(Placement,Rates,Alloc), 
    assert(deployment(A,Placement,Rates,Alloc)).
fogBrainX(A,NewPlacement) :-
    deployment(A,P,_,Alloc),
    newServices(P,NewServices),
    reasoningStep(P,Alloc,NotOkServices,[],OkRates,[],OkPlacement), 
    append(NewServices,NotOkServices,ServicesToPlace), 
    placement(ServicesToPlace,OkPlacement,Alloc,OkRates,NewRates,NewPlacement),
    allocatedResources(NewPlacement,NewRates,NewAlloc),
    retract(deployment(A,_,_,_)), assert(deployment(A,NewPlacement,NewRates,NewAlloc)).
fogBrainX(A, NewPlacement) :-
    deployment(A,_,_,Alloc), application(A, Services),
    placement(Services,[],Alloc,[],NewRates,NewPlacement),
    allocatedResources(NewPlacement,NewRates,NewAlloc),
    retract(deployment(A,_,_,_)), assert(deployment(A,NewPlacement,NewRates,NewAlloc)).

reasoningStep([on((S,V),_)|Ps],(AllocHW,AllocBW),KOs,ROk,StableR,POk,StableP) :-
    \+ service(S,V,_,_,_), 
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,ROk,StableR,POk,StableP).
reasoningStep([on((S,_),N)|Ps],(AllocHW,AllocBW),KOs,ROk,StableR,POk,StableP) :-
    nodeOk(S,N,V,POk,AllocHW), linksOk(S,N,[on((S,V),N)|POk],AllocBW,ROk,TmpR), footprintOk([on((S,V),N)|POk],ROk,_), !,
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,TmpR,StableR,[on((S,V),N)|POk],StableP).
reasoningStep([on((S,_),_)|Ps],(AllocHW,AllocBW),[S|KOs],ROk,StableR,POk,StableP) :-
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,ROk,StableR,POk,StableP).
reasoningStep([],_,[],R,R,P,P).

newServices(P, NewServices) :- findall(S, distinct((service(S,_,_,_,_), \+ member(on((S,_),_),P))), NewServices).
