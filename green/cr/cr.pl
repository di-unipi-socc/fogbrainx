:-['data.pl', 'placer.pl', 'gfogbrain.pl'].

:- dynamic deployment/3.

:-set_prolog_flag(stack_limit, 32 000 000 000).
:-set_prolog_flag(last_call_optimisation, true).

targetCarbon(0.3).
targetEnergy(1).

fogBrainX(A,Placement) :- 
    \+ deployment(A,_,_), placement(A,Placement),
    allocatedResources(Placement,Alloc), 
    assert(deployment(A,Placement,Alloc)).
fogBrainX(A,NewPlacement) :-
    deployment(A,P,Alloc),
    newServices(P,NewServices),
    reasoningStep(P,Alloc,NotOkServices,[],OkPlacement), 
    append(NewServices,NotOkServices,ServicesToPlace), 
    placement(ServicesToPlace,OkPlacement,Alloc,NewPlacement),
    allocatedResources(NewPlacement,NewAlloc),
    retract(deployment(A,_,_)), assert(deployment(A,NewPlacement,NewAlloc)).
fogBrainX(A, NewPlacement) :-
    deployment(A,_,Alloc), application(A, Services),
    placement(Services,[],Alloc,NewPlacement),
    allocatedResources(NewPlacement,NewAlloc),
    retract(deployment(A,_,_)), assert(deployment(A,NewPlacement,NewAlloc)).

reasoningStep([on(S,_,V)|Ps],(AllocHW,AllocBW),KOs,POk,StableP) :-
    \+ service(S,V,_,_,_), 
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,POk,StableP).
reasoningStep([on(S,N,_)|Ps],(AllocHW,AllocBW),KOs,POk,StableP) :-
    nodeOk(S,N,V,POk,AllocHW), linksOk(S,N,V,POk,AllocBW), footprintOk([on(S,N,V)|POk],_), !,
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,[on(S,N,V)|POk],StableP).
reasoningStep([on(S,_,_)|Ps],(AllocHW,AllocBW),[S|KOs],POk,StableP) :-
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,POk,StableP).
reasoningStep([],_,[],P,P).

newServices(P, NewServices) :- findall(S, distinct((service(S,_,_,_,_), \+ member(on(S,_,_),P))), NewServices).
