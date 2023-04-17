:-consult('placer.pl').
:-consult('gfogbrain.pl').
:-dynamic deployment/3.
:-dynamic application/2.
:-dynamic service/4.
:-dynamic s2s/4.
:-dynamic link/4.
:-dynamic node/4.

:-set_prolog_flag(stack_limit, 16 000 000 000).
:-set_prolog_flag(last_call_optimisation, true).

targetCarbon(1).
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

reasoningStep([on(S,_)|Ps],(AllocHW,AllocBW),KOs,POk,StableP) :-
    \+ service(S,_,_,_), 
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,POk,StableP).
reasoningStep([on(S,N)|Ps],(AllocHW,AllocBW),KOs,POk,StableP) :-
    nodeOk(S,N,POk,AllocHW), linksOk(S,N,POk,AllocBW), footprintOk([on(S,N)|POk],_), !,
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,[on(S,N)|POk],StableP).
reasoningStep([on(S,_)|Ps],(AllocHW,AllocBW),[S|KOs],POk,StableP) :-
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,POk,StableP).
reasoningStep([],_,[],P,P).

newServices(P, NewServices) :- findall(S, (service(S,_,_,_), \+ member(on(S,_),P)), NewServices).
