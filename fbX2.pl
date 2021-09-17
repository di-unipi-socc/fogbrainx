:-dynamic deployment/3.
:-dynamic application/2.
:-dynamic service/4.
:-dynamic s2s/4.
:-dynamic link/4.
:-dynamic node/4.

:-set_prolog_flag(stack_limit, 120 000 000 000).
:-set_prolog_flag(last_call_optimisation, true).

:-consult('utilsX.pl').

fogBrain(A,Placement) :- 
    \+ deployment(A,_,_), placement(A,Placement).
fogBrain(A,NewPlacement) :-
    deployment(A,P,Alloc),
    newServices(P,NewServices),
    reasoningStep(P,Alloc,NotOkServices,[],OkPlacement), 
    append(NewServices,NotOkServices,ServicesToPlace), 
    placement(ServicesToPlace,OkPlacement,Alloc,NewPlacement),
    allocatedResources(NewPlacement,NewAlloc), 
    retract(deployment(A,_,_)), assert(deployment(A,NewPlacement,NewAlloc)).
fogBrain(A, NewPlacement) :-
    deployment(A,_,Alloc),
    application(A, Services),
    placement(Services,[],Alloc,NewPlacement),
    allocatedResources(NewPlacement,NewAlloc), 
    retract(deployment(A,_,_)), assert(deployment(A,NewPlacement,NewAlloc)).

reasoningStep([on(S,_)|Ps],(AllocHW,AllocBW),KOs,POk,StableP) :-
    \+ service(S,_,_,_), 
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,POk,StableP).
reasoningStep([on(S,N)|Ps],(AllocHW,AllocBW), KOs, POk,StableP) :-
    nodeOk(S,N,POk,AllocHW), linksOk(S,N,POk,AllocBW),!,
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,[on(S,N)|POk],StableP).
reasoningStep([on(S,_)|Ps],(AllocHW,AllocBW),[S|KOs],POk,StableP) :-
    reasoningStep(Ps,(AllocHW,AllocBW),KOs,POk,StableP).
reasoningStep([],_,[],P,P).

newServices(P, NewServices) :- findall(S, (service(S,_,_,_), \+ member(on(S,_),P)), NewServices).

placement(A,P) :- 
    application(A,Services), placement(Services,[],([],[]),P),
    allocatedResources(P,Alloc), assert(deployment(A,P,Alloc)). 

allocatedResources(P,(AllocHW,AllocBW)) :- 
    findall((N,HW), (member(on(S,N),P), service(S,_,HW,_)), AllocHW),
    findall((N1,N2,BW), n2n(P,N1,N2,BW), AllocBW).
n2n(P,N1,N2,ReqBW) :- s2s(S1,S2,_,ReqBW), member(on(S1,N1),P), member(on(S2,N2),P), dif(N1,N2).
