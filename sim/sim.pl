:-dynamic deployment/3.
:-dynamic application/2.
:-dynamic service/4.
:-dynamic s2s/4.
:-dynamic link/4.
:-dynamic node/4.

try(I,P) :-
	consult('./commits/sockshop.pl'),
	string_concat('infra/infra', I, String),
	string_concat(String, '.pl', Infra),
	consult(Infra),
	consult('../fbX2Official.pl'),
	consult('../placers/placer.pl'),
	fogBrain(sockshop, P).

cr(AppSpec, NewPlacement, InferencesCR, TimeCR, OldL, NewL, M, NM):-
    consult('deployment.pl'), findall(deployment(A, Ss, S2Ss), deployment(A, Ss, S2Ss), OldDeployment),
    
    enactDeployment(OldDeployment), 

    consult(AppSpec), application(AppId,_), 
    statistics(inferences, Before1),
        statistics(cputime, T1),
            fogBrain(AppId, NewPlacement), 
        statistics(cputime, T2),
    statistics(inferences, After1), InferencesCR is After1 - Before1 - 5, TimeCR is T2 - T1,
    
    removeDeployment(OldDeployment), 

    findall(deployment(A, Ss, S2Ss), deployment(A, Ss, S2Ss),[D]), writeDeployment('deployment.pl',D), 
    getPlacement(OldDeployment,OldP), migrations(OldP, NewPlacement, OldL, NewL, M, NM),
    retractall(deployment(_,_,_)), unload_file(AppSpec).

p(AppSpec, NewPlacement, InferencesNoCR, TimeNoCR, OldL, NewL, M, NM) :-
	consult('edeployment.pl'), findall(deployment(A, Ss, S2Ss), deployment(A, Ss, S2Ss),OldDeployment),
	
    %enactDeployment(OldDeployment), 
    
	consult(AppSpec), application(AppId, _),
    retractall(deployment(_,_,_)), 
	statistics(inferences, Before2),
		statistics(cputime, T1),
			placement(AppId,NewPlacement),
		statistics(cputime, T2),
	statistics(inferences, After2), InferencesNoCR is After2 - Before2 - 5, TimeNoCR is T2 - T1, 
	
    %removeDeployment(OldDeployment), 

	findall(deployment(A, Ss, S2Ss), deployment(A, Ss, S2Ss),[D]), writeDeployment('edeployment.pl',D), 
	getPlacement(OldDeployment,OldP), migrations(OldP, NewPlacement, OldL, NewL, M, NM),
	retractall(deployment(_,_,_)), unload_file(AppSpec).

removeDeployment([]).
removeDeployment([deployment(_,_,(AllocHW, AllocBW))]) :-
	incrementNodes(AllocHW), incrementLinks(AllocBW).

incrementNodes([]).
incrementNodes([(N,_)|AllocHW]) :-
	node(N,_,HW,_), HW=inf, incrementNodes(AllocHW).
incrementNodes([(N,OldHW)|AllocHW]) :-
	retract(node(N,SW,HW,T)), dif(HW, inf),
	NewHW is HW + OldHW,
	assert(node(N,SW,NewHW,T)),
	incrementNodes(AllocHW).

incrementLinks([]).
incrementLinks([(N1,N2,_)|Ls]) :-
	link(N1,N2,_,inf), incrementLinks(Ls).
incrementLinks([(N1,N2,OldBW)|Ls]) :-
	retract(link(N1,N2,Lat,BW)), dif(BW,inf),
	NewBW is BW + OldBW,
	assert(link(N1,N2,Lat,NewBW)),
	incrementLinks(Ls).

enactDeployment([]).
enactDeployment([deployment(_,_,(AllocHW, AllocBW))]) :-
	decrementNodes(AllocHW), decrementLinks(AllocBW).

decrementNodes([]).
decrementNodes([(N,_)|AllocHW]) :-
	node(N,_,HW,_), HW=inf, decrementNodes(AllocHW).
decrementNodes([(N,OldHW)|AllocHW]) :-
	retract(node(N,SW,HW,T)), dif(HW, inf),
	NewHW is HW - OldHW,
	assert(node(N,SW,NewHW,T)),
	decrementNodes(AllocHW).

decrementLinks([]).
decrementLinks([(N1,N2,_)|Ls]) :-
	link(N1,N2,_,inf), decrementLinks(Ls).
decrementLinks([(N1,N2,OldBW)|Ls]) :-
	retract(link(N1,N2,Lat,BW)), dif(BW,inf),
	NewBW is BW - OldBW,
	assert(link(N1,N2,Lat,NewBW)),
	decrementLinks(Ls).

migrations(OldP, NewP, OldL, NewL, M, NM) :-
	findall(1, (member(on(S,N),OldP), member(on(S,N1),NewP), dif(N,N1)), Ms), length(Ms,M),
	findall(1, (member(on(S,N),OldP), member(on(S,N),NewP)), NMs), length(NMs,NM),
	length(OldP,OldL), length(NewP,NewL).

getPlacement([],[]).
getPlacement([deployment(_,P,_)],P).

writeDeployment(File,D) :-
    open(File,write,Out),
    write(Out,D), write(Out,'.\n'),
    close(Out).

load_infra(Infra) :-
	consult(Infra).

unload_infra :-
	retractall(node(_,_,_,_)),
	retractall(link(_,_,_,_)).
