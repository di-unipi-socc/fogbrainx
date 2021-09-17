:-dynamic deployment/3.
:-dynamic application/2.
:-dynamic service/4.
:-dynamic s2s/4.
:-dynamic link/4.
:-dynamic cap/2.
:-dynamic cap/4.
:-dynamic node/4.

set_seed :- set_random(seed(481183)).

migrations(OldP, NewP, OldL, NewL, M, NM) :-
	findall(1, (member(on(S,N),OldP), member(on(S,N1),NewP), dif(N,N1)), Ms), length(Ms,M),
	findall(1, (member(on(S,N),OldP), member(on(S,N),NewP)), NMs), length(NMs,NM),
	length(OldP,OldL), length(NewP,NewL).

cr(AppSpec, L, NewPlacement, InferencesCR, TimeCR, OldL, NewL, M, NM):-
	consult(AppSpec), consult('deployment.pl'), %consult('infra.pl'), 
	findall(deployment(A, Ss, S2Ss), deployment(A, Ss, S2Ss),OldDeployment),  %writeln(OldDeployment),
	enactDeployment(OldDeployment), %%%
	statistics(inferences, Before1),
		statistics(cputime, T1),
			application(AppId,_), fogBrain(AppId, NewPlacement), 
		statistics(cputime, T2),
	statistics(inferences, After1), InferencesCR is After1 - Before1 - 5, TimeCR is T2 - T1,
	removeDeployment(OldDeployment), 
	changeInfra(L),
	findall(deployment(A, Ss, S2Ss), deployment(A, Ss, S2Ss),[D]), writeDeployment('deployment.pl',D), 
	getPlacement(OldDeployment,OldP), migrations(OldP, NewPlacement, OldL, NewL, M, NM),
	retractall(deployment(_,_,_)), unload_file(AppSpec).

getPlacement([],[]).
getPlacement([deployment(_,P,_)],P).

changeInfra(L) :-
	changeNodes(L), changeLinks(L).

changeNodes(L) :-
	findall(node(N,SW,HW,T), picknode(L,N,SW,HW,T), Ns),
	changeNodes2(Ns).

picknode(L,N,SW,HW,T) :-
	random(F), F=<L, node(N,SW,HW,T).

changeNodes2([]).
changeNodes2([node(TargetNode,SW,HW,T)|Ns]) :- 
	change(node(TargetNode,SW,HW,T)),
	changeNodes2(Ns).

change(node(TargetNode,SW,HW,T)) :- 
	retract(node(TargetNode,SW,HW,T)), cap(TargetNode,C),
	( (dif(C, inf), HWMax is 1.1*C); HWMax = 100.0 ), %1.1
	random(0.1, HWMax, NewHW),
	assert(node(TargetNode,SW,NewHW,T)).

changeLinks(L) :-
	findall(link(N1,N2,Lat,BW),picklink(L,N1,N2,Lat,BW), Ls),
	changeLinks2(Ls).

picklink(L,N1,N2,Lat,BW) :-
	random(F), F=<L, link(N1,N2,Lat,BW).

changeLinks2([]).
changeLinks2([link(N1,N2,Lat,BW)|Ns]) :- 
	changeLink(link(N1,N2,Lat,BW)),
	changeLinks2(Ns).

changeLink(link(N1,N2,Lat,BW)) :- 
	retract(link(N1,N2,Lat,BW)), cap(N1,N2,BaseLat,C),
	( (dif(C, inf), BWMax is 1.1*C); BWMax = 100.0 ), %1.1
	random(0.1, BWMax, NewBw),
	MinLat is BaseLat/2, MaxLat is BaseLat*1.5,
	random(MinLat, MaxLat, NewLat), 
	assert(link(N1,N2,NewLat,NewBw)).

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

p(AppSpec, NewPlacement, InferencesNoCR, TimeNoCR, OldL, NewL, M, NM) :-
	consult('edeployment.pl'),
	findall(deployment(A, Ss, S2Ss), deployment(A, Ss, S2Ss),OldDeployment),
	retractall(deployment(_,_,_)),
	consult(AppSpec), application(AppId, _), %consult('infra.pl'),
	%enactDeployment(OldDeployment), %%%
	statistics(inferences, Before2),
		statistics(cputime, T1),
			placement(AppId,NewPlacement),
		statistics(cputime, T2),
	statistics(inferences, After2), InferencesNoCR is After2 - Before2 - 5, TimeNoCR is T2 - T1, 
	%removeDeployment(OldDeployment), 
	findall(deployment(A, Ss, S2Ss), deployment(A, Ss, S2Ss),[D]), writeDeployment('edeployment.pl',D), 
	getPlacement(OldDeployment,OldP), migrations(OldP, NewPlacement, OldL, NewL, M, NM),
	retractall(deployment(_,_,_)), unload_file(AppSpec).

random_range(_,_,0,[]).
random_range(L, U, N, [R|Ls]) :-
    random(L,U,R),
    NewN is N-1,
    random_range(L,U,NewN,Ls).

writeDeployment(File,D) :-
    open(File,write,Out),
    write(Out,D), write(Out,'.\n'),
    close(Out).

load_infra :- unload_file('infra.pl'), consult('infra.pl'), setCaps.

setCaps :- retractall(cap(_,_)), findall(_, setCap(_,_),_),
		   retractall(cap(_,_,_,_)),findall(_, setCap(_,_,_,_),_).

setCap(N,C) :- node(N,_,C,_), assert(cap(N,C)).
setCap(N1,N2,Lat,Bw) :- link(N1,N2,Lat,Bw), assert(cap(N1,N2,Lat,Bw)).
