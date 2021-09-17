:-dynamic deployment/3.
:-dynamic application/2.
:-dynamic service/4.
:-dynamic s2s/4.
:-dynamic link/4.
:-dynamic node/4.

:- consult('fbX2').
:- consult(placer:'placers/placer').

run(Placer) :- consult(Placer), make, show_coverage(run_tests).

exec_test(TestSpec) :-
    string_concat('tests/', TestSpec, Test), consult(Test),
    (deployment(toyApp, _, OldAlloc);OldAlloc=([],[])),
    fogBrain(toyApp, P),
    retract(deployment(toyApp, P, (AllocHW, AllocBW))),
    msort(P, SP), msort(AllocHW, SortedHW), msort(AllocBW, SortedBW),
    sumHW(SortedHW, [], SHW), sumBW(SortedBW, [], SBW),
    print(TestSpec, SP, SHW, SBW),
    checkHW(P, SHW), checkBW(P, SBW),
    checker(OldAlloc, SP),
    unload_file(Test).

checker(OldAlloc, Placement) :-
    application(toyApp, Services),
	findnsols(1, 1, (placer:placement(Services,[],OldAlloc,P), sort(P,Placement)), [1]).

checkHW(_,[]).
checkHW(Placement, [(N,HW)|Alloc]) :-
    findall(H, (member(on(S,N),Placement), service(S,_,H,_)), LocalAlloc),
    sum_list(LocalAlloc, HW),
    checkHW(Placement, Alloc).

checkBW(_,[]).
checkBW(Placement, [(N1,N2,BW)|Alloc]) :-
    findall(B, (member(on(S1,N1),Placement), member(on(S2,N2),Placement), s2s(S1,S2,_,B)), LocalAlloc),
    sum_list(LocalAlloc, BW),
    checkBW(Placement, Alloc).
    
sumHW([], A, A).
sumHW([X|Ls], [], A) :-
    sumHW(Ls, [X], A).
sumHW([(N,HW)|Ls], [(N,HW1)|A1], A) :-
    NewHW is HW + HW1,
    sumHW(Ls, [(N,NewHW)|A1], A).
sumHW([(N,HW)|Ls], [(N1,HW1)|A1], A) :-
    dif(N,N1),
    sumHW(Ls, [(N,HW),(N1,HW1)|A1], A).

sumBW([], A, A).
sumBW([X|Ls], [], A) :-
    sumBW(Ls, [X], A).
sumBW([(N1,N2,BW)|Ls], [(N1,N2,BW1)|A1], A) :-
    NewBW is BW + BW1,
    sumBW(Ls, [(N1,N2,NewBW)|A1], A).
sumBW([(N1,N2,BW)|Ls], [(N3,N4,BW1)|A1], A) :-
    (dif(N1,N3); dif(N2,N4)),
    sumBW(Ls, [(N1,N2,BW),(N3,N4,BW1)|A1], A).

print(Test, P, HW, BW):- 
    write(Test), write(': '), 
    write(P), write('-'), 
    write(HW), write('-'), 
    writeln(BW).

:- begin_tests(fogBrain).

test(reasoning, [
    nondet
    ]) :- exec_test('reasoning').

test(placer, [
    nondet
    ]) :- exec_test('placer').

test(nop, [
    nondet
    ]) :- exec_test('nop').

test(addServiceUpdate, [
    nondet
    ]) :- exec_test('addServiceUpdate').

test(removeServiceUpdate, [
    nondet
    ]) :- exec_test('removeServiceUpdate').

test(changeServiceUpdate, [
    nondet
    ]) :- exec_test('changeServiceUpdate').

test(changeServiceMigration, [
    nondet
    ]) :- exec_test('changeServiceMigration').

test(addServiceMigration, [
    
    nondet
    ]) :- exec_test('addServiceMigration').

test(changeServiceMigration, [
    
    nondet
    ]) :- exec_test('changeMigration').

test(addS2SMigration, [
    
    nondet
    ]) :- exec_test('addS2SMigration').

test(addS2SMigration1, [
    
    nondet
    ]) :- exec_test('addS2SMigration1').

test(addS2SUpdate, [
    
    nondet
    ]) :- exec_test('addS2SUpdate').

test(changeS2SMigration, [
    
    nondet
    ]) :- exec_test('changeS2SMigration').

test(changeS2SUpdate, [
    
    nondet
    ]) :- exec_test('changeS2SUpdate').

test(replace, [
    
    nondet
    ]) :- exec_test('replace').

test(twoInTheSame, [
    
    nondet
    ]) :- exec_test('twoInTheSame').

test(newNodeLink, [
    
    nondet
    ]) :- exec_test('newNodeLink').

test(removeNodeLink, [
    
    nondet
    ]) :- exec_test('removeNodeLink').

test(removeNodeMigration, [
    
    nondet
    ]) :- exec_test('removeNodeMigration').

test(removeLinkMigration, [
    
    nondet
    ]) :- exec_test('removeLinkMigration').

test(changeNodeUpdate, [
    
    nondet
    ]) :- exec_test('changeNodeUpdate').

test(changeLinkUpdate, [
    
    nondet
    ]) :- exec_test('changeLinkUpdate').

test(changeNodeMigration, [
    
    nondet
    ]) :- exec_test('changeNodeMigration').

test(changeLinkMigration, [
    
    nondet
    ]) :- exec_test('changeLinkMigration').

test(removeAll, [
    
    nondet
    ]) :- exec_test('removeAll').

test(removeAllandAddTwo, [
    
    nondet
    ]) :- exec_test('removeAllandAddTwo').

test(removeAllAddandChange, [
    
    nondet
    ]) :- exec_test('removeAllAddandChange').

test(twoInTheSameLink, [
    
    nondet
    ]) :- exec_test('twoInTheSameLink').


:- end_tests(fogBrain).
