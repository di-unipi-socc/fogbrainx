:-dynamic deployment/3.
:-dynamic application/2.
:-dynamic service/4.
:-dynamic s2s/4.
:-dynamic link/4.
:-dynamic node/4.

:- consult('fbX2').
:-consult('placers/placer').

run :- make, show_coverage(run_tests).

exec_test(TestSpec, SP) :-
    string_concat('tests/', TestSpec, Test), consult(Test),
    fogBrain(toyApp, P),
    retract(deployment(toyApp, P, (AllocHW,AllocBW))),
    msort(P, SP), msort(AllocHW, SortedHW), msort(AllocBW, SortedBW),
    sumHW(SortedHW, [], SHW), sumBW(SortedBW, [], SBW),
    print(TestSpec, SP, SHW, SBW),
    checkHW(P, SHW), checkBW(P, SBW),
    unload_file(Test).

print(Test, P, HW, BW):- 
    write(Test), write(': '), 
    write(P), write('-'), 
    write(HW), write('-'), 
    writeln(BW).

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

:- begin_tests(fogBrain).

test(reasoning, [
    true(P =@= [on(s1,n1),on(s2,n3)]),
    nondet
    ]) :- exec_test('reasoning', P).

test(placer, [
    true(P =@= [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('placer', P).

test(nop, [
    true(P =@= [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('nop', P).

test(addServiceUpdate, [
    true(P =@=  [on(s1,n1),on(s2,n2),on(s3,n3)]),
    nondet
    ]) :- exec_test('addServiceUpdate', P).

test(removeServiceUpdate, [
    true(P =@=  [on(s1,n1)]),
    nondet
    ]) :- exec_test('removeServiceUpdate', P).

test(changeServiceUpdate, [
    true(P =@=  [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeServiceUpdate', P).

test(changeServiceMigration, [
    true(P =@=  [on(s1,n3),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeServiceMigration', P).

test(addServiceMigration, [
    true(P =@=  [on(s1,n3),on(s2,n2),on(s3,n1)]),
    nondet
    ]) :- exec_test('addServiceMigration', P).

test(changeMigration, [
    true(P =@=  [on(s1,n3),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeMigration', P).

test(addS2SMigration, [
    true(P =@=  [on(s1,n1),on(s2,n2),on(s3,n3)]),
    nondet
    ]) :- exec_test('addS2SMigration', P).

test(addS2SMigration1, [
    true(P =@=  [on(s1,n2),on(s2,n1),on(s3,n3)]),
    nondet
    ]) :- exec_test('addS2SMigration1', P).

test(addS2SUpdate, [
    true(P =@=  [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('addS2SUpdate', P).

test(changeS2SMigration, [
    true(P =@=  [on(s1,n3),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeS2SMigration', P).

test(changeS2SUpdate, [
    true(P =@=  [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeS2SUpdate', P).

test(replace, [
    true(P =@= [on(s2,n2),on(s3,n1)]),
    nondet
    ]) :- exec_test('replace', P).

test(twoInTheSame, [
    true(P =@=  [on(s1,n1),on(s2,n2),on(s3,n1)]),
    nondet
    ]) :- exec_test('twoInTheSame', P).

test(newNodeLink, [
    true(P =@= [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('newNodeLink', P).

test(removeNodeLink, [
    true(P =@= [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('removeNodeLink', P).

test(removeNodeMigration, [
    true(P =@= [on(s1,n3),on(s2,n2)]),
    nondet
    ]) :- exec_test('removeNodeMigration', P).

test(removeLinkMigration, [
    true(P =@= [on(s1,n3),on(s2,n2)]),
    nondet
    ]) :- exec_test('removeLinkMigration', P).

test(changeNodeUpdate, [
    true(P =@= [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeNodeUpdate', P).

test(changeLinkUpdate, [
    true(P =@= [on(s1,n1),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeLinkUpdate', P).

test(changeNodeMigration, [
    true(P =@= [on(s1,n3),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeNodeMigration', P).

test(changeLinkMigration, [
    true(P =@= [on(s1,n3),on(s2,n2)]),
    nondet
    ]) :- exec_test('changeLinkMigration', P).

test(removeAll, [
    true(P =@= []),
    nondet
    ]) :- exec_test('removeAll', P).

test(removeAllandAddTwo, [
    true(P =@= [on(s3,n1),on(s4,n2)]),
    nondet
    ]) :- exec_test('removeAllandAddTwo', P).

test(removeAllAddandChange, [
    true(P =@= [on(s3,n1),on(s4,n2)]),
    nondet
    ]) :- exec_test('removeAllAddandChange', P).

test(twoInTheSameLink, [
    true(P =@= [on(s1,n1),on(s2,n2),on(s3,n1),on(s4,n1)]),
    nondet
    ]) :- exec_test('twoInTheSameLink', P).

:- end_tests(fogBrain).
