application(toyApp, [s1,s2,s3]).
service(s1, [], 1, []). % 6 -> 1
service(s2, [], 6, []).
service(s3, [], 9, []). % new service
s2s(s1, s2, 20, 10).
s2s(s2, s1, 20, 10).
s2s(s1, s3, 20, 10).
s2s(s3, s1, 20, 10).
s2s(s3, s2, 20, 10).
s2s(s2, s3, 20, 10).

deployment(toyApp, [on(s2, n2), on(s1, n1)], ([(n2, 6), (n1, 6)],[(n1, n2, 10), (n2, n1, 10)])).

%%node(n1,[], 10,[]).  %before performing previous deployment [on(s2,n2),on(s1,n1)]
node(n1,[], 4,[]).     %because of performed deployment [on(s2,n2),on(s1,n1)]
%%node(n2,[], 10,[]).  %before performing previous deployment [on(s2,n2),on(s1,n1)]
node(n2,[], 1,[]).
node(n3,[], 2,[]).
%%link(n1,n2,20,50).   %before performing previous deployment [on(s2,n2),on(s1,n1)]
link(n1,n2,20,40).     %because of performed deployment [on(s2,n2),on(s1,n1)]
%%link(n2,n1,20,50).   %before performing previous deployment [on(s2,n2),on(s1,n1)]
link(n2,n1,20,40).     %because of performed deployment [on(s2,n2),on(s1,n1)]
link(n1,n3,20,50).
link(n3,n1,20,50).
link(n2,n3,20,50).
link(n3,n2,20,50).
