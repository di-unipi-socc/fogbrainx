application(toyApp, [s1,s2,s3]).
service(s1, [], 6, []).
service(s2, [], 6, []).
service(s3, [], 6, []).
s2s(s1, s2, 20, 10).
s2s(s2, s1, 20, 10).
s2s(s3, s1, 20, 50).

deployment(toyApp, [on(s2, n2), on(s1, n1)], ([(n2, 6), (n1, 6)],[(n1, n2, 10), (n2, n1, 10)])).

node(n1,[], 4,[]).
node(n2,[], 4,[]).
node(n3,[], 10,[]).
link(n1,n2,20,40).
link(n2,n1,20,40).
link(n1,n3,20,50).
link(n3,n1,20,50).
link(n2,n3,20,70).
link(n3,n2,20,70).
