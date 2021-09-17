application(toyApp, [s1,s2,s3,s4]).
service(s1, [], 6, [sw]).
service(s2, [], 6, [sw2]).
service(s3, [], 1, [sw]).
service(s4, [], 1, [sw]).
s2s(s1, s2, 20, 10).
s2s(s2, s1, 20, 10).
s2s(s3, s2, 20, 10).
s2s(s2, s3, 20, 10).
s2s(s2, s4, 20, 20).

deployment(toyApp, [on(s2, n2), on(s1, n1)], ([(n2, 6), (n1, 6)],[(n1, n2, 10), (n2, n1, 10)])).

node(n1,[], 4,[sw]).
node(n2,[], 4,[sw2]).
node(n3,[], 10,[]).
link(n1,n2,20,40).
link(n2,n1,20,40).
link(n1,n3,20,50).
link(n3,n1,20,50).
link(n2,n3,20,50).
link(n3,n2,20,50).
