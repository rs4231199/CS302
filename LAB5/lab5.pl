% myLength(L,N)
myLength([], 0).
myLength([ _ | Tail ], N) :- myLength(Tail, M), N is M + 1.
% myLength([a, b, b], X).

%  myLast(X, L)
myLast(X,[ X ]).
myLast(X,[ _ | L ]) :- myLast(X, L).

% 3

% 4 A K M P N 
rooms([room(_,5),room(_,4),room(_,3),room(_,2),room(_,1)]).
hostel(Rooms) :- rooms(Rooms),
 member(room(akash, A), Rooms), A \= 5,
 member(room(kairav, K), Rooms), K \= 1,
 member(room(milind, M), Rooms), M \= 1,
 member(room(piyush, P), Rooms),
 not(adjacent(M, P)), not(adjacent(M, K)),
 member(room(nites, N), Rooms), N > K,
 print_rooms(Rooms).

adjacent(X, Y) :- X =:= Y+1.
adjacent(X, Y) :- X =:= Y-1.
print_rooms([A | B]) :- write(A), nl, print_rooms(B).
print_rooms([]).