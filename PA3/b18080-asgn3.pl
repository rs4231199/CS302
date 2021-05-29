%% alloc(d,o1).
%% alloc(a,o3).
%% alloc(b,o2).
%% alloc(c,o4).
%% store(c,g,d).
%% store(a,f,b).
%% copy(c,b).

%% %%% dummy cases
%% alloc(xxx, ttt).
%% copy(xxx, ttt).
%% load(xxx, ttt, ddd).
%% store(xxx, ttt, ddd).


goal(X, One, Vis) :- alloc(X, One), not(member(One, Vis)).
goal(X, One, Vis) :- copy(X, Tmp1), not(member(Tmp1, Vis)), goal(Tmp1, One, [X | Vis]).
goal(X, One, Vis) :- load(X, Tmp1, Tmp2), store(Tmp1, Tmp2, Tmp3), not(member(Tmp3, Vis)), goal(Tmp3, One, [X | Vis]).
pointees(X, L) :- findall(One, goal(X, One, []), L2), sort(L2, L).

%% pointees(a, L).


goal2(X, One, [ HeadTail ], _) :- store(X, HeadTail, Tmp1), goal(Tmp1, One, []).
goal2(X, One, [Head, Tail], Vis) :- store(X, Head, Tmp1), goal2(Tmp1, One, Tail, Vis).
goal2(X, One, FF, Vis) :- copy(X, Tmp1), not(member(Tmp1, Vis)), goal2(Tmp1, One, FF, Vis).

fieldpointees(X, FF, L) :- findall(One, goal2(X, One, FF, []), L2), sort(L2, L).
