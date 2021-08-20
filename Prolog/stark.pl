father(rickard, ned).
father(rickard, brandon).
father(rickard, benjen).
father(rickard, lyanna).
father(ned, robb).
father(ned, sansa).
father(ned, arya).
father(ned, bran).
father(ned, rickon).
father(rhaegar, jon).

mother(catelyn, robb).
mother(catelyn, sansa).
mother(catelyn, arya).
mother(catelyn, bran).
mother(catelyn, rickon).
mother(lyanna, jon).
mother(lyarra, ned).
mother(lyarra, brandon).
mother(lyarra, benjen).
mother(lyarra, lyanna).

parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

