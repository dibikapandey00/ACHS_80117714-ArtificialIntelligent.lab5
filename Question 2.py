#Represent and reason about a semantic network in Prolog using graph concepts. Your program will store entities, their relationships, and allow reasoning through inheritance by traversing the graph.

:- dynamic entity/1.
:- dynamic rel/3.        % rel(From, Relation, To)
:- dynamic prop/3.       % prop(Entity, Attribute, Value)

add_entity(E) :- (entity(E) -> true ; assertz(entity(E))).

add_rel(X, R, Y) :-
    add_entity(X), add_entity(Y),
    assertz(rel(X, R, Y)).

add_prop(E, A, V) :-
    add_entity(E),
    assertz(prop(E, A, V)).

isa(X, Y) :- rel(X, isa, Y).

ancestor(X, Y) :- isa(X, Y).
ancestor(X, Y) :- isa(X, Z), ancestor(Z, Y).

has_own_prop(E, A) :- prop(E, A, _).

inherited_prop(E, A, V) :- prop(E, A, V).
inherited_prop(E, A, V) :-
    ancestor(E, Anc),
    prop(Anc, A, V),
    \+ has_own_prop(E, A).

has_prop(E, A, V) :- inherited_prop(E, A, V).

related(X, R, Y) :- rel(X, R, Y).

path(X, Y, P) :- path_dfs(X, Y, [X], P).
path_dfs(Y, Y, Acc, P) :- reverse(Acc, P).
path_dfs(X, Y, Vis, P) :-
    rel(X, _, N),
    \+ member(N, Vis),
    path_dfs(N, Y, [N|Vis], P).

labeled_path(X, Y, Labeled) :- lp_dfs(X, Y, [X], [], Labeled).
lp_dfs(Y, Y, _, Acc, Labeled) :- reverse(Acc, Labeled).
lp_dfs(X, Y, Vis, Acc, Labeled) :-
    rel(X, R, N),
    \+ member(N, Vis),
    lp_dfs(N, Y, [N|Vis], [edge(X,R,N)|Acc], Labeled).

ancestors(E, As) :- setof(A, ancestor(E, A), As) ; As = [].
descendant_of(A, Ds) :- setof(D, ancestor(D, A), Ds) ; Ds = [].