/* vim: set ft=prolog: */
/*
 * CS131 Homework 6
 * Gautam Gupta
 * #304282688
 * Worked with: Jennifer Tan, Kelly Hosokawa, David Pu, Alex Crosthwaite
 */

duplist([], []).
duplist([H|T], L) :- append([H, H], Lr, L), duplist(T, Lr).

subseq([],[]).
subseq([H|T1], [H|T2]) :- subseq(T1, T2).
subseq(L, [_|T]) :- subseq(L, T).

/*
 * Helper for verbalarithmetic
 */
seqtoexpr([], 0).
seqtoexpr([H|T], X) :- length(T, L), seqtoexpr(T, Y), X = (H * (10**L)) + Y.

verbalarithmetic(Letters, Word1, Word2, Result) :-
    fd_domain(Letters, 0, 9),
    fd_all_different(Letters),
	[H1|_] = Word1,
	H1 #\= 0,
    [H2|_] = Word2,
    H2 #\= 0,
    [H3|_] = Result,
    H3 #\= 0,
    seqtoexpr(Word1, Expr1),
    seqtoexpr(Word2, Expr2),
    seqtoexpr(Result, ResExpr),
    ResExpr #= Expr1 + Expr2,
	fd_labeling(Letters).

performaction(world([B|T], S2, S3, none), pickup(B, stack1), world(T, S2, S3, B)).
performaction(world(S1, [B|T], S3, none), pickup(B, stack2), world(S1, T, S3, B)).
performaction(world(S1, S2, [B|T], none), pickup(B, stack3), world(S1, S2, T, B)).
performaction(world(S1, S2, S3, B), putdown(B, stack1), world([B|S1], S2, S3, none)) :- B \= none.
performaction(world(S1, S2, S3, B), putdown(B, stack2), world(S1, [B|S2], S3, none)) :- B \= none.
performaction(world(S1, S2, S3, B), putdown(B, stack3), world(S1, S2, [B|S3], none)) :- B \= none.

blocksworld(W, [], W).
blocksworld(Start, [Action|Actions], End) :-
    performaction(Start, Action, Res), blocksworld(Res, Actions, End).
