% https://github.com/CS131-TA-team/UCLA_CS131_CodeHelp/tree/master/Prolog
tower(N,T,C):-
    C = counts(Top,Bottom,Left,Right),
    len_row(Top,N),
    len_row(Bottom,N),
    len_row(Left,N),
    len_row(Right,N),
    len_row(T,N),
    len_col(T, N),
    within_domain(T, N),
    maplist(fd_all_different, T),
    transpose(T, X),
    maplist(fd_all_different, X),
    maplist(fd_labeling, T),
    maplist(reverse,X,RX),
    maplist(confirm,Top,X),
    maplist(confirm,Bottom,RX),
    maplist(reverse,T,RT),
    maplist(confirm,Left,T),
    maplist(confirm,Right,RT).

confirm(A,B):-
    valid_count(B,A,0,0).

valid_count([],N,_,N).
valid_count([H|T],N,Max,Cnt):-
    ((H>Max,Maximum is H,Number is Cnt+1);(H=<Max,Maximum is Max, Number is Cnt)),
    valid_count(T,N,Maximum,Number).

len_row(X, N) :-
    length(X, N).

len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

within_domain([], _).
within_domain([HD | TL], N) :-
    fd_domain(HD, 1, N),
    within_domain(TL, N).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%----------------------------------------------------------------

% TA Week 5 Slides

all_unique([]).
all_unique([Hd | Tl]) :-
    member(Hd, Tl), !, fail. 
all_unique([_ | Tl]) :-
    all_unique(Tl).

helper(N, L) :-
  length(L, N),
  maplist(between(1, N), L),
  all_unique(L).
    
%----------------------------
plain_tower(N,T,C):-
    len_row(T,N),
    len_col(T, N),   
    maplist(helper(N), T),
    transpose(T, X),
    maplist(helper(N), X),
    C = counts(Top,Bottom,Left,Right),
    len_row(Top,N),
    len_row(Bottom,N),
    len_row(Left,N),
    len_row(Right,N),
    maplist(confirm,Top,X),
    maplist(reverse,X,RX),
    maplist(confirm,Bottom,RX),
    maplist(confirm,Left,T),
    maplist(reverse,T,RT),
    maplist(confirm,Right,RT).
   
%-----------------------------------------------------------------
ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.

%-----------------------------------------------------------------

tower_test(Time):-
    statistics(cpu_time, [Start|_]),
    tower(4,_,counts([1,3,3,2],[3,1,2,3],[1,2,3,2],[2,1,2,3])),
    statistics(cpu_time, [End|_]),
    Time is End-Start.

plain_tower_test(P):-
    statistics(cpu_time, [Start|_]),
    plain_tower(4,_,counts([1,3,3,2],[3,1,2,3],[1,2,3,2],[2,1,2,3])),
    statistics(cpu_time, [End|_]),
    P is End-Start.

speedup(Ratio):-
    tower_test(Time),
    plain_tower_test(P),
    Ratio is P/Time.
