:- use_module(library(clpfd)).

tabuleiro(1, [[_, _, _, _, 1],
              [3, _, _, _, _],
              [_, _, _, _, _],
              [5, _, _, 1, _],
              [_, _, _, 4, _]],
             [[1, 1, 2, 2, 2],
              [3, 3, 3, 2, 2],
              [6, 3, 4, 4, 4],
              [6, 6, 6, 4, 4],
              [6, 5, 5, 5, 5]]).
      
% WIP
suguru(Rows) :-
    maplist(same_length(Rows), Rows),  
    append(Rows, Vs),
    Rows = [As, Bs, Cs, Ds, Es],  % FIXME funcionará somente para 5x5
    append(Chunk1, [_,_], Rows),
    append([_|Chunk2], [_], Rows),
    append([_,_|Chunk3], [], Rows),
    maplist(unique_adjacency, Chunk1, Chunk2, Chunk3),
    distintos_row(Vs).
    


% Define se os numeros sao unicos com relação às celulas adjacentes
% [[N1-_, N2-_, N3-_],
%  [N4-_, N5-_, N6-_],
%  [N7-_, N8-_, N9-_]]
unique_adjacency([N1, N2, N3 | T1], [N4, N5, N6 | T2], [N7, N8, N9 | T3]) :-
    all_distinct([N1, N2, N4, N5]), % checa canto superior esquerdo
    all_distinct([N3, N2, N5, N6]), % checa canto superior direito
    all_distinct([N7, N4, N5, N8]), % checa canto inferior esquerdo
    all_distinct([N9, N6, N5, N8]), % checa canto inferior direito
    append([N2, N3], T1, TT1),
    append([N5, N6], T2, TT2),
    append([N8, N9], T3, TT3),
    unico_adjacente(TT1, TT2, TT3).
unique_adjacency([_,_], [_,_], [_,_]).


% Define uma regra para valores V pertencentes a regiao Region conforme a lista de regioes Rt.
% Os valores pertencentes a regiao Region sao guardados em At.
values_region(_, [], [], []).
values_region(Region, [V|Vt], [R|Rt], [V|At]) :- R #= Region, values_region(Region, Vt, Rt, At).
values_region(Region, [V|Vt], [R|Rt], At) :- R \= Region, values_region(Region, Vt, Rt, At).

/*
% Define se os numeros em uma linha sao distintos na sua regiao.
% ?- distintos_row([1-1, 1-2, 4-3, 5-2])
% true .
% ?- distintos_row([1-1, 1-2, 4-3, 1-1, 5-2])
% false .
distintos_row([N1-R1, N2-R2 | T]) :- 
    R1 \== R2, 
    append([N1-R1], T, TT1), 
    append([N2-R2], T, TT2), 
    distintos_row(TT1), 
    distintos_row(TT2).
distintos_row([N1-R, N2-R | T]) :- 
    N1 \== N2, 
    append([N1-R], T, TT1), 
    append([N2-R], T, TT2), 
    distintos_row(TT1), 
    distintos_row(TT2).
distintos_row([_-_]).
*/