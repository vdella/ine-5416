:- use_module(library(clpfd)).

tabuleiro(1, [[_-1, _-1, _-2, _-2, 1-2],
              [3-3, _-3, _-3, _-2, _-2],
              [_-4, _-3, _-5, _-5, _-5],
              [5-4, _-4, _-3, 1-5, _-5],
              [_-4, _-6, _-6, _-6, _-6]]).
      
% WIP
suguru(Rows) :-
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Rows = [As, Bs, Cs, Ds, Es],
    append(Chunk1, [_,_], Rows),
    append([_|Chunk2], [_], Rows),
    append([_,_|Chunk3], [], Rows),
    maplist(unico_adjacente, Chunk1, Chunk2, Chunk3),
    distintos_row(Vs),
    
/*
blocks([N1-R1,N2-R2,N3-R3|Ns1], [N4-R4,N5-R5,N6-R6|Ns2], [N7-R7,N8-R8,N9-R9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).
    
size_region_row(_, 0, []).
size_region_row(R, Result, [C1-R1 | T]) :- R \== R1, size_region(R, Result2, T), Result is Result2 + 0.
size_region_row(R, Result, [C1-R | T]) :- size_region(R, Result2, T), Result is Result2 + 1.

size_region(_, 0, []).
size_region(R, Result, [Row | T]) :- size_region_row(R, ResRow, Row), Result is ResRow + 0.
*/


% Define se os numeros sao unicos com relação a celulas adjacentes
% [[N1-_, N2-_, N3-_],
%  [N4-_, N5-_, N6-_],
%  [N7-_, N8-_, N9-_]]
unico_adjacente([N1-_, N2-R2, N3-R3 | T1], [N4-_, N5-R5, N6-R6 | T2], [N7-_, N8-R8, N9-R9 | T3]) :-
    all_distinct([N1, N2, N4, N5]), % checa canto superior esquerdo
    all_distinct([N3, N2, N5, N6]), % checa canto superior direito
    all_distinct([N7, N4, N5, N8]), % checa canto inferior esquerdo
    all_distinct([N9, N6, N5, N8]), % checa canto inferior direito
    append([N2-R2, N3-R3], T1, TT1),
    append([N5-R5, N6-R6], T2, TT2),
    append([N8-R8, N9-R9], T3, TT3),
    unico_adjacente(TT1, TT2, TT3).
unico_adjacente([_,_], [_,_], [_,_]).

% Define se os numeros em uma linha sao distintos na sua regiao.
% ?- distintos_row([1-1, 1-2, 4-3, 5-2])
% true; false.
% ?- distintos_row([1-1, 1-2, 4-3, 1-1, 5-2])
% false.
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