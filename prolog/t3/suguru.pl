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


%%% Regra que define o que eh um tabuleiro de suguru, de acordo com as regras definidas mais a frente.
suguru(Values, Regions) :-
    maplist(same_length(Values), Values),  
    maplist(same_length(Regions), Regions),  
    length(Values, L),
    append(Values, Vs),
    append(Regions, Rs),
    Max is L*L,
    Vs ins 1..Max,
    append(Chunk1, [_,_], Values),
    append([_|Chunk2], [_], Values),
    append([_,_|Chunk3], [], Values),
    maplist(unique_adjacency, Chunk1, Chunk2, Chunk3),
    regions(Rs, RL),
    maplist(region(Vs, Rs), RL).

    


% Define se os numeros sao unicos com relação às celulas adjacentes
% [[N1, N2, N3],
%  [N4, N5, N6],
%  [N7, N8, N9]]
unique_adjacency([N1, N2, N3 | T1], [N4, N5, N6 | T2], [N7, N8, N9 | T3]) :-
    all_distinct([N1, N2, N4, N5]), % checa canto superior esquerdo
    all_distinct([N3, N2, N5, N6]), % checa canto superior direito
    all_distinct([N7, N4, N5, N8]), % checa canto inferior esquerdo
    all_distinct([N9, N6, N5, N8]), % checa canto inferior direito
    append([N2, N3], T1, TT1),
    append([N5, N6], T2, TT2),
    append([N8, N9], T3, TT3),
    unique_adjacency(TT1, TT2, TT3).
unique_adjacency([_,_], [_,_], [_,_]).


% Define uma regra para valores V pertencentes a regiao Region conforme a lista de regioes Rt.
% Os valores pertencentes a regiao Region sao guardados em At.
values_region(_, [], [], []).
values_region(Region, [V|Vt], [R|Rt], [V|At]) :- R #= Region, values_region(Region, Vt, Rt, At).
values_region(Region, [_|Vt], [R|Rt], At) :- R \= Region, values_region(Region, Vt, Rt, At).

% Define regra que retorna em At a lista de regioes Rt unicas.
regions([], []).
regions([R|Rt], [R|At]) :- \+ memberchk(R, Rt), regions(Rt, At).
regions([R|Rt], At) :- memberchk(R, Rt), regions(Rt, At).

% Define regra para dizer se o valor V esta presente na regiao R
in_region(R, V, Regions, Values) :- values_region(R, Values, Regions, ValuesR), memberchk(V, ValuesR), !.

% Define regra para o tamanho S da regiao R em Rt.
length_region(_, [], 0).
length_region(R, [R|Rt], S) :- length_region(R, Rt, S1), S is S1 + 1, !. 
length_region(Region, [_|Rt], S) :- length_region(Region, Rt, S).


% Regra que define se um valor V e uma regiao R formam uma regiao valida.
region(Values, Regions, R) :- values_region(R, Values, Regions, Result), all_different(Result), length_region(R, Regions, L), maplist(#>=(L), Result).

%% ?- tabuleiro(1, Values, Regions), suguru(Values, Regions), maplist(label, Values).