:-use_module(library(clpfd)).
:-use_module(library(aggregate)).



%entradas
	% professor - (indiceP-[Lista de linguas-rendimentoporhora|_]-[Lista de dias|_])
	% candidatura - (Lingua-Numero)
	% linguas - (indiceL-Preço)

% saidas
	% ([lingua|TAIL])
	% lucro
%restriçao
	% 1 - max 15 horas/semana/professor
	% 2 - minimo 2 horas por semana/linguas
	% 3 - maximo 4 horas/semana/lingua
	% 4 - 15 alunos/lingua
	% 5 - em cada dia nao pode haver mais do que x aulas em cada intervalo inicio-fim, para todos os intervalos

clearScreen:-write('\e[2J').


professoresQueDaoLingua(_, [], []).
professoresQueDaoLingua(Lingua,  [ [Prof, Linguas,_] | OutrosProfs], Lista):-
	member(Lingua-_, Linguas),
	professoresQueDaoLingua(Lingua, OutrosProfs, Lista2),
	append([Prof], Lista2, Lista).

professoresQueDaoLingua(Lingua,  [ [_, _,_] | OutrosProfs], Lista):-
	professoresQueDaoLingua(Lingua, OutrosProfs, Lista).


testeProfs(Sol):-
	professoresQueDaoLingua(5, [[0, [0-1,1-2],_], [1, [0-2],_], [2, [3-2],_]], Sol).




initialize_days_list([],_,_,_).
initialize_days_list([H|T], N, NProfessores, FdSetDeProfs):-
	H = [N,Hora,Prof],

	Prof in_set FdSetDeProfs,
	domain([Hora], 0, 4),
	N1 is N+1,
	initialize_days_list(T, N1, NProfessores, FdSetDeProfs).


initialize_solution([], _,_,_).
initialize_solution([H|T], Lingua, NProfessores, Professores):-
	H = Lingua-Lista,
	length(Lista, 5),
	professoresQueDaoLingua(Lingua, Professores, ListaDeProfs),
	list_to_fdset([-1|ListaDeProfs], FdSetDeProfs),
	initialize_days_list(Lista, 0, NProfessores,FdSetDeProfs),
	Lingua1 is Lingua+1,
	initialize_solution(T, Lingua1, NProfessores, Professores).

forceEmpty([]).
forceEmpty([_-Horas-Prof|T]):-
	Horas in {0},
	Prof in {-1},
	forceEmpty(T).

excludeEmpty([],_).
excludeEmpty([Lingua-Horarios|Solucoes], Candidaturas):-
	member(Lingua-0, Candidaturas),
	forceEmpty(Horarios),
	excludeEmpty(Solucoes, Candidaturas).

	excludeEmpty([_-_|Solucoes], Candidaturas):-
		excludeEmpty(Solucoes, Candidaturas).


% [ 0-[ [0, 2, 4], [1, 3, 5] ], 1-[[0,4,5], [1,3,5],[2,6,6] ]  ]

labelAll([],_).
labelAll(Sol, Cost):-
	labelAll(Sol, Cost, []).

labelAll([], Cost, Flat):- labeling(maximize(Cost), Flat).
labelAll([Indice-[Dia1,Dia2,Dia3,Dia4,Dia5]|Tail], Cost, Flat):-
	append(Flat, Dia1, Flat1),
	append(Flat1, Dia2, Flat2),
	append(Flat2, Dia3, Flat3),
	append(Flat3, Dia4, Flat4),
	append(Flat4, Dia5, Flat5),
	labelAll(Tail, Cost, Flat5).

restrict_teacher_hours(_, []).
restrict_teacher_hours(Solucao, [Professor|T]):-
	Professor=[Indice,_,_],
	restrict_teacher(Solucao, Indice, 0),
	restrict_teacher_hours(Solucao, T).

%
restrict_teacher([], _, Acum):- Acum #< 15, write(Acum).
restrict_teacher([H|T], Indice, Acum):-
	H = _-Dias,
	teacher_times(Dias, Indice, Time),
	Acum1 #= Time+Acum,
	restrict_teacher(T, Indice, Acum1).


teacher_times([], _, 0).
teacher_times([[_, Horas, ProfessorResponsavel]|Dias], Indice, Time):-
	ProfessorResponsavel #= Indice #<=> B,
	TempoQueOProfDaEstaAula #= B*Horas,
	teacher_times(Dias, Indice, TempoRestante),
	Time #= TempoQueOProfDaEstaAula+TempoRestante.




escola_de_linguas(Professores, Candidaturas, Linguas, Lucro, Solucao):-
	length(Professores, NProfessores),
	length(Linguas, NLinguas),
	length(Solucao, NLinguas),
	initialize_solution(Solucao, 0, NProfessores, Professores),
	restrict_teacher_hours(Solucao, Professores),
	make_profit(Solucao, Linguas, Professores, Candidaturas, Lucro),
	write(Lucro),
	labelAll(Solucao, Lucro).

teste(Solucao, Lucro):-
%escola_de_linguas([[0, [0-19, 1-19, 2-10],[0,1,2,3,4,5,6]], [1, [4-25],[0,1,2]]], [0-15, 1-10, 2-3, 3-10,4-1], [0-1,1-1,2-2,3-3,4-4], Lucro, Solucao).
escola_de_linguas([[0, [0-10],[0,1]]], [0-10], [0-20], Lucro, Solucao).


falha(N):-N in 0..10.
falha(N).

testinho(_,0,0).
testinho(P, N, Value):-
	N1 is N-1,
	testinho(P, N1, Value1),
	Value #= P+Value1.

make_profit([], _, _,_, 0).
make_profit([Solucao|Solucoes], Linguas, Professores, Candidaturas, Lucro):-
	make_profit_language(Solucao, Linguas, Professores, Candidaturas, LucroLingua),
	make_profit(Solucoes, Linguas, Professores, Candidaturas, LucroResto),
	Lucro #= LucroResto+LucroLingua.

%Lingua = Indice-[[0, 1,1],[1,2,3],[3,4,5]|...]
make_profit_language([], _,_,0).
make_profit_language(Lingua, Linguas, Professores, Candidaturas, LucroLingua):-
	Lingua = IndiceL-Dias,
	make_profit_day(Dias, Linguas, Professores, Candidaturas, IndiceL, LucroLingua).
make_profit_day([],_,_,_,_,0).
make_profit_day([Dia|OutrosDias], Linguas, Professores, Candidaturas, IndiceL, LucroDia):-
	Dia = [_, Horas, Professor],
	member([Professor, LinguasProfessor, _], Professores),
	member(IndiceL-Remuneracao, LinguasProfessor),
	Prejuizo #= Remuneracao * Horas,
	member(IndiceL-Preco, Linguas),
	member(IndiceL-NumeroCandidatos, Candidaturas),
	Lucro #= Preco*NumeroCandidatos*Horas,
	LucroDiaAtual #= (Lucro-Prejuizo),
	make_profit_day(OutrosDias, Linguas, Professores, Candidaturas, IndiceL, LucroRest),
	LucroDia #= LucroRest + LucroDiaAtual.


%   0-[[0,_662339,_662259],[1,_662647,_662567],[2,_662955,_662875],[3,_663263,_663183],[4,_663571,_663491]]
%
