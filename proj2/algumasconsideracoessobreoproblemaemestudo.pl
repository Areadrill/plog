:-use_module(library(clpfd)).

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


professoresQueDaoLingua(Lingua, [], []).
professoresQueDaoLingua(Lingua,  [ [Prof, Linguas,_] | OutrosProfs], Lista):-
	member(Lingua-_, Linguas),
	professoresQueDaoLingua(Lingua, OutrosProfs, Lista2),
	append([Prof], Lista2, Lista).

professoresQueDaoLingua(Lingua,  [ [Prof, Linguas,_] | OutrosProfs], Lista):-
	professoresQueDaoLingua(Lingua, OutrosProfs, Lista).


testeProfs(Sol):-
	professoresQueDaoLingua(5, [[0, [0-1,1-2],_], [1, [0-2],_], [2, [3-2],_]], Sol).




initialize_days_list([],_,_,_).
initialize_days_list([H|T], N, NProfessores, FdSetDeProfs):-
	H = N-Hora-Prof,
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
forceEmpty([Dia-Horas-Prof|T]):-
	Horas in {0},
	Prof in {-1},
	forceEmpty(T).

excludeEmpty([],_).
excludeEmpty([Lingua-Horarios|Solucoes], Candidaturas):-
	member(Lingua-0, Candidaturas),
	forceEmpty(Horarios),
	excludeEmpty(Solucoes, Candidaturas).

	excludeEmpty([Lingua-Horarios|Solucoes], Candidaturas):-
		excludeEmpty(Solucoes, Candidaturas).

testeExclude:-
	excludeEmpty([0-[]|T], [0-0]).



escola_de_linguas(Professores, Candidaturas, Linguas, Lucro, Solucao):-
	length(Professores, NProfessores),
	length(Candidaturas, NCandid),
	length(Linguas, NLinguas),
	length(Solucao, NLinguas),
	initialize_solution(Solucao, 0, NProfessores, Professores),
	excludeEmpty(Solucao, Candidaturas).



teste(Solucao):-
	escola_de_linguas([[0, [0-19, 1-19, 2-10],[0,1,2,3,4,5,6]], [1, [4-25],[0,1,2]]], [0-15, 1-10, 2-3, 3-10,4-0], [0-1,1-1,2-2,3-3,4-4], _, Solucao).
%	escola_de_linguas([[0, [0-10],[0,1]]], [0-0], [0-20], _, Solucao).
