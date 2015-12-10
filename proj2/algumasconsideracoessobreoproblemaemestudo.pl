:-use_module(library(clpfd)).
:-use_module(library(aggregate)).
:-use_module(library(lists)).



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
	% 5 - em cada dia nao pode haver mais do que x aulas

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

labelAll([], Cost, Flat):-write('gona label'),nl, labeling([maximize(Cost)], Flat).
labelAll([_-[Dia1,Dia2,Dia3,Dia4,Dia5]|Tail], Cost, Flat):-
	write('labeling'),nl,
	append(Flat, Dia1, Flat1),
	append(Flat1, Dia2, Flat2),
	append(Flat2, Dia3, Flat3),
	append(Flat3, Dia4, Flat4),
	append(Flat4, Dia5, Flat5),
	labelAll(Tail, Cost, Flat5).

restrict_teacher_hours(_, []).
restrict_teacher_hours(Solucao, [Professor|T]):-

	
generate_domain_day_list([],[],[]).
generate_domain_day_list([[_,Hora,Prof]|Dias], H, P):-
	generate_domain_day_list(Dias, HRest, PRest),
	append([Hora], HRest, H),
	append([Prof], PRest, P).
	
generate_variable_domain_list([],[],[]).
generate_variable_domain_list([_-Dias|Solucoes], H, P):-
	generate_domain_day_list(Dias, HThis, PThis),
	generate_variable_domain_list(Solucoes, HRest, PRest),
	append(HThis, HRest, H),
	append(PThis, PRest, P).	
	
escola_de_linguas(Professores, Candidaturas, Linguas, Lucro, Solucao):-
	length(Professores, NProfessores),
	length(Linguas, NLinguas),
	length(Solucao, NLinguas),
	cria_tabela_custos(Professores, TabelaCustos, []),
	initialize_solution(Solucao, 0, NProfessores, Professores),
	generate_variable_domain_list(Solucao, H, P),write(P), nl , write(H), nl,
	%restrict_teacher_hours(Solucao, Professores),
	restrict_course_times(Solucao),
	make_profit(Solucao, Linguas, Professores, TabelaCustos, Candidaturas, Lucro),
	write(Lucro),
	labelAll(Solucao, Lucro).
	
	
make_prof_table(Indice, Lista, NProfs):-
	Length is NProfs+1,
	length(Lista, Length),
	iterate_prof_table(Lista, NProfs, Indice).

iterate_prof_table(_,-1,_).
iterate_prof_table(Lista, NProfs, NProfs):- !, nth0(NProfs, Lista, [NProfs, 1]), Next is NProfs-1, iterate_prof_table(Lista, Next, NProfs).
iterate_prof_table(Lista, NProfs, Indice):- nth0(NProfs, Lista, [NProfs,0]), Next is NProfs-1, iterate_prof_table(Lista, Next, Indice).
	
	
teste(Lucro, Solucao):-
%escola_de_linguas([[0, [0-19, 1-19, 2-10,3-4,4-5],[0,1,2,3,4,5,6]], [1, [4-25],[0,1,2]]], [0-15, 1-10, 2-3, 3-10,4-1], [0-1,1-1,2-2,3-3,4-4], Lucro, Solucao).
escola_de_linguas([[0, [0-10,1-10],[0,1]], [1, [0-5],[0,1]],[2, [0-5],[0,1]]], [0-10,1-10], [0-20,1-40], Lucro, Solucao).


cria_tabela_custos([], TabelaCustos, TabelaCustos).
cria_tabela_custos([Professor|Professores], TabelaCustos, Acum):-
	Professor = [Indice,Linguas,_],
	cria_tabela_linguas(Linguas, Indice, TabelaLinguas, []),
	append(Acum, TabelaLinguas, NovaTabela),
	cria_tabela_custos(Professores, TabelaCustos, NovaTabela).
	
	
cria_tabela_linguas([], _, T, T).
cria_tabela_linguas([LinguaCusto|LinguasCustos], Indice, TabelaLinguas, Acum):-
	LinguaCusto = IndiceL-Custo,
	append(Acum, [[Indice,IndiceL,Custo]], NovaTabela),
	cria_tabela_linguas(LinguasCustos, Indice, TabelaLinguas, NovaTabela).
	
make_profit([], _, _, _,_, 0).
make_profit([Solucao|Solucoes], Linguas, Professores,TabelaCustos, Candidaturas, Lucro):-
	make_profit_language(Solucao, Linguas, Professores, TabelaCustos, Candidaturas, LucroLingua),
	make_profit(Solucoes, Linguas, Professores, TabelaCustos, Candidaturas, LucroResto),
	Lucro #= LucroResto+LucroLingua.

%Lingua = Indice-[[0, 1,1],[1,2,3],[3,4,5]|...]
make_profit_language(Lingua, Linguas, Professores, TabelaCustos, Candidaturas, LucroLingua):-
	Lingua = IndiceL-Dias,
	make_profit_day(Dias, Linguas, Professores, TabelaCustos, Candidaturas, IndiceL, LucroLingua).
make_profit_day([], _, _,_, _, _, 0).
make_profit_day([Dia|OutrosDias], Linguas, Professores, TabelaCustos, Candidaturas, IndiceL, LucroDia):-
	Dia = [_, Horas, Professor],
	table([[Professor, IndiceL, Remuneracao]],TabelaCustos),
	Prejuizo #= Remuneracao * Horas,
	member(IndiceL-Preco, Linguas),
	member(IndiceL-NumeroCandidatos, Candidaturas),
	Lucro #= Preco*NumeroCandidatos*Horas,
	LucroDiaAtual #= (Lucro-Prejuizo),
	make_profit_day(OutrosDias, Linguas, Professores,TabelaCustos, Candidaturas, IndiceL, LucroRest),
	LucroDia #= LucroRest + LucroDiaAtual.


%   0-[[0,_662339,_662259],[1,_662647,_662567],[2,_662955,_662875],[3,_663263,_663183],[4,_663571,_663491]]
%
