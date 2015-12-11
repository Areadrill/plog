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

generate_professor_availability([], []).
generate_professor_availability([[Indice,_,Dias]|Professors], Table):-
	generate_professor_availability(Dias,Indice,TableThis),
	generate_professor_availability(Professors, TableRest),
	append(TableThis, TableRest, TableN),
	append([[-1,0],[-1,1],[-1,2],[-1,3],[-1,4],[-1,5]], TableN, Table).

generate_professor_availability([], _, []).
generate_professor_availability([Dia|Dias],Indice, Table):-
	generate_professor_availability(Dias, Indice, TableRest),
	append([[Indice,Dia]], TableRest, Table).





initialize_days_list([],_,_,_,_).
initialize_days_list([H|T], N, NProfessores, FdSetDeProfs, ProfAvailable):-
	H = [N,Hora,Prof],
	Prof #= -1 #=> Hora #= 0,
	table([[Prof,N]],ProfAvailable),
	Prof in_set FdSetDeProfs ,
	domain([Hora], 0, 4),
	N1 is N+1,
	initialize_days_list(T, N1, NProfessores, FdSetDeProfs, ProfAvailable).




initialize_solution([], _,_,_,_).
initialize_solution([H|T], Lingua, NProfessores, Professores, ProfAvailable):-
	H = Lingua-Lista,
	length(Lista, 5),
	professoresQueDaoLingua(Lingua, Professores, ListaDeProfs),
	list_to_fdset([-1|ListaDeProfs], FdSetDeProfs),
	initialize_days_list(Lista, 0, NProfessores,FdSetDeProfs,ProfAvailable),
	Lingua1 is Lingua+1,
	initialize_solution(T, Lingua1, NProfessores, Professores, ProfAvailable).

	restrict_course_times([]).
	restrict_course_times([_-[[_,H1,_],[_,H2,_],[_,H3,_],[_,H4,_],[_,H5,_]]|T]):-
		sum([H1,H2,H3,H4,H5],#=<,4),
		sum([H1,H2,H3,H4,H5],#>=,2),
		restrict_course_times(T).

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

labelAll([], Cost, Flat):-write('gona label'),nl, labeling([ff,enum,down,maximize(Cost)], Flat).
labelAll([_-[Dia1,Dia2,Dia3,Dia4,Dia5]|Tail], Cost, Flat):-
	append(Flat, Dia1, Flat1),
	append(Flat1, Dia2, Flat2),
	append(Flat2, Dia3, Flat3),
	append(Flat3, Dia4, Flat4),
	append(Flat4, Dia5, Flat5),
	labelAll(Tail, Cost, Flat5).

restrict_teacher_hours(_,_,[], _).
restrict_teacher_hours(Horas, ProfessoresDominio, [[Indice,_,_]|T], NProfs):-
	make_prof_table(Indice, Table, NProfs),!,
	restrict_teacher(Table, ProfessoresDominio, Rest),
	sum_product(Horas, Rest, Sum),
	Sum #=< 15,
	restrict_teacher_hours(Horas, ProfessoresDominio, T, NProfs).

restrict_teacher(_, [], []).
restrict_teacher(Table, [-1|Ps],[0|T]):- restrict_teacher(Table, Ps, T).
restrict_teacher(Table, [P|Ps], [Rest|T]):-
	table([[P,Rest]], Table),
	restrict_teacher(Table, Ps, T).


sum_product([],[],0).
sum_product([X|Xs],[Y|Ys], Sum):-
	sum_product(Xs,Ys,SumRest),
	Sum #= X*Y + SumRest.







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

custo_nulo([], 0).
custo_nulo(TabelaCustos, NLinguas):-
	NLinguas \= 0,
	write(NLinguas),
	N is NLinguas-1,
	custo_nulo(TabelaCustosR, N),
	append([[-1,N,0]], TabelaCustosR, TabelaCustos).

escola_de_linguas(Professores, Candidaturas, Linguas, Lucro, Solucao):-
	length(Professores, NProfessores),
	length(Linguas, NLinguas),
	length(Solucao, NLinguas),
	generate_professor_availability(Professores, ProfAvailable),
	cria_tabela_custos(Professores, TabelaCustosReais, []),
	write(NLinguas), nl, nl,
	custo_nulo(CustoNulo, NLinguas),
	append(CustoNulo, TabelaCustosReais, TabelaCustos),write(TabelaCustos),
	initialize_solution(Solucao, 0, NProfessores, Professores,ProfAvailable),
	generate_variable_domain_list(Solucao, H, P),
	restrict_teacher_hours(H, P, Professores, NProfessores),
	restrict_course_times(Solucao),
	make_profit(Solucao, Linguas, Professores, TabelaCustos, Candidaturas, Lucro),

	write(Lucro),
	labelAll(Solucao, Lucro),
	fd_statistics.


make_prof_table(Indice, Lista, NProfs):-
	Length is NProfs+1,
	length(Lista, Length),
	iterate_prof_table(Lista, NProfs, Indice).

iterate_prof_table(_,-1,_).
iterate_prof_table(Lista, NProfs, NProfs):- !, nth0(NProfs, Lista, [NProfs, 1]), Next is NProfs-1, iterate_prof_table(Lista, Next, NProfs).
iterate_prof_table(Lista, NProfs, Indice):- nth0(NProfs, Lista, [NProfs,0]), Next is NProfs-1, iterate_prof_table(Lista, Next, Indice).


teste(Lucro, Solucao):-
	statistics(runtime, [T0| _]),
	escola_de_linguas([[0, [0-10,1-10],[0,1]], [1, [0-5,2-1],[0,1]],[2, [0-5],[0,1]]], [0-10,1-10,2-10], [0-20,1-40,2-10], Lucro, Solucao),
	%escola_de_linguas([[0, [0-1, 1-1,2-1],[0,2,3,4,5,6]]], [0-15, 1-10,2-10,3-10], [0-1,1-1,2-1], Lucro, Solucao),
	statistics(runtime, [T1|_]),
	T is T1 - T0,
	format('p/0 took ~3d sec.~n', [T]).


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
