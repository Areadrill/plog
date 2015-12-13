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
	Prof #= -1 #<=> Hora #= 0,
	findall(ProfA, member( [ProfA, N], ProfAvailable), ListaDeProfsA),
	list_to_fdset(ListaDeProfsA, FdSetDisponivel),
	fdset_intersection(FdSetDisponivel, FdSetDeProfs, FdSetFinal),
	Prof in_set FdSetFinal,
	Hora in {0} \/{2},
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
		sum([H1,H2,H3,H4,H5],#\=,1),
		sum([H1,H2,H3,H4,H5],#>=,0),
		restrict_course_times(T).



labelAll(H,P,Cost):- append(H, P, List), labeling([up,bisect,ff,timeout(60000,),maximize(Cost)], List).


restrict_teacher_hours(_,_,[], _).
restrict_teacher_hours(Horas, ProfessoresDominio, [[Indice,_,_]|T], NProfs):-
	make_prof_table(Indice, Table, NProfs),!,
	append(Table, [[-1, 0]], NTable),
	restrict_teacher(NTable, ProfessoresDominio, Rest),
	sum_product(Horas, Rest, Sum),
	Sum #=< 15,
	restrict_teacher_hours(Horas, ProfessoresDominio, T, NProfs).

restrict_teacher(_, [], []).
restrict_teacher(Table, [P|Ps], [Rest|T]):-
	table([[P,Rest]], Table),
	restrict_teacher(Table, Ps, T).


sum_product([],[],0).
sum_product([X|Xs],[Y|Ys], Sum):-
	sum_product(Xs,Ys,SumRest),
	Sum #= X*Y + SumRest.

make_prof_table(Indice, Lista, NProfs):-
	Length is NProfs+1,
	length(Lista, Length),
	iterate_prof_table(Lista, NProfs, Indice).

iterate_prof_table(_,-1,_).
iterate_prof_table(Lista, NProfs, NProfs):- !, nth0(NProfs, Lista, [NProfs, 1]), Next is NProfs-1, iterate_prof_table(Lista, Next, NProfs).
iterate_prof_table(Lista, NProfs, Indice):- nth0(NProfs, Lista, [NProfs,0]), Next is NProfs-1, iterate_prof_table(Lista, Next, Indice).

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
	N is NLinguas-1,
	custo_nulo(TabelaCustosR, N),
	append([[-1,N,0]], TabelaCustosR, TabelaCustos).

escola_de_linguas(Professores, Candidaturas, Linguas, Rooms, Lucro, Solucao):-
	length(Professores, NProfessores),
	length(Linguas, NLinguas),
	length(Solucao, NLinguas),
	generate_professor_availability(Professores, ProfAvailable),
	cria_tabela_custos(Professores, TabelaCustosReais, []),
	custo_nulo(CustoNulo, NLinguas),
	append(CustoNulo, TabelaCustosReais, TabelaCustos),
	initialize_solution(Solucao, 0, NProfessores, Professores,ProfAvailable),
	generate_days_prof_table(Solucao, DayProf),
	restrict_rooms(DayProf,Rooms, NLinguas),
	generate_variable_domain_list(Solucao, H, P),
	restrict_teacher_hours(H, P, Professores, NProfessores),
	restrict_course_times(Solucao),
	make_profit(Solucao, Linguas, Professores, TabelaCustos, Candidaturas, Lucro),
	labelAll(H, P, Lucro),
	printSolution(Solucao),
	 write('O lucro total obtido e de '), write(Lucro),  nl,
	fd_statistics.





teste:-
	statistics(runtime, [T0| _]),
	%escola_de_linguas([[0, [0-0,1-0,3-0,4-0,5-0,6-0],[0,1,2,3,4]], [1, [0-0,2-0],[0,1,2,3,4]],[2, [0-0,3-0,4-0,5-0,6-0],[0,1,2,3,4]],[3, [0-0,1-0,3-0,4-0,5-0,6-0],[0,1,2,3,4]]], [0-10,1-10,2-10,3-10,4-10,5-10,6-10], [0-1,1-1,2-1,3-1,4-1,5-1,6-1], 40,Lucro, Solucao),
	escola_de_linguas([[0, [0-1, 1-1,2-1,3-0,4-0],[0,1,4,5,6]], [1, [0-1, 1-1,2-1],[0,4]], [2, [0-1, 1-1,3-1],[0,3]] ],
	[0-15, 1-10,2-10,3-10,4-10,5-10],
	[0-1,1-1,2-1,3-10,4-10,5-10], 4,_, _),
	statistics(runtime, [T1|_]),
	T is T1 - T0,
	format('O resultado foi obtido em ~3d segundos.~n', [T]).


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

	Inscritos is min(NumeroCandidatos, 15),
	Lucro #= Preco*Inscritos*Horas,

	LucroDiaAtual #= (Lucro-Prejuizo),
	make_profit_day(OutrosDias, Linguas, Professores,TabelaCustos, Candidaturas, IndiceL, LucroRest),
	LucroDia #= LucroRest + LucroDiaAtual.

generate_days_prof_table([], [[],[],[],[],[]]).
generate_days_prof_table([_-[[_,_,Prof1],[_,_,Prof2],[_,_,Prof3],[_,_,Prof4],[_,_,Prof5]]|Solucoes], [Dia1, Dia2, Dia3, Dia4, Dia5]):-
	generate_days_prof_table(Solucoes, [Dia1R, Dia2R, Dia3R, Dia4R, Dia5R]),
	append([Prof1], Dia1R, Dia1),
	append([Prof2], Dia2R, Dia2),
	append([Prof3], Dia3R, Dia3),
	append([Prof4], Dia4R, Dia4),
	append([Prof5], Dia5R, Dia5).

restrict_rooms([],_,_).
restrict_rooms([Dia|Dias], Rooms, NLinguas):-
	AulasVazias is NLinguas - Rooms,
	count(-1, Dia, #>=, AulasVazias),
	restrict_rooms(Dias, Rooms, NLinguas).


printSolution([]).
printSolution([Indice-Dias|Solutions]):-
	nl, write('Language '), write(Indice), write(' Time Table'),nl, write('_______________'),nl,
	printLanguage(Indice, Dias),
	printSolution(Solutions).

dia(0, 'Monday').
dia(1, 'Tuesday').
dia(2, 'Wednesday').
dia(3, 'Thursday').
dia(4, 'Friday').

printLanguage(_, []).
printLanguage(_, [[_,0,-1]|Dias]):-printLanguage(Indice, Dias).
printLanguage(Indice, [ [Dia,Horas,Prof]|Dias]):-
	dia(Dia, DiaDaSemana),
	write(DiaDaSemana), write(' - '), write(Horas), write(' hours with professor '), write(Prof), nl,
	printLanguage(Indice, Dias).

%   0-[[0,_662339,_662259],[1,_662647,_662567],[2,_662955,_662875],[3,_663263,_663183],[4,_663571,_663491]]
%
