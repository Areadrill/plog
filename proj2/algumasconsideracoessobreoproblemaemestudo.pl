:-use_module(library(clpfd)).

%entradas
	% professor - (indiceP-[Lista de linguas-rendimentoporhora|_]-[Lista de dias|_])
	% candidatura - (indiceC-indiceL)
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
	
escola_de_linguas(Professores, Candidaturas, Linguas, Lucro, Solucao):-
length(Professores, NProfessores),
length(Candidaturas, NCandid),
length(Linguas, NLinguas),
NSolucoes is NProfessores * NCandid * NLinguas,
length(Solucao, NLinguas),
domain(Solucao, 2, 4),



