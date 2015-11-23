:-use_module(library(clpfd)).

%entradas
	% professor - (Nome-[Lista de linguas |_]-[Lista de dias-rendimentoporhora|_])
	% candidatura - (NomeAluno-Lingua)
	% linguas - (Lingua-Preço)

% saidas
	% ([Lingua-dia-professor-carga horaria|TAIL])
	% lucro maninho
	
	
escola_de_linguas():-
	