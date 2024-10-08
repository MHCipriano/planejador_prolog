# planejador_prolog

Trabalho Prático 1 da Disciplina:
"Inteligência Artificial" - 2024/2
* Aluno: Marcello Henrique Soares Cipriano

Foram implementados dois planejadores:
* planner_me.pl (means-end planner)
* planner_gr.pl (goal-regression based)

Há ainda 4 arquivos de inicialização dos estados iniciais e finais,
conforme a especificação da questão 3 do trabalho:
* movimento1.pl (item 1);
* movimento2.pl (item 2);
* movimento3.pl (item 3);
* movimento4.pl (item 4).

Para executar os programas, é só chamar na linha de comando do Prolog
o planner que deseja executar e a movimentação que deseja fazer (por exemplo):
* ?- [planner_me].
* ?- [movimento1].

E depois, é só executar o planejador da seguinte forma:
* ?- initialState(State), goalState(Goal), plan(State,Goal,PLan,FinalState).  {para o planner_me.pl}; ou
* ?- initialState(State), goalState(Goal), plan(State,Goal,PLan).             {para o planner_gr.pl}.
   
