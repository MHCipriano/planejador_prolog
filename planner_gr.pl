%--------------------------------------------------------------------------
% Código em PROLOG conforme modelo do livro do Bratko adaptado pelo aluno
% Marcello Henrique Soares Cipriano conforme especificação feita para o
% Trabalho Prático da disciplina  "Inteligência Artificial"- período 2024/2.
%
%                  Planejamento para "Blocks World"
%                       de diferentes tamanhos
%--------------------------------------------------------------------------

% "X" é um lugar na malha espacial das posições (de 1 a 24 posições, numerado de baixo pra cima e da esquerda para direita).
place(X) :- between(1,24,X).

% Definição do tamanho dos blocos
block(a).
block(b).
block(c).
block(d).

size(X,S) :-
    block(X), X = a, S is 1,!;
	block(X), X = b, S is 1,!;
	block(X), X = c, S is 2,!;
	block(X), X = d, S is 3,!.

on(Block,Space) :- 
    size(Block,3), Space = [_,_,_];
    size(Block,2), Space = [_,_];
    size(Block,1), Space = [_].

% Procedure para efetuar ações condicionais
if(Condition,Then,_) :- Condition, !, Then.
if(_,_,Else) :- Else.

% Verificar se certo espaço está ocupado e por quem
occupationBy(Block,X) :- on(Block,E), member(X,E),!.

% Auxiliar da função "can" para encontrar a base para onde vai ser movimentado o bloco
searchBase(Block, Pos, Base) :-
    size(Block,1), Pos = [A], Base = [Ab], Ab is A - 6,!;
	size(Block,2), Pos = [A,B], Base = [Ab,Bb], Ab is A - 6, Bb is B - 6,!;
	size(Block,3), Pos = [A,B,C], Base = [Ab,Bb,Cb], Ab is A - 6, Bb is B - 6, Cb is C - 6.

% Auxiliar da função "can" para conferir o tamanho da base para onde vai ser movimentado o bloco
countBases([],0).
countBases([H|T], Count) :-
    countBases(T,CountSmallerBase),
    if((H =< 0; occupationBy(_,H)), Count is CountSmallerBase + 1, Count is CountSmallerBase).

teto(Space,Teto) :- Teto is Space + 6.


%----------------------------------------------

% can(Action, Condition), where action and gosls are uninstantiated
can(move(Block3,[Oa,Ob,Oc],[Da,Db,Dc]),
         [clear(Ta), clear(Tb), clear(Tc), clear(Da), clear(Db), clear(Dc),
          on(Block3,[Oa,Ob,Oc]),different([Oa,Ob,Oc],[Da,Db,Dc])]) :-
                size(Block3,3), teto(Oa,Ta), teto(Ob,Tb), teto(Oc,Tc).

can(move(Block2,[Oa,Ob],[Da,Db]),
         [clear(Ta), clear(Tb), clear(Da), clear(Db),
          on(Block2,[Oa,Ob]),different([Oa,Ob],[Da,Db])]) :-
                size(Block2,2), teto(Oa,Ta), teto(Ob,Tb).


can(move(Block,[Oa],[Da]),
         [clear(Ta), clear(Da), on(Block,[Oa]),different([Oa],[Da])]) :-
                size(Block,1), teto(Oa,Ta).

%--------------------------------------------

%----------------------------------------------------------
% adds(Action, Relationships): Action establishes new Relationships
% adds(move(Block,From,To),[on(Block,To),clear(From)]). 
adds(move(Block3,[Oa,Ob,Oc],To),[on(Block3,To),clear(Oa),clear(Ob),clear(Oc)]) :- size(Block3,3).
adds(move(Block2,[Oa,Ob],To),[on(Block2,To),clear(Oa),clear(Ob)]) :- size(Block2, 2).
adds(move(Block,[Oa],To),[on(Block,To),clear(Oa)]) :- size(Block, 1).
%----------------------------------------------------------------

%----------------------------------------------------------
% deletes(Action, Relationships): Action destroys Relationships
% deletes(move(Block,From,To),[on(Block,From),clear(To)]).
deletes(move(Block3,From,[Da,Db,Dc]),[on(Block3,From),clear(Da),clear(Db),clear(Dc)]) :- size(Block3,3).
deletes(move(Block2,From,[Da,Db]),[on(Block2,From),clear(Da),clear(Db)]) :- size(Block2,2).
deletes(move(Block,From,[Da]),[on(Block,From),clear(Da)]) :- size(Block,1).
%--------------------------------------------------------------
%----------------------------------------------------------------

impossible(on(X,Y),Goals):-
    member(on(X,Y1),Goals), Y1 \== Y   % Block cannot be in two places
    ;
    member(on(X1,Y), Goals), X1 \== X. % Two blocks cannot be at the same place
%-----------------------------------------------------------


%----------------------------------------------------------
%   Figure  17.6 (4th) Edition   
%   A planner based on goal regression. 
%   This planner searches in iterative-deepening style.

%   A means-ends planner with goal regression
%   plan( State, Goals, Plan)
plan( State, Goals, []):-
  satisfied( State, Goals).                   % Goals true in State

plan( State, Goals, Plan):-
  append( PrePlan, [Action], Plan),           % Divide plan achieving breadth-first effect
  select( State, Goals, Goal),                % Select a goal
  achieves( Action, Goal),
  can( Action, Condition),                    % Ensure Action contains no variables
  preserves( Action, Goals),                  % Protect Goals
  regress( Goals, Action, RegressedGoals),    % Regress Goals through Action
  plan( State, RegressedGoals, PrePlan).
%------------------------------------------------------------

%-------------------------------------------
%satisfied( State, Goals)  :-
%  delete_all( Goals, State, []).              % All Goals in State
% --------------------------------------------
% Suggestion from  page 400, 4th edition
satisfied(State,[Goal | Goals]):-
    holds(Goal),
    satisfied(State,Goals).

holds(different(X,Y)):-
    \+ X = Y, !.
holds(different(X,Y)):-
    X == Y,
    false.
%------------------------------------------------

% --------------------------------------------
select( State, Goals, Goal)  :-               % Select Goal from Goals
  member( Goal, Goals).                       % A simple selection principle

% --------------------------------------------
achieves( Action, Goal)  :-
  adds( Action, Goals),
  member( Goal, Goals).
%--------------------------------------------

% --------------------------------------------
preserves( Action, Goals)  :-                 % Action does not destroy Goals
  deletes( Action, Relations),
  \+  (member( Goal, Relations),              % not member
       member( Goal, Goals) ).
%---------------------------------------------

% --------------------------------------------
regress( Goals, Action, RegressedGoals)  :-       % Regress Goals through Action
  adds( Action, NewRelations),
  delete_all( Goals, NewRelations, RestGoals),
  can( Action, Condition),
  addnew( Condition, RestGoals, RegressedGoals).  % Add precond., check imposs.
%-----------------------------------------------

% --------------------------------------------
% addnew( NewGoals, OldGoals, AllGoals):
%   OldGoals is the union of NewGoals and OldGoals
%   NewGoals and OldGoals must be compatible
addnew( [], L, L).

addnew( [Goal | _], Goals, _)  :-
  impossible( Goal, Goals),         % Goal incompatible with Goals
  !, 
  fail.                             % Cannot be added

addnew( [X | L1], L2, L3)  :-
  member( X, L2),  !,               % Ignore duplicate
  addnew( L1, L2, L3).

addnew( [X | L1], L2, [X | L3])  :-
  addnew( L1, L2, L3).
%-------------------------------------------------

% --------------------------------------------
% delete_all( L1, L2, Diff): Diff is set-difference of lists L1 and L2
delete_all( [], _, []).

delete_all( [X | L1], L2, Diff)  :-
  member( X, L2), !,
  delete_all( L1, L2, Diff).

delete_all( [X | L1], L2, [X | Diff])  :-
  delete_all( L1, L2, Diff).
%----------------------------------------------

