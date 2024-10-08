%--------------------------------------------------------------------------
% Código em PROLOG conforme modelo do livro do Bratko adaptado pelo aluno
% Marcello Henrique Soares Cipriano conforme especificação feita para o
% Trabalho Prático da disciplina  "Inteligência Artificial"- período 2024/2.
%
%                  Planejamento para "Blocks World"
%                       de diferentes tamanhos
%--------------------------------------------------------------------------

% "X" é um lugar na malha espacial das posições (de 1 a 24 posições,
% numerado de baixo pra cima e da esquerda para direita).
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

%can(move(Block,From,To),[clear(To),on(Block,From)]):-
can(move(Block3,[Oa,Ob,Oc],[Da,Db,Dc]),[clear(Da),clear(Db),clear(Dc),on(Block3,[Oa,Ob,Oc]),clear(Ta),clear(Tb),clear(Tc)]) :-
    
    block(Block3),                                          % There is this Block to move
    size(Block3,3),                                         % Bloco com dimensão 3
    place(Da), place(Db), place(Dc),                        % 'From' is a place
    place(Oa), place(Ob), place(Oc),                        % 'From' is a place
    Oa \== Da, Ob \== Db, Oc \== Dc,                        % 'To' is a new position to move  
    teto(Oa,Ta), teto(Ob,Tb), teto(Oc,Tc),
    %clear(Ta), clear(Tb), clear(Tc),
    O_leftPoint is Oa mod 6, D_leftPoint is Da mod 6, 
    between(1, 4, O_leftPoint), between(1, 4, D_leftPoint), % Condição de encaixe do bloco no espaço maximo de 6 casas 
    Ob is Oa + 1, Oc is Ob + 1,                             % Posições ocupados pelo bloco em sequencia
    Db is Da + 1, Dc is Db + 1,                             % Posições ocupados pelo bloco em sequencia

    searchBase(Block3,[Da,Db,Dc],[A,B,C]),                  
    \+ member(A,Block3), \+ member(B,Block3), \+ member(C,Block3),
    countBases([A,B,C], SizeB),   
    SizeB >= 2.                                             % Bloco on the top can exceed maximum 1 of base
    

can(move(Block2,[Oa,Ob],[Da,Db]),[clear(Da),clear(Db),on(Block2,[Oa,Ob]),clear(Ta),clear(Tb)]) :-    
    
    block(Block2),                                          % There is this Block to move
    size(Block2,2),                                         % Bloco com dimensão 2
    place(Da), place(Db),                                   % 'To' is a place
    place(Oa), place(Ob),                                   % 'From' is a place
    Oa \== Da, Ob \== Db,                                   % 'To' is a new position to move  
    teto(Oa,Ta), teto(Ob,Tb),
    %clear(Ta), clear(Tb),
    O_leftPoint is Oa mod 6, D_leftPoint is Da mod 6, 
    between(1, 5, O_leftPoint), between(1, 5, D_leftPoint), % Condição de encaixe do bloco no espaço maximo de 6 casas 
    Ob is Oa + 1, Db is Da + 1,                             % Posições ocupados pelo bloco em sequencia

    searchBase(Block2,[Da,Db],[A,B]),                  
    \+ member(A,Block2), \+ member(B,Block2),
    countBases([A,B], SizeB),
    SizeB >= 1.                                             % Bloco on the top can exceed maximum 1 of base


can(move(Block,[Oa],[Da]),[clear(Da),on(Block,[Oa]),clear(Ta)]) :-
    
    block(Block),                                       % There is this Block to move
    size(Block,1),                                      % Bloco com dimensão 1
    place(Da), place(Oa),                               % 'To' e 'From' is a place  
    Oa \== Da,                                          % 'To' is a new position to move  
    teto(Oa,Ta),
    searchBase(Block,[Da],[A]),                  
    \+ member(A,Block),
    countBases([A], SizeB),
    SizeB >= 1.                                        % Bloco on the top need a base

%--------------------------------------------------------------------------------------


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


%--------------------------------------------------------------

% plan(State, Goals, Plan, FinalState)
plan(State, Goals, [], State):-
    satisfied(State,Goals).
plan(State, Goals, Plan, FinalState):-
    append(Plan,_,_),
    append(PrePlan,[Action|PostPlan],Plan),
    select(State,Goals,Goal),
    achieves(Action,Goal),
    can(Action,Condition),
    plan(State,Condition,PrePlan,MidState1),
    apply(MidState1,Action,MidState2),
    plan(MidState2,Goals,PostPlan,FinalState),!.

%----------------------------------------------------------
% satisfied(State, goal): Goals are true in State
satisfied(_,[]).
satisfied(State,[Goal|Goals]):-
    member(Goal,State),
    satisfied(State,Goals).                 

%----------------------------------------------------------
select(State, Goals, Goal):-
    member(Goal,Goals),
    \+ member(Goal,State).                   % goal not true


%----------------------------------------------------------
% achieves(Action, Goal): goal is in add-list of Action 
achieves(Action, Goal):-
    adds(Action,Goals),
    member(Goal, Goals).

%----------------------------------------------------------
% apply(State, Action, NewState): execution of Action at 
%                                 State produces NewState
apply(State, Action, NewState):-
    deletes(Action, DelList), % get properties to be deleted
    delete_all(State,DelList, State1), !,
    adds(Action, AddList),
    append(AddList, State1, NewState).

%----------------------------------------------------------
% delete_all(L1,L2,Diff) Diff is set-difference of L1 and L2
delete_all([],_,[]).
delete_all([X|L1],L2,Diff):-
    member(X,L2), !,
    delete_all(L1,L2,Diff).
delete_all([X|L1],L2,[X|Diff]):-
    delete_all(L1,L2,Diff).
%------------------------------------------------------------

