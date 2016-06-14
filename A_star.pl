%Wywo¸anie
%start_A_star(gdansk,PathCost,3,20).

succ(gdansk, move, 170, olsztyn).
succ(gdansk, move, 377, warszawa).
succ(gdansk, move, 165, bydgoszcz).
succ(bydgoszcz, move, 227, lodz).
succ(bydgoszcz, move, 270, warszawa).
succ(warszawa, move, 137, lodz).
succ(warszawa, move, 212, olsztyn).
succ(warszawa, move, 193, bialystok).
succ(warszawa, move, 178, kielce).
succ(kielce, move, 116, krakow).
succ(lodz, move, 193, katowice).
succ(katowice, move, 80, kielce).

hScore(gdansk, 546).
hScore(warszawa, 282).
hScore(bydgoszcz, 408).
hScore(olsztyn, 800).
hScore(bialystok, 900).
hScore(kielce, 115).
hScore(lodz, 217).
hScore(katowice, 75).
hScore(krakow, 0).

goal(krakow).

start_A_star(InitState, PathCost, N, MaxStepLimit):-
	score(InitState, 0, 0, InitCost, InitScore),
	search_A_star([node(InitState, nil, nil, InitCost , InitScore )], [], PathCost, N, 1, MaxStepLimit).

search_A_star(Queue, ClosedSet, PathCost, N, StepCounter, MaxStepLimit):-
	StepCounter < MaxStepLimit, ! ,
	write("Numer kroku: "),
	write(StepCounter), nl,
	fetch_new(Node, Queue, ClosedSet, RestQueue, N),
	NewStepCounter is StepCounter + 1,
	continue(Node, RestQueue, ClosedSet, PathCost, N, NewStepCounter, MaxStepLimit).

search_A_star(Queue, ClosedSet, PathCost, N, StepCounter, MaxStepLimit):-
	write("Numer kroku: "),
	write(StepCounter), nl,
	output_nodes(Queue, N, ClosedSet, _),
	write('Przekroczono limit krok—w. Zwi«kszy limit? (t/n)'), nl,
	read('t'),
	NewLimit is MaxStepLimit + 1,
	fetch_new(Node, Queue, ClosedSet, RestQueue, N),
	NewStepCounter is StepCounter + 1,
	continue(Node, RestQueue, ClosedSet, PathCost, N, NewStepCounter, NewLimit).

continue(node(State, Action, Parent, Cost, _), _, ClosedSet, path_cost(Path, Cost), _, _, _):-
	goal(State),!,
	build_path(node(Parent, _, _, _, _), ClosedSet, [Action/State], Path).

continue(Node, RestQueue, ClosedSet, Path, N, StepCounter, MaxStepLimit):-
	expand(Node, NewNodes),
	insert_new_nodes(NewNodes, RestQueue, NewQueue),
	search_A_star(NewQueue, [Node| ClosedSet], Path, N, StepCounter, MaxStepLimit).

fetch(node(State, Action,Parent, Cost, Score), [node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet, RestQueue, _):-
	\+ member(node(State, _ ,_  , _ , _ ) , ClosedSet), ! .

fetch(Node, [ _ |RestQueue], ClosedSet, NewRest, N):-
	fetch(Node, RestQueue, ClosedSet , NewRest, N).

output_nodes(_, 0, _, 0):- ! .

output_nodes([], N, _, N).

output_nodes([X|R], N, ClosedSet, N2):-
	member(X, ClosedSet), ! ,
	output_nodes(R, N, ClosedSet, N2).

output_nodes([X|R], N, ClosedSet, N2):-
	write(X), nl,
	NewN is N - 1,
	output_nodes(R, NewN, ClosedSet, N2).

input_decisions(0, []):- ! .

input_decisions(N, [D|RestDecisions]):-
	read(D),
	NewN is N - 1,
	input_decisions(NewN, RestDecisions).

get_user_decisions(Queue, N, ClosedSet, Decisions):-
	output_nodes(Queue, N, ClosedSet, Diff),
	write('Wybierz indeksy: '), nl,
	NewN is N - Diff,
	input_decisions(NewN, Decisions).

get_index_nondeterministic(X, [X|_]).

get_index_nondeterministic(X, [_|R]):-
	get_index_nondeterministic(X, R).

get_element_at_index(1, X, [X|R], ClosedSet, R):-
	\+ member(X, ClosedSet), ! .

get_element_at_index(Index, Node, [X|R], ClosedSet,[X|RestQueue]):-
	\+ member(X, ClosedSet), ! ,
	NewIndex is Index - 1,
	get_element_at_index(NewIndex, Node, R, ClosedSet, RestQueue).

get_element_at_index(Index, Node, [X|R], ClosedSet, [X|RestQueue]):-
	get_element_at_index(Index, Node, R, ClosedSet, RestQueue).

fetch_new(Node, Queue, ClosedSet, RestQueue, N):-
	get_user_decisions(Queue, N, ClosedSet, Decisions),
	get_index_nondeterministic(Index, Decisions),
	get_element_at_index(Index, Node, Queue, ClosedSet, RestQueue).

expand(node(State, _ ,_ , Cost, _ ), NewNodes):-
	findall(node(ChildState, Action, State, NewCost, ChildScore),
			(succ(State, Action, StepCost, ChildState), score(ChildState, Cost, StepCost, NewCost, ChildScore)),
%			(successor(State, Action, StepCost, ChildState), score(ChildState, Cost, StepCost, NewCost, ChildScore)),
			NewNodes), ! .

score(State, ParentCost, StepCost, Cost, FScore):-
	Cost is ParentCost + StepCost,
	hScore(State, HScore),
	FScore is Cost + HScore.

insert_new_nodes( [ ], Queue, Queue) .

insert_new_nodes( [Node|RestNodes], Queue, NewQueue):-
	insert_p_queue(Node, Queue, Queue1),
	insert_new_nodes( RestNodes, Queue1, NewQueue) .

insert_p_queue(Node,  [ ], [Node] ):- ! .

insert_p_queue(node(State, Action, Parent, Cost, FScore),
		[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
		[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] ):-
	FScore >= FScore1, ! ,
	insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1).

insert_p_queue(node(State, Action, Parent, Cost, FScore), Queue, [node(State, Action, Parent, Cost, FScore)|Queue]).

build_path(node(nil, _, _, _, _ ), _, Path, Path):- ! .

build_path(node(EndState, _ , _ , _, _ ), Nodes, PartialPath, Path):-
	del(Nodes, node(EndState, Action, Parent , _ , _  ) , Nodes1) ,
	build_path(node(Parent,_ ,_ , _ , _ ) , Nodes1, [Action/EndState|PartialPath],Path).

del([X|R],X,R).

del([Y|R],X,[Y|R1]):-
	X\=Y,
	del(R,X,R1).