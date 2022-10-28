:- use_module(library(csv)).
:- dynamic(goal/1).
:- dynamic(graph/3).
:- dynamic(heuristics/3).

start :-
    retractall(graph(_,_,_)),
    retractall(heuristics(_,_,_)),
    readHeuristics,
    readRoadData,
    write('Search system from place A to place B'), nl,
    write('Enter the start place: '), read(Start),
    write('Enter the goal place: '), read(Goal),
    write('Enter your choice of Algorithm (dfs(depth-first)/bfs(best-first)): '), read(Algorithm),
    solve(Start, Goal, Algorithm, Solution),
    reverse(Solution, Sol),
    printList(Sol), nl,
    getCost(Sol, Cost),
    write('Total Lenght: '), write(Cost), write(' kms').

solve(Start, Goal, dfs, Solution) :-
    assert(goal(Goal)),
    solveDFS(Start, Solution),
    retract(goal(Goal)).

solve(Start, Goal, bfs, Solution) :-
    solveBestFirst(Start, Goal, Solution).

solve(_, _, _, _) :-
    write('Invalid Algorithm! Try again.'), nl, 
    start, 
    !.

getCost([_], 0) :- !.

getCost([H|T], Cost) :-
    T = [HT|_],
    graph(H, HT, C),
    getCost(T, Cost1),
    number(C),
    Cost is Cost1 + C.

readHeuristics :- 
    csv_read_file(
        './data/heuristics.csv', 
        Rows, 
        [
            functor(heuristics), 
            arity(3),
            skip_header('City'), 
            strip(true), 
            case(down)
        ]
    ),
    maplist(assert, Rows).

solveBestFirst(Start, Goal, Solution) :-
    bestFirst([[Start]], Goal, Solution, _).

bestFirst([[Goal|Path]|_], Goal, [Goal|Path], 0) :- !.

bestFirst([Path|Queue], Goal, FinalPath, N) :-
    % extend(Path, NewPaths), 
    Path = [H|T],
    findall(
        [X, H|T],
        (
            graph(H, X, _),
            \+ member(X, T)
        ),
        NewVars
    ),
    append(Queue, NewVars, FinalQueue),
    sortQueueByHeuristicValue(FinalQueue, NewQueue, Goal), 
    bestFirst(NewQueue, Goal, FinalPath, M),
    N is M+1.

extend([Node|Path], NewPaths) :-
    findall(
        [NewNode, Node|Path],
        (
            graph(Node,NewNode,_), 
            \+ member(NewNode,Path)
        ),
        NewPaths
    ).

sortQueueByHeuristicValue(Initial, Final, Target) :-
    swapBasedOnHeuristicValue(Initial, Swapped, Target),
    !,
    sortQueueByHeuristicValue(Swapped, Final, Target).

sortQueueByHeuristicValue(Initial, Initial, _) :- !.

% sort_queue1(L,L2, Goal) :-
%     swap1(L,L1, Goal), !,
%     sort_queue1(L1,L2, Goal).

% sort_queue1(L,L, _).

swapBasedOnHeuristicValue(Initial, Swapped, Target) :-
    Initial = [[H1|T1], [H2|T2]|T],
    Swapped = [[H2|T2], [H1|T1]|T],
    heuristics(H1, Target, H1Value),
    heuristics(H2, Target, H2Value),
    number(H1Value),
    number(H2Value),
    H1Value > H2Value.

swapBasedOnHeuristicValue([H|T1], [H|T2], Target) :-
    swapBasedOnHeuristicValue(T1, T2, Target).

% swap1([[A1|B1], [A2|B2]|T], [[A2|B2], [A1|B1]|T], Goal) :-
%     heuristics(A1, Goal, W1),
%     heuristics(A2, Goal, W2),
%     number(W1),
%     number(W2),
%     W1 > W2.

% swap1([X|T],[X|V], Goal) :-
%     swap1(T,V, Goal).

% hh(State, Value) :- 
%     heuristics(State,_,Value),
%     number(Value), !.

% hh(State, Value) :- 
%    write('Incorrect heuristic functionh: '),
%    write(heuristics(State,_,Value)), nl,
%    abort.

readRoadData :-
    csv_read_file(
        './data/dist1.csv', 
        Data, 
        [
            functor(road), 
            skip_header('Road'), 
            strip(true), 
            case(down)
        ]
    ),
    generateGraph(Data).
    

generateGraph(Data) :-
    Data = [Header | Rows],
    forall(
        member(Row, Rows), 
        (
            addEdge(Header, Row)
        )
    ).

addEdge(Header, Row):-
    arg(1, Row, RowName),
    functor(Header, _, Arity),
    forall(
        between(2, Arity, ArgIndex), 
        (
            arg(ArgIndex, Header, ColumnName), 
            arg(ArgIndex, Row, Value), 
            (
                Value = '-' -> ! ;
                (
                    assert(graph(RowName, ColumnName, Value)),
                    assert(graph(ColumnName, RowName, Value))
                )    
            )
        )
    ).

solveDFS(Start, Solution) :-
    dfs([], Start, Solution).

dfs(Path, Node, [Node|Path]) :-
    goal(Node).

dfs(Path, Node, Sol) :-
    graph(Node, Node1, _),
    not(member(Node1, Path)),
    dfs([Node|Path], Node1, Sol).

reverseList([Head | Tail], L) :-
    reverseList(Tail, L),
    L = [Head | L].

reverseList([], _) :- !.

printList([Head | Tail]) :- 
    length(Tail, 0) -> 
        write(Head); 
        (
            write(Head), 
            write(' -> '), 
            printList(Tail)
        ).

% printListCost([Head | Tail], Cost) :- 
%     length(Tail, 0) -> 
%         (
%             write(Head)
%         ); 
%         (
%             write(Head), 
%             Tail = [HeadTail | _],
%             graph(Head, HeadTail, Dist),
%             write(' -> '), 
%             number(Dist),
%             write(Dist),
            
%             printListCost(Tail, Cost),
%             Cost1 = [Dist | Cost]
%         ).

printList([Head]) :-
    writeln(Head).

% printListCost([], Cost) :- 
%     !.

% printListCost([Head], Cost) :-
%     writeln(Head).

printList([]) :- !. 

% graph(a, b, 1).
% graph(a, c, 2).
% graph(a, d, 10).
% graph(b, e, 8).
% graph(c, e, 7).
% graph(c, f, 5).
% graph(d, f, 4).
% graph(e, f, 6).
% graph(b, a, 1).
% graph(c, a, 2).
% graph(d, a, 10).
% graph(e, b, 8).
% graph(e, c, 7).
% graph(f, c, 5).
% graph(f, d, 4).
% graph(f, e, 6).
