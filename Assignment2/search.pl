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
    write('Enter the start place: '),
    enterPlace(Start),
    write('Enter the goal place: '),
    enterPlace(Goal),
    write('Enter your choice of Algorithm (dfs(depth-first)/bfs(best-first)): '), read(Algorithm),
    solve(Start, Goal, Algorithm, Solution),
    reverse(Solution, Sol),
    printList(Sol), nl,
    getCost(Sol, Cost),
    write('Total Lenght: '), write(Cost), write(' kms').

enterPlace(Place) :-
    read(Place), 
    graph(Place,_,_) -> true; (write('Place not found, enter new Place: '), enterPlace(Place)).

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

printList([Head]) :-
    writeln(Head).

printList([]) :- !. 