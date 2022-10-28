:- use_module(library(csv)).
:- dynamic(goal/1).
:- dynamic(road/23).
:- dynamic(graph/3).
:- dynamic(heuristics/3).

start :-
    retractall(graph(_,_,_)),
    retractall(heuristics(_,_,_)),
    readHeuristics,
    % best_first([[a]], e, N, P).
    readRoadData,
    % write('Search system from place A to place B'), nl,
    write('Enter the start place: '), read(Start),
    write('Enter the goal place: '), read(Goal),
    solveBestFirst(Start, Goal, Solution),
    % assert(goal(Goal)),
    % solve(Start, Solution),
    % % write(Solution),
    % % reverseList(Solution, Sol),
    reverse(Solution, Sol),
    % write(Sol),
    printList(Sol).
    % retract(goal(Goal)). 

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
    best_first([[Start]], Goal, Solution, _).

best_first([[Goal|Path]|_], Goal, [Goal|Path], 0) :- !.

best_first([Path|Queue], Goal, FinalPath, N) :-
    extend(Path, NewPaths), 
    append(Queue, NewPaths, Queue1),
    sort_queue1(Queue1, NewQueue, Goal), 
    best_first(NewQueue, Goal, FinalPath, M),
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

sort_queue1(L,L2, Goal) :-
    swap1(L,L1, Goal), !,
    sort_queue1(L1,L2, Goal).

sort_queue1(L,L, Goal).

swap1([[A1|B1], [A2|B2]|T], [[A2|B2], [A1|B1]|T], Goal) :-
    heuristics(A1, Goal, W1),
    heuristics(A2, Goal, W2),
    number(W1),
    number(W2),
    W1 > W2.

swap1([X|T],[X|V], Goal) :-
    swap1(T,V, Goal).

hh(State, Value) :- 
    heuristics(State,_,Value),
    number(Value), !.

hh(State, Value) :- 
   write('Incorrect heuristic functionh: '),
   write(heuristics(State,_,Value)), nl,
   abort.







































    

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

% printList([Head]) :-
    % writeln(Head).

printList([]) :- !.

graph(a, b, 1).
graph(a, c, 2).
graph(a, d, 10).
graph(b, e, 8).
graph(c, e, 7).
graph(c, f, 5).
graph(d, f, 4).
graph(e, f, 6).
graph(b, a, 1).
graph(c, a, 2).
graph(d, a, 10).
graph(e, b, 8).
graph(e, c, 7).
graph(f, c, 5).
graph(f, d, 4).
graph(f, e, 6).
