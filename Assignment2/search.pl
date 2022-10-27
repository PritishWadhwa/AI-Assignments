:- use_module(library(csv)).
:- dynamic(goal/1).
:- dynamic(road/23).
:- dynamic(graph/3).

start :-
    readRoadData,
    write('Search system from place A to place B'), nl,
    write('Enter the start place: '), read(Start),
    write('Enter the goal place: '), read(Goal),
    assert(goal(Goal)),
    solve(Start, Solution),
    % write(Solution),
    % reverseList(Solution, Sol),
    reverse(Solution, Sol),
    % write(Sol),
    printList(Sol),
    retract(goal(Goal)). 

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

solve(Start, Solution) :-
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

% graph(a, b, 1).
% graph(a, c, 2).
% graph(a, d, 3).
% graph(b, e, 8).
% graph(c, e, 7).
% graph(c, f, 5).
% graph(d, f, 4).
% graph(e, f, 6).
% graph(b, a, 1).
% graph(c, a, 2).
% graph(d, a, 3).
% graph(e, b, 8).
% graph(e, c, 7).
% graph(f, c, 5).
% graph(f, d, 4).
% graph(f, e, 6).
