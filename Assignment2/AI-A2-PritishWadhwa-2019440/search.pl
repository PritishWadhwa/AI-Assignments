% Programmer: Pritish Wadhwa 

% importing the required library for reading csv file
:- use_module(library(csv)).

% Declaring the dynamic predicates
:- dynamic(goal/1).
:- dynamic(graph/3).
:- dynamic(heuristics/3).

start :-
    % Retracting the already existing facts
    retractall(graph(_,_,_)),
    retractall(heuristics(_,_,_)),

    % Reading the csv file and storing the facts in the database
    readHeuristics,
    readRoadData,

    % Reading the start and goal nodes from the user
    write('Search system from place A to place B'), nl,
    write('Enter the start place: '),
    enterPlace(Start),
    write('Enter the target place: '),
    enterPlace(Goal),

    % Reading the algorithm to be used from the user
    write('Enter your choice of Algorithm (dfs(depth-first)/bfs(best-first)): '), read(Algorithm),
    
    % solving the problem using the algorithm selected by the user
    solve(Start, Goal, Algorithm, Solution),

    % printing the solution
    reverse(Solution, Sol),
    printList(Sol), nl,

    % calculating the total distance travelled
    getCost(Sol, Cost),
    write('Total Lenght: '), write(Cost), write(' kms').

checkHeuristic :-
    % checking if the heuristics are valid or not
    forall(
        heuristics(X,Y,Z), 
        (
            solve(X,Y,bfs,Solution),
            reverse(Solution, Sol),
            getCost(Sol, Cost),
            (Z < Cost -> true; (write(X), write(' '), write(Y), write(' '), write(Z), write(' '), write(Cost), nl))
        )
    ).


enterPlace(Place) :-
    % reading the place from the user
    read(Place), 
    graph(Place, _, _) -> true; (write('Place not found, enter new Place: '), enterPlace(Place)).


solve(Start, Goal, dfs, Solution) :-
    % solving the problem using dfs
    assert(goal(Goal)),
    solveDFS(Start, Solution),
    retract(goal(Goal)).

solve(Start, Goal, bfs, Solution) :-
    % solving the problem using bfs
    solveBestFirst(Start, Goal, Solution).

solve(_, _, _, _) :-
    % if the algorithm is not valid
    write('Invalid Algorithm! Try again.'), nl, 
    start, 
    !.

getCost([_], 0) :- !.

getCost([H|T], Cost) :-
    % calculating the total distance travelled
    T = [HT|_],
    graph(H, HT, C),
    getCost(T, Cost1),
    number(C),
    Cost is Cost1 + C.

readHeuristics :- 
    % reading the heuristics from the csv file and storing them as facts
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
    maplist(assert, Rows),
    forall(
        heuristics(X,Y,Z), 
        (
            K is Z//4,
            assert(heuristics(X,Y,K)),
            retract(heuristics(X,Y,Z))
        )
    ).


solveBestFirst(Start, Goal, Solution) :-
    % solving the problem using best first search
    bestFirst([[Start]], Goal, Solution, _).

bestFirst([[Goal|Path]|_], Goal, [Goal|Path], 0) :- !.

bestFirst([Path|Queue], Goal, FinalPath, N) :-
    % expanding the node with the least heuristic value
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
    % extending the node with all the possible paths from it 
    findall(
        [NewNode, Node|Path],
        (
            graph(Node,NewNode,_), 
            \+ member(NewNode,Path)
        ),
        NewPaths
    ).

sortQueueByHeuristicValue(Initial, Final, Target) :-
    % sorting the queue by the heuristic value
    swapBasedOnHeuristicValue(Initial, Swapped, Target),
    !,
    sortQueueByHeuristicValue(Swapped, Final, Target).

sortQueueByHeuristicValue(Initial, Initial, _) :- !.

swapBasedOnHeuristicValue(Initial, Swapped, Target) :-
    % swapping the nodes in the queue based on the heuristic value
    Initial = [[H1|T1], [H2|T2]|T],
    Swapped = [[H2|T2], [H1|T1]|T],
    heuristics(H1, Target, H1Value),
    heuristics(H2, Target, H2Value),
    number(H1Value),
    number(H2Value),
    H1Value > H2Value.

swapBasedOnHeuristicValue([H|T1], [H|T2], Target) :-
    % swapping the nodes in the queue based on the heuristic value
    swapBasedOnHeuristicValue(T1, T2, Target).

readRoadData :-
    % reading the road data from the csv file and storing them as facts
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
    % generating the graph from the road data
    Data = [Header | Rows],
    forall(
        member(Row, Rows), 
        (
            addEdge(Header, Row)
        )
    ).

addEdge(Header, Row):-
    % adding the edge to the graph
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
    % solving the problem using dfs
    dfs([], Start, Solution).

dfs(Path, Node, [Node|Path]) :-
    % checking if the goal is reached
    goal(Node).

dfs(Path, Node, Sol) :-
    % expanding the node with all the possible paths from it
    graph(Node, Node1, _),
    not(member(Node1, Path)),
    dfs([Node|Path], Node1, Sol).

reverseList([Head | Tail], L) :-
    % reversing the list
    reverseList(Tail, L),
    L = [Head | L].

reverseList([], _) :- !.

printList([Head | Tail]) :- 
    % printing the list
    length(Tail, 0) -> 
        write(Head); 
        (
            write(Head), 
            write(' -> '), 
            printList(Tail)
        ).

printList([Head]) :-
    % printing the list
    writeln(Head).

printList([]) :- !. 