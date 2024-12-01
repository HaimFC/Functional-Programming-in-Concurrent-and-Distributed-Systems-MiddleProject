%%%-----------------------------------------------------------------------------------------------------------------------
%%% Exm - Middle Project
%%% @ Haim Fellner Cohen
%%%                              
%%% July 2024
%%%-----------------------------------------------------------------------------------------------------------------------
-module(middleProj).

-record(node, {elem, left, right}).
-record(recstruct, {val, sublist}).

-export([exp_to_bdd/3, solve_bdd/2, listOfLeaves/1, reverseIteration/1]).

-import(timer, [now_diff/2]).
-import(lists, [min/1, nth/2]).

%--------------------------------------------------------------------------------------------------------------------------
exp_to_bdd(BoolFunc, Ordering, DataStructureType) ->
    % The Function gets BoolFunc, Ordering, DataStructureType variables and arranges them in a list
    % according to the structure selection it prints the tree.

    TimerStart = erlang:timestamp(),                      % Time measure - for section c
    PermLists = permutations(extract_variables(BoolFunc)),        % Permutations for section ii
    ResTtoL = tup_to_list(BoolFunc),
    ListT = case DataStructureType of
                    record -> [recordReduce(crtRecStruct(L, [], ResTtoL)) || L <- PermLists];
                    map -> [mapReduce(crtMapStruct(L, [], ResTtoL)) || L <- PermLists]     
                end,    
    % Creating the tree phase by Ordering variable
    Result = calculate_result(Ordering, ListT),
    Tend = erlang:timestamp(),      % Stop the timer
    io:format("================================================================~n"),  
    print_res(Ordering, Result),
    io:format("The running time was: ~p microseconds~n", [timer:now_diff(Tend, TimerStart)]),      % Print and calculate the time
    lists:nth(indexMatch(lists:min(Result), Result), ListT).

calculate_result(tree_height, ListT) -> [tree_high_calc(Tree) || Tree <- ListT];
calculate_result(num_of_nodes, ListT) -> [number_of_nodes(Tree) || Tree <- ListT];
calculate_result(num_of_leaves, ListT) -> [number_of_leaves(Tree) || Tree <- ListT].

print_res(tree_height, Result) ->
    io:format("Height of the tree is: ~p.~n", [lists:nth(1, Result)]);
print_res(num_of_nodes, Result) ->
    io:format("Number of nodes in the tree are: ~p.~n", [lists:nth(1, Result)]);
print_res(num_of_leaves, Result) ->
    io:format("Number of leaves in the tree are: ~p.~n", [lists:nth(1, Result)]).

%--------------------------------------------------------------------------------------------------------------------------
% Calculate all permutations of List - copied from lecture presentation.
permutations([]) -> [[]];
permutations(L) -> [[H | T] || H <- L, T <- permutations(L -- [H])].

%--------------------------------------------------------------------------------------------------------------------------
crtRecStruct([H], List, BoolFunc) ->  % Case when T is an empty list - one variable (head)
  #node{elem = H,
        left = reShape(BoolFunc, List ++ [{H, false}]),
        right = reShape(BoolFunc, List ++ [{H, true}])};

crtRecStruct([H | T], List, BoolFunc) ->  % Case when T is not an empty list - long list
  #node{elem = H,
        left = crtRecStruct(T, List ++ [{H, false}], BoolFunc),
        right = crtRecStruct(T, List ++ [{H, true}], BoolFunc)}.

crtMapStruct([H], List, BoolFunc) ->  % Case when T is an empty list - one variable (head)
  #{elem => H,
    left => reShape(BoolFunc, List ++ [{H, false}]),
    right => reShape(BoolFunc, List ++ [{H, true}])};

crtMapStruct([H | T], List, BoolFunc) ->  % Case when T is not an empty list - long list
  #{elem => H,
    left => crtMapStruct(T, List ++ [{H, false}], BoolFunc),
    right => crtMapStruct(T, List ++ [{H, true}], BoolFunc)}.

%--------------------------------------------------------------------------------------------------------------------------
% Reducing the tree for record chosen
recordReduce(Tree) ->
  #node{elem = Elem, left = LT, right = RT} = Tree,
  if
    (is_record(LT, node) =:= false) and (is_record(RT, node) =:= false) ->
      if
        LT =:= RT -> RT;
        true -> Tree  % Stop condition
      end;
    true -> {LeftT, RightT} = {recordReduce(LT), recordReduce(RT)},
      if
        (is_record(LeftT, node) =:= false) and (is_record(RightT, node) =:= false) and (LeftT =:= RightT) -> RightT;
        true -> #node{elem = Elem, left = recordReduce(LT), right = recordReduce(RT)}
      end
  end.

%--------------------------------------------------------------------------------------------------------------------------
% Reducing the tree for map chosen
mapReduce(Tree) ->
  #{elem := Elem, left := LT, right := RT} = Tree,
  if
    (is_map(LT) =:= false) and (is_map(RT) =:= false) ->
      if
        LT =:= RT -> RT;
        true -> Tree  % Stop condition
      end;
    true -> {LeftT, RightT} = {mapReduce(LT), mapReduce(RT)},
      if
        (is_map(LeftT) =:= false) and (is_map(RightT) =:= false) and (LeftT =:= RightT) -> RightT;
        true -> #{elem => Elem, left => mapReduce(LT), right => mapReduce(RT)}
      end
  end.

%--------------------------------------------------------------------------------------------------------------------------
% Counts number of leaves in total at the tree
tree_high_calc(#node{left = LT, right = RT}) ->
    erlang:max(1 + tree_high_calc(LT), 1 + tree_high_calc(RT));
tree_high_calc(Tree = #{left := LT, right := RT}) when is_map(Tree) ->
    erlang:max(1 + tree_high_calc(LT), 1 + tree_high_calc(RT));
tree_high_calc(_) ->
    1.

%--------------------------------------------------------------------------------------------------------------------------
% Counting No. of nodes in the tree, splits between both sides till there is no match
number_of_nodes(#node{left = LT, right = RT}) ->
    1 + number_of_nodes(LT) + number_of_nodes(RT);
number_of_nodes(Tree = #{left := LT, right := RT}) when is_map(Tree) ->
    1 + number_of_nodes(LT) + number_of_nodes(RT);
number_of_nodes(_) ->
    1.

%--------------------------------------------------------------------------------------------------------------------------
% Counting No. of leaves in the tree, splits between both sides till there is no match
number_of_leaves(#node{left = LT, right = RT}) ->
    number_of_leaves(LT) + number_of_leaves(RT);
number_of_leaves(Tree = #{left := LT, right := RT}) when is_map(Tree) ->
    number_of_leaves(LT) + number_of_leaves(RT);
number_of_leaves(_) ->
    1.

%--------------------------------------------------------------------------------------------------------------------------
% Function received tree and values for the function variables and return the result
solve_bdd(T, Arg) ->
    TimerStart = erlang:timestamp(),
    Res = solve(T, Arg),
    TimerEnd = erlang:timestamp(),
    io:format("================================================================~n"),
    io:format("Result is - ~p.~n", [Res]),
    io:format("The running time was: ~p microseconds~n", [timer:now_diff(TimerEnd, TimerStart)]).

% Unified function to solve both map and record trees
solve(Tree, Arg) when is_map(Tree); is_record(Tree, node) ->
    ListBool = convertNumToBool(Arg),
    solve_tree(Tree, ListBool).

% Helper function to navigate through the tree
solve_tree(#{elem := Elem, left := LT, right := RT} = _Tree, ListBool) ->
    {Element, Value} = findPair(ListBool, Elem),
    NewList = lists:delete({Element, Value}, ListBool),
    case Value of
        true ->
            case RT of
                #{elem := _, left := _, right := _} -> solve_tree(RT, NewList);
                _ -> RT
            end;
        false ->
            case LT of
                #{elem := _, left := _, right := _} -> solve_tree(LT, NewList);
                _ -> LT
            end
    end;
solve_tree(#node{elem = Elem, left = LT, right = RT} = _Tree, ListBool) ->
    {Element, Value} = findPair(ListBool, Elem),
    NewList = lists:delete({Element, Value}, ListBool),
    case Value of
        true ->
            case RT of
                #node{} -> solve_tree(RT, NewList);
                _ -> RT
            end;
        false ->
            case LT of
                #node{} -> solve_tree(LT, NewList);
                _ -> LT
            end
    end;
solve_tree(Tree, _) ->
    Tree.

%--------------------------------------------------------------------------------------------------------------------------
% Convert into true and false terms
convertNumToBool([]) -> [];  % Recursion stop condition
convertNumToBool([{Element, 0} | T]) ->
    [{Element, false} | convertNumToBool(T)];
convertNumToBool([{Element, 1} | T]) ->
    [{Element, true} | convertNumToBool(T)];
convertNumToBool([H | T]) ->
    [H | convertNumToBool(T)].

%--------------------------------------------------------------------------------------------------------------------------
% This function returns a list of [true, false, false...] where the terminals are the leaves of the tree.
listOfLeaves(BDD_Tree) ->
    Time_Start = erlang:timestamp(),
    Res = case is_record(BDD_Tree, node) of
        true -> listOfLeaves_record(BDD_Tree);
        false -> listOfLeaves_map(BDD_Tree)
    end,
    Time_End = erlang:timestamp(),
    Diff = timer:now_diff(Time_End, Time_Start),
    io:format("The time it took for list of leaves is ~p microseconds ~n", [Diff]), % Print time difference
    Res.

% Helper function to recursively get leaves from a record-based tree
listOfLeaves_record(BDD_Tree) ->
    case is_atom(BDD_Tree) of
        % Base case: it is a leaf
        true -> [BDD_Tree];
        % Recursive case: continue with the left and right nodes
        false -> listOfLeaves_record(BDD_Tree#node.left) ++ listOfLeaves_record(BDD_Tree#node.right)
    end.

% Helper function to recursively get leaves from a map-based tree
listOfLeaves_map(BDD_Tree) ->
    case is_atom(BDD_Tree) of
        % Base case: it is a leaf
        true -> [BDD_Tree];
        % Recursive case: continue with the left and right nodes
        false -> listOfLeaves_map(maps:get(left, BDD_Tree)) ++ listOfLeaves_map(maps:get(right, BDD_Tree))
    end.

%--------------------------------------------------------------------------------------------------------------------------
% The function takes a pointer to a leaf and returns a list of variables representing the path to the root.

reverseIteration(LeafPtr) ->
    case LeafPtr of
        % Match when LeafPtr is a map
        #{list := List} -> List;
        
        % Match when LeafPtr is a record of type recstruct
        #recstruct{val = _Val, sublist = List} -> List;
        
        % Default case if neither condition is met
        _ -> true
    end.

%--------------------------------------------------------------------------------------------------------------------------
% I don't know why but in this section I didn't succeed with nested tuple pattern matching.
% So I converted the input tuple into list and worked with nested lists pattern matching.
valOfElem([], _Value) ->
    io:fwrite("Argument key not in list"),
    false;  % Return false if the element is not found
valOfElem([{Element, Value} | _T], Element) ->
    Value;
valOfElem([{_Elem, _Value} | T], Element) ->
    valOfElem(T, Element).

reShape(['not', Num], List) when is_list(Num) ->
    not reShape(Num, List);
reShape(['not', Num], List) ->
    not valOfElem(List, Num);

reShape(['or', Arg1, Arg2], List) when is_list(Arg1), is_list(Arg2) ->
    reShape(Arg1, List) or reShape(Arg2, List);
reShape(['or', Arg1, Arg2], List) when is_list(Arg1) ->
    reShape(Arg1, List) or valOfElem(List, Arg2);
reShape(['or', Arg1, Arg2], List) when is_list(Arg2) ->
    valOfElem(List, Arg1) or reShape(Arg2, List);
reShape(['or', Arg1, Arg2], List) ->
    valOfElem(List, Arg1) or valOfElem(List, Arg2);

reShape(['and', Arg1, Arg2], List) when is_list(Arg1), is_list(Arg2) ->
    reShape(Arg1, List) and reShape(Arg2, List);
reShape(['and', Arg1, Arg2], List) when is_list(Arg1) ->
    reShape(Arg1, List) and valOfElem(List, Arg2);
reShape(['and', Arg1, Arg2], List) when is_list(Arg2) ->
    valOfElem(List, Arg1) and reShape(Arg2, List);
reShape(['and', Arg1, Arg2], List) ->
    valOfElem(List, Arg1) and valOfElem(List, Arg2).

%-------------------------------------------------------------------------------------------------------------------------------
% input tuple of Boolean Function and return list of all variables in the tuple
extract_variables(Tuple) ->
    Variables = extract_variables(Tuple, []),
    io:format("Final extracted variables: ~p~n", [Variables]),
    lists:usort(Variables). % Remove duplicates

extract_variables(Var, Acc) when is_atom(Var) ->
    [Var | Acc];
extract_variables({_, Term1}, Acc) ->
    extract_variables(Term1, Acc);
extract_variables({_, Term1, Term2}, Acc) ->
    extract_variables(Term1, extract_variables(Term2, Acc));
extract_variables({_, Term1, Term2, Term3}, Acc) ->
    extract_variables(Term1, extract_variables(Term2, extract_variables(Term3, Acc)));
extract_variables([], Acc) ->
    Acc;
extract_variables([H | T], Acc) ->
    extract_variables(T, extract_variables(H, Acc));
extract_variables(_, Acc) ->
    Acc.

%-------------------------------------------------------------------------------------------------------------------------------
indexMatch(_, [], _) ->
    not_found;
indexMatch(Element, [Element | _], Index) ->
    Index;
indexMatch(Element, [_ | T], Index) ->
    indexMatch(Element, T, Index + 1).

indexMatch(Element, List) ->
    indexMatch(Element, List, 1).

%--------------------------------------------------------------------------------------------------------------------------
% Finding pair
findPair([], _Val) -> io:fwrite("Can't find the variable in the list");
findPair([{X, Y} | _T], X) -> {X, Y};
findPair([_H | T], Elem) -> findPair(T, Elem).

%--------------------------------------------------------------------------------------------------------------------------
% Convert nested tuple into nested list
tup_to_list(Tuple) when is_tuple(Tuple) ->
    List = erlang:tuple_to_list(Tuple),
    [tup_to_list(Elem) || Elem <- List];
tup_to_list(Elem) ->
    Elem.