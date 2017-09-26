%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Contraction Clustering (RASTER):
% Reference Implementation in Erlang with an Example
% (c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics
%
% Algorithm development and implementation:
% Gregor Ulm (gregor.ulm@fcc.chalmers.se)
%
% Requirements:
% . Erlang
% . external libraries: numpy, pandas
%
%
% This demo has been developed and tested on Ubuntu Linux 16.04.
%
% For a description of the algorithm including relevant theory, please
% consult our paper on Contraction Clustering (RASTER).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(raster).
-compile([export_all]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation note:
%
% The code below uses 'ordsets' instead of 'sets'. The latter is faster,
% yet cannot be easily inspected in the REPL, which outputs the internal
% representation.
%
% A simple solution would be to simply find/replace all instances of
% 'ordsets' with 'sets' in the source code.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


allPoints([]    , Acc, _     ) -> Acc;
allPoints(Points, Acc, Scalar) ->

    [ {X, Y} | T ] = Points,

    % scale X, Y
    X_ = trunc(X * Scalar),
    Y_ = trunc(Y * Scalar),

    case maps:find({X_, Y_}, Acc) of

    {ok, Value} -> New_Acc = maps:put({X_, Y_}, Value + 1, Acc );
    error       -> New_Acc = maps:put({X_, Y_}, 1, Acc )
    end,

    allPoints(T, New_Acc, Scalar).



mapToTiles(Points, Precision, Threshold) ->

    Scalar     = math:pow(10, Precision),
    All_Points = allPoints(Points, maps:new(), Scalar),

    % retain tiles that contain at least the provided threshold value of
    % observations
    Significant_Tiles =
        maps:keys(
            maps:filter(
                fun(_K, V) -> V >= Threshold end,
                All_Points)),

    {Significant_Tiles, Scalar}.



getNeighbors({X, Y}, Tiles) ->
    % neighbor lookup in O(1)

    % 8-way clustering
    Neighbors  = [{X + 1, Y    },
                  {X - 1, Y    },
                  {X    , Y + 1},
                  {X    , Y - 1},
                  {X + 1, Y - 1},
                  {X + 1, Y + 1},
                  {X - 1, Y - 1},
                  {X - 1, Y + 1}],

    Set_Tiles = ordsets:from_list(Tiles),
    Pred      = fun(X_) -> ordsets:is_element(X_, Set_Tiles) end,
    ordsets:filter(Pred, Neighbors).



% Clusters: list of lists, where each list represents a cluster
cluster_all([]     , _     , Clusters) -> Clusters;
cluster_all(Tiles, Min_Size, Clusters) ->

    [ Start | _T ] = Tiles,
    Cluster        = cluster_one([Start], Tiles, ordsets:new()),
    % remove all points from set
    New_Tiles      = ordsets:subtract(Tiles, Cluster),
    Set_Cluster    = ordsets:to_list(Cluster),

    case length(Set_Cluster) >= Min_Size of

    true  -> cluster_all(New_Tiles, Min_Size, [Set_Cluster | Clusters]);
    false -> cluster_all(New_Tiles, Min_Size, Clusters)
    end.



cluster_one([]      , _    , Visited) -> Visited;
cluster_one(To_Check, Tiles, Visited) ->

    [H | T]     = To_Check,
    New_Visited = ordsets:add_element(H, Visited),
    Candidates  = getNeighbors(H, Tiles),

    Vals        =
        lists:filter(
            fun(X) -> not ordsets:is_element(X, New_Visited) end,
            Candidates),

    cluster_one(T ++ Vals, Tiles, New_Visited).



get_tuples([]     , Acc) -> Acc;
get_tuples([H | T], Acc) ->
    % input e.g.
    % <<"13.55103746471259,9.811559258820193">>

    [X, Y]           = binary:split(H, [<<",">>], [global]),
    {X_Float, _Rest} = string:to_float(binary_to_list(X)),
    {Y_Float, _Rest} = string:to_float(binary_to_list(Y)),

    get_tuples(T, [ {X_Float, Y_Float} | Acc]).



number_clusters([]      , Acc, _  ) -> Acc;
number_clusters([C | Cs], Acc, Num) ->

    A = lists:map(fun({X, Y}) -> {Num, X, Y} end, C),

    number_clusters(Cs, Acc ++ A, Num + 1).



demo() ->

    File_in  = "input/sample.csv",
    File_out = "output/clustered.csv",

    {ok, Data} = file:read_file(File_in),
    All_Points = get_tuples(
                    binary:split(Data, [<<"\n">>], [global]), []),

    % Step 1: Projection
    Threshold = 5,
    Precision = 1, % i.e. 1 place value after the decimal point
    {Significant_Tiles, Scalar} =
        mapToTiles(All_Points, Precision, Threshold),

    % Step 2: Agglomeration
    Min_size              = 5,
    Set_Significant_Tiles = ordsets:from_list(Significant_Tiles),
    Clusters              = cluster_all(
                                Set_Significant_Tiles, Min_size, []),

    io:format("Number of clusters: ~p ~n", [length(Clusters)]),

    Body   = number_clusters(Clusters, [], 1),
    Header = {"Cluster Number", "X-Position", "Y-Position"},

    Body_Scaled = lists:map(
        fun({X, Y, Z}) -> { X, Y / Scalar, Z / Scalar } end, Body),


    % write to file
    {ok, F} = file:open(File_out, write),
    lists:foreach(
        fun({X, Y, Z}) -> io:format(F, "~p, ~p, ~p~n", [X, Y, Z]) end,
        [ Header | Body_Scaled ]),
    file:close(F).

