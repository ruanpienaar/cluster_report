-module(cluster_report).
-export([
    start/0,
    add_node/2,
    remove_node/1,
    cluster_nodes/0,
    cluster_modules/0, cluster_modules/1,
    cluster_modules_extra_info/0, cluster_modules_extra_info/1,
    cluster_applications/0,
% ]).

% -ifdef(TEST).
% -export([
    do_cluster_modules/0,
    do_cluster_modules/1,
    do_cluster_applications/0,
    consistency/1, consistency/2,
    compare_others/3
]).
% -endif.

%%------------------------------------------------------------------------------
%% API

start() ->
    ok = application:start(hawk),
    ok = application:start(cluster_report).

add_node(Node, Cookie) ->
    {ok,_Pid} = hawk:add_node(Node, Cookie).

remove_node(Node) ->
    ok = hawk:remove_node(Node).

% discover_cluster() ->
%     ok.

cluster_nodes() ->
    hawk:nodes().

cluster_modules() ->
    try
        consistency(do_cluster_modules())
    catch
        C:E ->
            {error, C, E, erlang:get_stacktrace()}
    end.

cluster_modules(App) ->
    try
        consistency(do_cluster_modules(App))
    catch
        C:E ->
            {error, C, E, erlang:get_stacktrace()}
    end.

%% XXX: include module info here...
%% Maybe use the crell remove injection stuff
cluster_modules_extra_info() ->
    ok.

cluster_modules_extra_info(_App) ->
    ok.

%% TODO: application description AND version is checked,
%%       we might want to relax the application desc? maybe
cluster_applications() ->
    try
        consistency(do_cluster_applications())
    catch
        C:E ->
            {error, C, E, erlang:get_stacktrace()}
    end.

%%------------------------------------------------------------------------------

do_cluster_modules() ->
    lists:map(fun(Node) ->
        case rpc:call(Node, code, all_loaded, []) of
            Res when is_list(Res) ->
                {Node, lists:sort( [NN || {NN,_} <- Res] )};
            {badrpc,R} ->
                throw({badrpc,R})
        end
    end, cluster_nodes()).

-spec do_cluster_modules(atom()) -> list({node(), nonempty_list()}) | {badrpc, term()}.
do_cluster_modules(App) ->
    lists:map(fun(Node) ->
        case rpc:call(Node, application, get_key, [App, modules]) of
            {ok,Res} when is_list(Res) ->
                {Node, lists:sort(Res)};
            {badrpc,R} ->
                {badrpc,R}
        end
    end, cluster_nodes()).

do_cluster_applications() ->
    lists:map(fun(Node) ->
        case rpc:call(Node, application, which_applications, []) of
            Res when is_list(Res) ->
                {Node, lists:sort(Res)};
            {badrpc,R} ->
                {badrpc,R}
        end
    end, cluster_nodes()).

consistency([{Node, Mods} | T]) ->
    consistency(T, [{Node, Mods}]). %% use the first node

%% pass in a list of [ {Node, [NodeModules]}, ... ]
%% and return the consistent, inconsistent modules
consistency([], R) ->
    R;
consistency([{Node, Mods} | T], R) ->
    consistency(T, compare_others({Node, Mods}, R, R)).

compare_others({Node, Mods}, R, []) ->
    [{Node, Mods} | R];
compare_others({Node, Mods}, R, [{Other, Mods} | _T]) when is_list(Other) ->
    lists:keyreplace(Other, 1, R, {lists:sort([Node | Other]), Mods});
compare_others({Node, Mods}, R, [{Other, Mods} | _T]) when is_atom(Other) ->
    lists:keyreplace(Other, 1, R, {lists:sort([Node | [Other]]), Mods});
compare_others({Node, Mods}, R, [{_, _} | T]) ->
    compare_others({Node, Mods}, R, T).