-module(cluster_report).
-export([
    start/0,
    add_node/2,
    remove_node/1,
    cluster_nodes/0,
    cluster_modules/0, cluster_modules/1,
    cluster_modules_extra_info/0, cluster_modules_extra_info/1,
    cluster_apps/0
]).
%% application:get_key(APP, modules).

%%------------------------------------------------------------------------------
%% API

start() ->
    ok = application:start(hawk),
    ok = application:start(cluster_report),

    add_node('test1@mbp', test),
    add_node('test2@mbp', test),
    add_node('test3@mbp', test),
    add_node('test4@mbp', test).

add_node(Node, Cookie) ->
    {ok,_Pid} = hawk:add_node(Node, Cookie).

remove_node(Node) ->
    ok = hawk:remove_node(Node).

discover_cluster() ->
    ok.

cluster_nodes() ->
    hawk:nodes().

cluster_modules() ->
    try
        cluster_module_consistency(do_cluster_modules())
    catch
        C:E ->
            {error, C, E, erlang:get_stacktrace()}
    end.

cluster_modules(App) ->
    try
        cluster_module_consistency(do_cluster_modules(App))
    catch
        C:E ->
            {error, C, E, erlang:get_stacktrace()}
    end.

%% XXX: include module info here...
%% Maybe use the crell remove injection stuff
cluster_modules_extra_info() ->
    ok.

cluster_modules_extra_info(App) ->
    ok.

%%------------------------------------------------------------------------------

do_cluster_modules() ->
    lists:map(fun(Node) ->
        case rpc:call(Node, code, all_loaded, []) of
            Res when is_list(Res) ->
                {Node, lists:sort(Res)};
            {badrpc,R} ->
                throw({badrpc,R})
        end
    end, cluster_nodes()).

do_cluster_modules(App) ->
    lists:map(fun(Node) ->
        case rpc:call(Node, application, get_key, [App, modules]) of
            {ok,Res} when is_list(Res) ->
                {Node, lists:sort(Res)};
            {badrpc,R} ->
                throw({badrpc,R})
        end
    end, cluster_nodes()).

cluster_apps() ->
    lists:map(fun(Node) ->
        case rpc:call(Node, application, which_applications, []) of
            Res when is_list(Res) ->
                {Node, lists:sort(Res)};
            {badrpc,R} ->
                throw({badrpc,R})
        end
    end, cluster_nodes()).

cluster_module_consistency(L) ->
    cluster_module_consistency(L,[]).

cluster_module_consistency([],R) ->
    R;
cluster_module_consistency([{Node,Mods}|T],[]) ->
    cluster_module_consistency(T,[{[Node],Mods}]);
cluster_module_consistency([{Node,Mods}|T],R) ->
    cluster_module_consistency(T, compare_others({Node,Mods}, R, R)).

compare_others({Node, Mods}, R, []) ->
    [{Node, Mods}|R];
compare_others({Node, Mods}, R, [{Nodes,Mods}|T]) ->
    lists:keyreplace(Nodes, 1, R, {[Node|Nodes], Mods});
compare_others({Node, Mods}, R, [{_,_}|T]) ->
    compare_others({Node, Mods}, R, T).
