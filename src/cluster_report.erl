-module(cluster_report).
-export([
    start/0,
    add_node/2,
    remove_node/1,
    cluster_nodes/0,
    cluster_modules/0, cluster_modules/1,
    cluster_apps/0
]).
%% application:get_key(APP, modules).

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
        do_cluster_modules()
    catch
        C:E ->
            {error, C, E}
    end.

cluster_modules(App) ->
    try
        do_cluster_modules(App)
    catch
        C:E ->
            {error, C, E}
    end.

do_cluster_modules() ->
    NodesApps = lists:map(fun(Node) ->
        case rpc:call(Node, code, all_loaded, []) of
            Res when is_list(Res) ->
                {Node, Res};
            {badrpc,R} ->
                throw({badrpc,R})
        end
    end, cluster_nodes()).

do_cluster_modules(App) ->
    NodesApps = lists:map(fun(Node) ->
        case rpc:call(Node, application, get_key, [App, modules]) of
            {ok,Res} when is_list(Res) ->
                {Node, Res};
            {badrpc,R} ->
                throw({badrpc,R})
        end
    end, cluster_nodes()).

cluster_apps() ->
    NodesApps = lists:map(fun(Node) ->
        case rpc:call(Node, application, which_applications, []) of
            Res when is_list(Res) ->
                {Node, Res};
            {badrpc,R} ->
                throw({badrpc,R})
        end
    end, cluster_nodes()).