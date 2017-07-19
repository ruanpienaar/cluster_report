-module(cluster_report).
-export([
    start/0,
    add_node/2,
    remove_node/1,
    cluster_nodes/0,
    cluster_modules/0, cluster_modules/1,
    cluster_modules_extra_info/0, cluster_modules_extra_info/1,
    cluster_applications/0, cluster_applications/1
]).
%% application:get_key(APP, modules).

-ifdef(TEST).
-export([
    do_cluster_modules/0,
    do_cluster_modules/1,
    do_cluster_applications/0,
    cluster_module_consistency/1, cluster_module_consistency/2,
    compare_others/3,
    cluster_application_consistency/2
]).
-endif.

%%------------------------------------------------------------------------------
%% API

start() ->
    ok = application:start(hawk),
    ok = application:start(cluster_report).

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

cluster_applications() ->
    [Node1|_] = hawk:nodes(),
    cluster_applications(Node1).

cluster_applications(CorrectNode) ->
    try
        cluster_application_consistency(CorrectNode, do_cluster_applications())
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

do_cluster_modules(App) ->
    lists:map(fun(Node) ->
        case rpc:call(Node, application, get_key, [App, modules]) of
            {ok,Res} when is_list(Res) ->
                {Node, lists:sort(Res)};
            {badrpc,R} ->
                throw({badrpc,R})
        end
    end, cluster_nodes()).

do_cluster_applications() ->
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
    % io:format("~p ~p~n~n~n", [Node, Nodes]),
    lists:keyreplace(Nodes, 1, R, {lists:sort([Node|Nodes]), Mods});
compare_others({Node, Mods}, R, [{_,_}|T]) ->
    compare_others({Node, Mods}, R, T).

cluster_application_consistency(CorrectNode, Nodes) ->
    ok.




 % {['test4@rpmbp.home'],
 %  [{application,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
 %   {application_controller,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
 %   {application_master,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"},
 %   {auth,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/auth.beam"}
 %  ]
 % }
 %