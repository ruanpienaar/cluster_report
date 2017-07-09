-module(cluster_report_tests).
-include_lib("eunit/include/eunit.hrl").

cluster_module_consistency_test() ->
    DoClusterModulesResult =
        [
            {'testnode1@localhost',
                [ {application,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
                  {application_controller,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
                  {application_master,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"}
                ]
            },
            {'testnode2@localhost',
                [ {application,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
                  {application_controller,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
                  {application_master,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"}
                ]
            }
        ],
    ?assertEqual(
        [
         {[testnode1@localhost,testnode2@localhost],
           [{application,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
            {application_controller,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
            {application_master,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"}
           ]
         }
        ],
        cluster_report:cluster_module_consistency(DoClusterModulesResult)
    ).

cluster_module_consistency_inconsistent_test() ->
    %% Node1 and 3 are simmilar, 2 is diff
    DoClusterModulesResult =
        [
            {'testnode1@localhost',
                [ {application,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
                  {application_controller,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
                  {application_master,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"}
                ]
            },
            {'testnode2@localhost',
                [ {application,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
                  {application_controller,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
                  {application_master,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"},
                  {binary,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/binary.beam"}
                ]
            },
            {'testnode3@localhost',
                [ {application,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
                  {application_controller,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
                  {application_master,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"}
                ]
            }
        ],
    ?assertEqual(
        [{testnode2@localhost,
           [ {application,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
             {application_controller,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
             {application_master,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"},
             {binary,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/binary.beam"}
           ]
         },
         {[testnode1@localhost,testnode3@localhost],
           [ {application,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
             {application_controller,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
             {application_master,"/usr/lib/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"}
           ]
         }
        ],
        cluster_report:cluster_module_consistency(DoClusterModulesResult)
    ).

cluster_report_integration_test_() ->
    {setup,
     fun() ->
        % dbg:tracer(),
        % dbg:p(all, call),
        % dbg:tpl(cluster_report, compare_others, cx),
        ?debugFmt("Setup", []),
        ok = application:load(hawk),
        ok = application:set_env(hawk, conn_retry_wait, 1),
        ok = application:set_env(hawk, connection_retries, 100),
        {ok, [hawk,cluster_report]} = application:ensure_all_started(cluster_report),
        {ok, Host} = inet:gethostname(),
        % Host = os:cmd("hostname -f") -- "\n",
        {ok, _} = net_kernel:start([list_to_atom("goanna_eunit_test@"++Host), shortnames])
     end,
     fun(_) ->
        ?debugFmt("Teardown", []),
        ok = application:stop(cluster_report),
        ok = application:stop(hawk)
     end,
     [
        {foreach,
         fun() ->
            Cookie = erlang:get_cookie(),
            ?debugFmt("FOREACH SETUP epmd -names : ~p~n", [os:cmd("epmd -names")]),
            ?debugFmt("foreach fixture setup", []),
            {ok, Host} = inet:gethostname(),
            {ok, SlaveNodeName1} = slave:start(Host, slave_node1),
            {ok, SlaveNodeName2} = slave:start(Host, slave_node2),
            {ok, SlaveNodeName3} = slave:start(Host, slave_node3),

            true = (undefined /= rpc:call(SlaveNodeName1, erlang, whereis, [rex])),
            true = (undefined /= rpc:call(SlaveNodeName2, erlang, whereis, [rex])),
            true = (undefined /= rpc:call(SlaveNodeName3, erlang, whereis, [rex])),

            ?debugFmt("slave node1 cookie:~p\n", [rpc:call(SlaveNodeName1, erlang, get_cookie, [])]),
            ?debugFmt("slave node2 cookie:~p\n", [rpc:call(SlaveNodeName2, erlang, get_cookie, [])]),
            ?debugFmt("slave node3 cookie:~p\n", [rpc:call(SlaveNodeName3, erlang, get_cookie, [])]),

            % timer:sleep(500),
            {ok,_} = cluster_report:add_node(SlaveNodeName1, Cookie),
            {ok,_} = cluster_report:add_node(SlaveNodeName2, Cookie),
            {ok,_} = cluster_report:add_node(SlaveNodeName3, Cookie),
            % timer:sleep(500),
            ?assert(length(hawk:nodes())==3),
            [SlaveNodeName1, SlaveNodeName2, SlaveNodeName3]
         end,
         fun([SlaveNodeName1, SlaveNodeName2, SlaveNodeName3]) ->
            ?debugFmt("foreach fixture teardown", []),
            ok = hawk:remove_node(SlaveNodeName1),
            ok = hawk:remove_node(SlaveNodeName2),
            ok = hawk:remove_node(SlaveNodeName3),
            % timer:sleep(500),
            ok = slave:stop(SlaveNodeName1),
            ok = slave:stop(SlaveNodeName2),
            ok = slave:stop(SlaveNodeName3),
            timer:sleep(500),
            ?debugFmt("CLEANUP epmd -names : ~p~n", [os:cmd("epmd -names")]),
            ?assert(length(hawk:nodes())==0)
         end,
         [
            fun cluster_modules/1,
            fun do_cluster_applications/1
         ]
        }
    ]}.

cluster_modules(Slaves = [Node1, Node2, Node3]) ->
    [{Slaves, Mods}] = cluster_report:cluster_modules(),
    {module,testmod1} = rpc:call(Node1, c, l, [testmod1]),
    {module,testmod1} = rpc:call(Node2, c, l, [testmod1]),
    {module,testmod1} = rpc:call(Node3, c, l, [testmod1]),
    [{Slaves, NewMods}] = cluster_report:cluster_modules(),
    % ?debugFmt("~p~n", [NewMods]),
    ?_assertMatch(
        {testmod1, _BeamPath},
        lists:keyfind(testmod1, 1, NewMods)
    ).

do_cluster_applications(_Slaves) ->
    ?_assert(true).


% node_modules() ->
%     [lists, array, string, re].

% inconsistent_cluster() ->
%     Node1=node_modules()++[my_own_mod1],
%     Node2=node_modules(),
%     Node3=node_modules().

% [{[slave_node1@rpmbp,slave_node2@rpmbp,slave_node3@rpmbp],
%   [{application,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/application.beam"},
%    {application_controller,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/application_controller.beam"},
%    {application_master,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/application_master.beam"},
%    {auth,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/auth.beam"},
%    {binary,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/binary.beam"},
%    {c,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/c.beam"},
%    {code,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/code.beam"},
%    {code_server,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/code_server.beam"},
%    {dist_util,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/dist_util.beam"},
%    {erl_distribution,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/erl_distribution.beam"},
%    {erl_epmd,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/erl_epmd.beam"},
%    {erl_eval,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/erl_eval.beam"},
%    {erl_lint,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/erl_lint.beam"},
%    {erl_parse,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/erl_parse.beam"},
%    {erl_prim_loader,preloaded},
%    {erl_signal_handler,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/erl_signal_handler.beam"},
%    {erl_tracer,preloaded},
%    {erlang,preloaded},
%    {error_handler,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/error_handler.beam"},
%    {error_logger,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/error_logger.beam"},
%    {error_logger_tty_h,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/error_logger_tty_h.beam"},
%    {erts_code_purger,preloaded},
%    {erts_dirty_process_code_checker,preloaded},
%    {erts_internal,preloaded},
%    {erts_literal_area_collector,preloaded},
%    {ets,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/ets.beam"},
%    {file,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/file.beam"},
%    {file_io_server,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/file_io_server.beam"},
%    {file_server,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/file_server.beam"},
%    {filename,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/filename.beam"},
%    {gb_sets,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/gb_sets.beam"},
%    {gb_trees,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/gb_trees.beam"},
%    {gen,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/gen.beam"},
%    {gen_event,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/gen_event.beam"},
%    {gen_server,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/gen_server.beam"},
%    {gen_tcp,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/gen_tcp.beam"},
%    {global,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/global.beam"},
%    {global_group,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/global_group.beam"},
%    {heart,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/heart.beam"},
%    {hipe_unified_loader,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/hipe_unified_loader.beam"},
%    {inet,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/inet.beam"},
%    {inet_config,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/inet_config.beam"},
%    {inet_db,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/inet_db.beam"},
%    {inet_gethost_native,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/inet_gethost_native.beam"},
%    {inet_parse,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/inet_parse.beam"},
%    {inet_tcp,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/inet_tcp.beam"},
%    {inet_tcp_dist,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/inet_tcp_dist.beam"},
%    {inet_udp,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/inet_udp.beam"},
%    {init,preloaded},
%    {kernel,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/kernel.beam"},
%    {kernel_config,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/kernel_config.beam"},
%    {lists,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/lists.beam"},
%    {net_kernel,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/net_kernel.beam"},
%    {orddict,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/orddict.beam"},
%    {os,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/os.beam"},
%    {otp_ring0,preloaded},
%    {prim_eval,preloaded},
%    {prim_file,preloaded},
%    {prim_inet,preloaded},
%    {prim_zip,preloaded},
%    {proc_lib,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/proc_lib.beam"},
%    {re,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/re.beam"},
%    {rpc,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/rpc.beam"},
%    {slave,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/slave.beam"},
%    {standard_error,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/standard_error.beam"},
%    {string,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/string.beam"},
%    {supervisor,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/supervisor.beam"},
%    {supervisor_bridge,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/supervisor_bridge.beam"},
%    {unicode,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/unicode.beam"},
%    {unicode_util,"/Users/ruanpienaar/erlang/20.0/lib/stdlib-3.4/ebin/unicode_util.beam"},
%    {user_sup,"/Users/ruanpienaar/erlang/20.0/lib/kernel-5.3/ebin/user_sup.beam"},
%    {zlib,preloaded}]}]

