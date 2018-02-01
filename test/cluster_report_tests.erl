-module(cluster_report_tests).
-include_lib("eunit/include/eunit.hrl").

cluster_module_consistency_test() ->
    DoClusterModulesResult =
        [
            {'testnode1@localhost',
                [ application,application_controller,application_master ]
            },
            {'testnode2@localhost',
                [ application,application_controller,application_master ]
            }
        ],
    ?assertEqual(
        [
         {['testnode1@localhost',testnode2@localhost],
           [ application, application_controller, application_master ]
         }
        ],
        cluster_report:consistency(DoClusterModulesResult)
    ).

cluster_module_consistency_inconsistent_test() ->
    %% Node1 and 3 are simmilar, 2 is diff
    DoClusterModulesResult =
        [
            {'testnode1@localhost',
                [ application, application_controller, application_master ]
            },
            {'testnode2@localhost',
                [ application, application_controller, application_master, binary ]
            },
            {'testnode3@localhost',
                [ application, application_controller, application_master ]
            }
        ],
    ?assertEqual(
        [{'testnode2@localhost',
           [ application, application_controller, application_master, binary ]
         },
         {['testnode1@localhost','testnode3@localhost'],
           [ application, application_controller, application_master ]
         }
        ],
        cluster_report:consistency(DoClusterModulesResult)
    ).

% Since i always use 1 to compare, let's test this...
cluster_module_consistency_2_3_inconsistent_test() ->
    %% Node2 and 3 are simmilar, 1 is diff
    DoClusterModulesResult =
        [
            {'testnode1@localhost',
                [ application, application_controller, application_master, binary ]
            },
            {'testnode2@localhost',
                [ application, application_controller, application_master ]
            },
            {'testnode3@localhost',
                [ application, application_controller, application_master ]
            }
        ],
    ?assertEqual(
        [{['testnode2@localhost','testnode3@localhost'],
           [ application, application_controller, application_master ]
         },
         {'testnode1@localhost',
           [ application, application_controller, application_master, binary ]
         }
        ],
        cluster_report:consistency(DoClusterModulesResult)
    ).


cluster_module_consistency_all_inconsistent_test() ->
    %% Node1, 2 and 3 are diff
    DoClusterModulesResult =
        [
            {'testnode1@localhost',
                [ application, application_controller, application_master ]
            },
            {'testnode2@localhost',
                [ application, application_controller, application_master, binary ]
            },
            {'testnode3@localhost',
                [ application, application_controller, crypto ]
            }
        ],
    ?assertEqual(
        [{'testnode3@localhost',
           [ application, application_controller, crypto ]
         },
         {'testnode2@localhost',
           [ application, application_controller, application_master, binary ]
         },
         {'testnode1@localhost',
           [ application, application_controller, application_master ]
         }
        ],
        cluster_report:consistency(DoClusterModulesResult)
    ).

%% All apps consistent
cluster_application_consistency_test() ->
    DoClusterApplicationsResult =
        [
            {'test2@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"}
            ]},
            {'test1@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"}
            ]}
        ],
    ?assertEqual(
        [
            {['test1@mbp', 'test2@mbp'],
             [
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"}
             ]
            }
        ],
        cluster_report:consistency(DoClusterApplicationsResult)
    ).

%% Apps inconsistent ( mnesia extra on node 1 )
cluster_application_consistency_inconsistent_test() ->
DoClusterApplicationsResult =
        [
            {'test2@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"}
            ]},
            {'test1@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {mnesia,"MNESIA  CXC 138 12","4.15"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"}
            ]}
        ],
    ?assertEqual(
        [
            {'test1@mbp',
             [
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {mnesia,"MNESIA  CXC 138 12","4.15"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"}
             ]
            },
            {'test2@mbp',
             [
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"}
             ]
            }
        ],
        cluster_report:consistency(DoClusterApplicationsResult)
    ).

%% Here the application versions are diff
cluster_application_consistency_inconsistent_app_version_test() ->
DoClusterApplicationsResult =
        [
            {'test2@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"},
                {zzz_my_own_app, "ZZZ app", "1.0.1"}
            ]},
            {'test1@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"},
                {zzz_my_own_app, "ZZZ app", "1.0.123"}
            ]}
        ],
    ?assertEqual(
        [
            {'test1@mbp',
             [
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"},
                {zzz_my_own_app, "ZZZ app", "1.0.123"}
             ]
            },
            {'test2@mbp',
             [
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"},
                {zzz_my_own_app, "ZZZ app", "1.0.1"}
             ]
            }
        ],
        cluster_report:consistency(DoClusterApplicationsResult)
    ).

%% Here the application desc's are diff
cluster_application_consistency_inconsistent_app_desc_test() ->
DoClusterApplicationsResult =
        [
            {'test2@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"},
                {zzz_my_own_app, "ZZZ app", "1.0.1"}
            ]},
            {'test1@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"},
                {zzz_my_own_app, "ZZZ-bla app", "1.0.1"}
            ]}
        ],
    ?assertEqual(
        [
            {'test1@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"},
                {zzz_my_own_app,"ZZZ-bla app","1.0.1"}
            ]},
            {'test2@mbp',[
                {asn1,"The Erlang ASN1 compiler version 5.0","5.0"},
                {crypto,"CRYPTO","4.0"},
                {kernel,"ERTS  CXC 138 10","5.3"},
                {public_key,"Public key infrastructure","1.4.1"},
                {runtime_tools,"RUNTIME_TOOLS","1.12"},
                {sasl,"SASL  CXC 138 11","3.0.4"},
                {ssl,"Erlang/OTP SSL application","8.2"},
                {stdlib,"ERTS  CXC 138 10","3.4"},
                {zzz_my_own_app,"ZZZ app","1.0.1"}
            ]}
        ],
        cluster_report:consistency(DoClusterApplicationsResult)
    ).

% cluster_application_consistency_inconsistency_test() ->
%     ?_test(true).

cluster_report_integration_test_() ->
    {setup,
     fun() ->
        ok = application:load(hawk),
        ok = application:set_env(hawk, conn_retry_wait, 1),
        ok = application:set_env(hawk, connection_retries, 100),
        {ok, [hawk,cluster_report]} = application:ensure_all_started(cluster_report),
        {ok, Host} = inet:gethostname(),
        {ok, _} = net_kernel:start([list_to_atom("goanna_eunit_test@"++Host), shortnames])
     end,
     fun(_) ->
        ok = application:stop(cluster_report),
        ok = application:stop(hawk)
     end,
     [
        {foreachx,
         fun(_) ->
            Cookie = erlang:get_cookie(),
            {ok, Host} = inet:gethostname(),
            {ok, SlaveNodeName1} = slave:start(Host, slave_node1),
            {ok, SlaveNodeName2} = slave:start(Host, slave_node2),
            {ok, SlaveNodeName3} = slave:start(Host, slave_node3),
            timer:sleep(30),
            true = (undefined /= rpc:call(SlaveNodeName1, erlang, whereis, [rex])),
            true = (undefined /= rpc:call(SlaveNodeName2, erlang, whereis, [rex])),
            true = (undefined /= rpc:call(SlaveNodeName3, erlang, whereis, [rex])),
            {ok,_} = cluster_report:add_node(SlaveNodeName1, Cookie),
            {ok,_} = cluster_report:add_node(SlaveNodeName2, Cookie),
            {ok,_} = cluster_report:add_node(SlaveNodeName3, Cookie),
            timer:sleep(100), %% Sleep for hawk, and slave modules to load.
            ?assert(length(hawk:nodes())==3),
            [SlaveNodeName1, SlaveNodeName2, SlaveNodeName3]
         end,
         fun(_, [SlaveNodeName1, SlaveNodeName2, SlaveNodeName3]) ->
            ok = hawk:remove_node(SlaveNodeName1),
            ok = hawk:remove_node(SlaveNodeName2),
            ok = hawk:remove_node(SlaveNodeName3),
            ok = slave:stop(SlaveNodeName1),
            ok = slave:stop(SlaveNodeName2),
            ok = slave:stop(SlaveNodeName3),
            timer:sleep(100),
            ?assert(length(hawk:nodes())==0)
         end,
         [
            {"cluster_modules", fun cluster_modules/2},
            {"cluster_applications", fun cluster_applications/2}
         ]
        }
    ]}.

cluster_modules(_, _Slaves = [Node1, Node2, Node3]) ->
    timer:sleep(100),
    ClusterModsResponse = cluster_report:cluster_modules(),
    %% check that all 3 slaves are consistent
    ?assertMatch(
        [{[Node1, Node2, Node3], Mods}],
        ClusterModsResponse
    ),
    [{_, Mods}] = ClusterModsResponse,
    % ?debug
    %% Test some modules
    ?assertMatch(true, lists:member(lists, Mods)),
    ?assertMatch(true, lists:member(net_kernel, Mods)),
    ?_assertMatch(true, lists:member(erlang, Mods)).

cluster_applications(_, Slaves = [Node1, Node2, Node3]) ->
    timer:sleep(100),
    %% check that all 3 slaves are consistent
    ClusterAppsResponse = cluster_report:cluster_applications(),

    ?assertMatch(
        [{[Node1, Node2, Node3], [{kernel,_,_},{stdlib,_,_}]}],
        ClusterAppsResponse
    ),
    ?assertEqual(ok, rpc:call(Node1, application, start, [asn1])),
    ?assertEqual(ok, rpc:call(Node2, application, start, [asn1])),
    ?_assertEqual(ok, rpc:call(Node3, application, start, [asn1])).
