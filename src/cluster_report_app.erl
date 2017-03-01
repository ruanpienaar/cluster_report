-module(cluster_report_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("cluster_report.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cluster_report_sup:start_link().

stop(_State) ->
    ok.
