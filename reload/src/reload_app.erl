-module(reload_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start()->
	io:format("Starting up reload app ~n"),
	inets:start(),
	ssl:start(),
	Res = application:start(reload),
	io:format("REs is ~w ~n", [ Res ]).

start(_StartType, _StartArgs) ->
	io:format("Start link"),
    reload_sup:start_link().

stop(_State) ->
    ok.
