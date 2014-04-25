-module(rlogger).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/1, log_msg/3, roll/1]).

% These are all wrappers for calls to the server
start(Filename) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Filename], [ ]).
log_msg(MsgType, Msg, Format) -> gen_server:cast(?MODULE, {MsgType, Msg, Format}). 
roll(Filename)-> gen_server:call(?MODULE, roll, Filename).

logfile_name(Prefix)->
	{{Year, Month, Day},_} = calendar:local_time(),
	io_lib:format("~s-~B-~B-~B.txt", [Prefix, Year, Month, Day]).


% This is called when a connection is made to the server
init([ undefined ]) ->
	io:format("Starting up the logger without a Filename~n"),
	{ok, _} = timer:apply_interval(86400000, ?MODULE, roll, [undefined]),
	{ok, State} = file:open(logfile_name("logfile"), [append]),
	{ok, State};

init([ Filename ]) ->
	io:format("Starting up the logger with a Filename~n"),
	{ok, _} = timer:apply_interval(86400000, ?MODULE, roll, [Filename]),
	{ok, State} = file:open(logfile_name(Filename), [append]),
	{ok, State}.

leak_memory([]) ->
	leak_memory(<<"Binnary 1111111">>);

leak_memory(State)->
	<<"BINARY">> ++ leak_memory(State).


handle_cast({error, Msg, MsgFormat}, State) ->
	{{Year, Month, Day},{Hour, Minute, Second}} = calendar:local_time(),
	io:format("About to breakkk"),
	io:format("Bad float ~f", [ "hello"]), 
	io:format(State, "~B-~B-~B ~B:~B~B:ERROR: ~s~n ",[Year, Month, Day, Hour, Minute, Second, io_lib:format(Msg, MsgFormat)]),
	{noreply, State};

handle_cast({info, Msg, MsgFormat}, State) ->
	{{Year, Month, Day},{Hour, Minute, Second}} = calendar:local_time(),
	io:format("Generic Message ~n"),
	io:format(State, "~B-~B-~B ~B:~B~B:INFO: ~s~n ",[Year, Month, Day, Hour, Minute, Second, io_lib:format(Msg, MsgFormat)]),
	{noreply, State};

handle_cast({debug, Msg, MsgFormat}, State) ->
	{{Year, Month, Day},{Hour, Minute, Second}} = calendar:local_time(),
	io:format(State, "~B-~B-~B ~B:~B~B:DEBUG: ~s~n ",[Year, Month, Day, Hour, Minute, Second, io_lib:format(Msg, MsgFormat)]),
	{noreply, State};

handle_cast(_Message, State) -> {noreply, State}.


handle_call(roll, undefined, State) -> 
	ok = file:close(State),
	{ok, NewState}  = file:open(logfile_name("hqcontroller"), [append]),
	{noreply, NewState};

handle_call(roll, Filename, State) -> 
	ok = file:close(State),
	{ok, NewState}  = file:open(logfile_name(Filename), [append]),
	{noreply, NewState};


handle_call(_, _, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.

terminate(_Reason, State) -> 
	file:close(State),
	ok.

code_change(_OldVersion, State, _Extra) -> 
	io:format("Code has changed ~n"),
	{ok, State}.
