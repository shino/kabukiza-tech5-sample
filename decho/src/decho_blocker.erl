-module(decho_blocker).
-behaviour(gen_server).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([block/2]).
-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

block(Pid, Req) ->
    gen_server:call(Pid, Req, infinity).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call({block, SleepInterval, 0 = _Spawns}, _From, State) ->
    ok = timer:sleep(SleepInterval),
    io:format(user, "sleep_done: ~p~n", [sleep_done]),
    {reply, ok, State};
handle_call({block, SleepInterval, Spawns}, _From, State) ->
    {ok, Spawned} = decho_blocker:start_link(),
    io:format(user, "Spawned: ~p~n", [Spawned]),
    Res = decho_blocker:block(Spawned, {block, SleepInterval, Spawns - 1}),
    {reply, Res, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
