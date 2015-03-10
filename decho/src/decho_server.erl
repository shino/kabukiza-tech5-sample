-module(decho_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {previous}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([call/1, block/0, block/1]).
-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

call(Req) ->
    gen_server:call(?SERVER, Req).

block() ->
    block(2000).

block(SleepInterval) ->
    gen_server:call(?SERVER, {block, SleepInterval, 2}, infinity).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(boom, _From, State) ->
    {reply, {ok, boom:boom()}, State};
handle_call({sleep, SleepTime}, _From, State) ->
    {reply, {ok, timer:sleep(SleepTime)}, State};
handle_call({block, _, _} = BlockReq, _From, State) ->
    {ok, Blocker} = decho_blocker:start_link(),
    Res = decho_blocker:block(Blocker, BlockReq),
    {reply, Res, State};
handle_call(Request, _From, #state{previous = spam} = State) ->
    {reply, {ok, string:copies("SPAM!!", 10)}, State#state{previous = Request}};
handle_call(Request, _From, #state{previous = Previous} = State) ->
    {reply, {ok, Previous}, State#state{previous = Request}}.


handle_cast({sleep, SleepTime}, State) ->
    timer:sleep(SleepTime),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

