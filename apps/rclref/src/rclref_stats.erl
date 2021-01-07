-module(rclref_stats).

-behaviour(gen_server).

%% API
-export([stop/0, start_link/0]).
-export([add_keys/2, add_put/1, add_get/1, add_delete/1, remove_keys/2, get_stats/1, compute_load_balancing/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state,
        {node :: term(),
         keys :: non_neg_integer(),
         puts :: non_neg_integer(),
         gets :: non_neg_integer(),
         deletes :: non_neg_integer()}).

-type state() :: #state{}.
-type stats() ::
    {term(),
    {keys, non_neg_integer()},
    {puts, non_neg_integer()},
    {gets, non_neg_integer()},
    {deletes, non_neg_integer()}}.

stop() ->
    gen_server:cast(?MODULE, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok,
     #state{node = node(),
            keys = 0,
            puts = 0,
            gets = 0,
            deletes = 0}}.

-spec add_keys(Node :: term(), Number :: non_neg_integer()) -> ok.
add_keys(Node, Number) ->
    gen_server:call({?MODULE, Node}, {add_keys, Number}).

-spec add_put(Node :: term()) -> ok.
add_put(Node) ->
    gen_server:call({?MODULE, Node}, add_put).

-spec add_get(Node :: term()) -> ok.
add_get(Node) ->
    gen_server:call({?MODULE, Node}, add_get).

-spec add_delete(Node :: term()) -> ok.
add_delete(Node) ->
    gen_server:call({?MODULE, Node}, add_delete).

-spec remove_keys(Node :: term(), Number :: non_neg_integer()) -> ok.
remove_keys(Node, Number) ->
    gen_server:call({?MODULE, Node}, {remove_keys, Number}).

-spec get_stats(Node :: term()) -> {ok, stats()}.
get_stats(Node) ->
    gen_server:call({?MODULE, Node}, get_stats).

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({add_keys, Number}, _From, State = #state{keys = Keys}) ->
    {reply, ok, State#state{keys = Keys + Number}};
handle_call(add_get, _From, State = #state{gets = Gets}) ->
    {reply, ok, State#state{gets = Gets + 1}};
handle_call(add_put, _From, State = #state{puts = Puts}) ->
    {reply, ok, State#state{puts = Puts + 1}};
handle_call(add_delete, _From, State = #state{deletes = Deletes}) ->
    {reply, ok, State#state{deletes = Deletes + 1}};
handle_call({remove_keys, Number}, _From, State = #state{keys = Keys}) ->
    {reply, ok, State#state{keys = max(0, Keys - Number)}};
handle_call(get_stats, _From, State) ->
    {reply, {ok, stats_from_state(State)}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec stats_from_state(State :: state()) -> stats().
stats_from_state(#state{node = Node,
                        keys = Keys,
                        puts = Puts,
                        gets = Gets,
                        deletes = Deletes}) ->
    {Node,
    {keys, Keys},
    {puts, Puts},
    {gets, Gets},
    {deletes, Deletes}}.

-spec compute_load_balancing([stats()]) -> [{term(), float()}].

compute_load_balancing(Stats) ->
    KeysPerNode = [{Node, Keys} || {Node, {keys, Keys}, _, _, _} <- Stats],
    TotalKeys = lists:foldl(fun({_, K}, Sum) -> Sum + K end, 0 , KeysPerNode),
    lists:map(fun({N, K}) -> {N, K / TotalKeys} end, KeysPerNode).
