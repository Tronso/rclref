-module(rclref_stats_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},

    ChildSpecifications =
        [#{id => rclref_stats,
           start => {rclref_stats, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => worker,
           modules => [some_worker]}],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
