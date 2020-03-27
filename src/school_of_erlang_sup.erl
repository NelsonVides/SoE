%%%-------------------------------------------------------------------
%% @doc school_of_erlang top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(school_of_erlang_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs =
    [
     #{id => listener,
       start => {listener, start_link, []},
       restart => permanent,
       shutdown => infinity,
       type => worker,
       modules => [listener]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
