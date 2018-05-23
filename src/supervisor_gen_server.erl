%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2018 12:41
%%%-------------------------------------------------------------------
-module(supervisor_gen_server).
-author("rafal").
-behavior(supervisor).

%% API
-export([start_link/1, init/1]).

start_link(InitValue) ->
  supervisor:start_link({local, supervisorGenServer}, ?MODULE, InitValue).

init(InitValue) ->
  {ok, {
    {one_for_one, 2, 3},
    [
      {
        pollution_var_server,
        {pollution_var_server, start_link, [InitValue]},
        permanent, brutal_kill, worker, [pollution_var_server]
      },
      {pollution_gen_server, {
        pollution_gen_server, start_link, [InitValue]},
        permanent, brutal_kill, worker, [pollution, pollution_gen_server]
      }
    ]
  }}.
