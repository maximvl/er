-module(er).

-export([start/0, stop/0]).
-export([new/2, replace_or_new/2, get_or_new/2,
         get_public/1, get_private/1,
         del_public/1, del_private/1]).

start() ->
  application:start(er).

stop() ->
  application:stop(er).

new(Id, Opts) ->
  case proplists:get_value(public, Opts) of
    true -> er_pub_worker:new(Id, Opts);
    undefined -> er_priv_worker:new(Id, Opts)
  end.

replace_or_new(Id, Opts) ->
  case proplists:get_value(public, Opts) of
    true -> er_pub_worker:replace_or_new(Id, Opts);
    undefined -> er_priv_worker:replace_or_new(Id, Opts)
  end.

get_or_new(Id, Opts) ->
  case proplists:get_value(public, Opts) of
    true -> er_pub_worker:get_or_new(Id, Opts);
    undefined -> er_priv_worker:get_or_new(Id, Opts)
  end.

get_public(Id) ->
  er_pub_worker:get_tab(Id).

get_private(Id) ->
  er_priv_worker:get_tab(Id).

del_public(Id) ->
  er_pub_worker:delete(Id).

del_private(Id) ->
  er_priv_worker:delete(Id).
