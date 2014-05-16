-module(er).

-export([start/0, stop/0]).
-export([new/2, new_or_replace/2, new_or_get/2,
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

new_or_replace(Id, Opts) ->
  case proplists:get_value(public, Opts) of
    true -> er_pub_worker:new_or_replace(Id, Opts);
    undefined -> er_priv_worker:new_or_replace(Id, Opts)
  end.

new_or_get(Id, Opts) ->
  case proplists:get_value(public, Opts) of
    true -> er_pub_worker:new_or_get(Id, Opts);
    undefined -> er_priv_worker:new_or_get(Id, Opts)
  end.

get_public(Id) ->
  er_pub_worker:get_tab(Id).

get_private(Id) ->
  er_priv_worker:get_tab(Id).

del_public(Id) ->
  er_pub_worker:delete(Id).

del_private(Id) ->
  er_priv_worker:delete(Id).
