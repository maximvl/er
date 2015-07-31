-module(er).

-export([start/0, stop/0]).
-export([new/2, new_e/2, replace/2, replace_e/2,
         get_or_new/2, get_or_new_e/2,
         get_public/1, get_public_e/1,
         get_private/1, get_private_e/1,
         get_protected/1, get_protected_e/1,
         obtain/1, obtain_e/1, return_private/1,
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

new_e(Id, Opts) ->
  case new(Id, Opts) of
    {ok, Tab} -> Tab;
    {error, E} -> throw(E)
  end.

replace(Id, Opts) ->
  case proplists:get_value(public, Opts) of
    true -> er_pub_worker:replace_or_new(Id, Opts);
    undefined -> er_priv_worker:replace_or_new(Id, Opts)
  end.

replace_e(Id, Opts) ->
  case replace(Id, Opts) of
    {ok, Tab} -> Tab;
    {error, E} -> throw(E)
  end.

get_or_new(Id, Opts) ->
  case proplists:get_value(public, Opts) of
    true -> er_pub_worker:get_or_new(Id, Opts);
    undefined -> er_priv_worker:get_or_new(Id, Opts)
  end.

get_or_new_e(Id, Opts) ->
  case get_or_new(Id, Opts) of
    {ok, Tab} -> Tab;
    {error, E} -> throw(E)
  end.

get_public(Id) ->
  er_pub_worker:get_tab(Id).

get_public_e(Id) ->
  case er_pub_worker:get_tab(Id) of
    {ok, Tab} -> Tab;
    {error, E} -> throw(E)
  end.

get_protected(Id) ->
  get_private(Id).

get_protected_e(Id) ->
  get_private_e(Id).

get_private(Id) ->
  er_priv_worker:get_tab(Id).

get_private_e(Id) ->
  case er_priv_worker:get_tab(Id) of
    {ok, Tab} -> Tab;
    {error, E} -> throw(E)
  end.

obtain(Id) ->
  er_priv_worker:obtain_tab(Id).

obtain_e(Id) ->
  case er_priv_worker:obtain_tab(Id) of
    {ok, Tab} -> Tab;
    {error, E} -> throw(E)
  end.

del_public(Id) ->
  er_pub_worker:delete(Id).

del_private(Id) ->
  er_priv_worker:delete(Id).

return_private(Id) ->
    case er_priv_worker:get_tab(Id) of
        {ok, Tab} ->
            case ets:info(Tab, owner) == self() of
                true ->
                    ets:give_away(Tab, erlang:whereis(er_priv_worker), {er_priv, Id}),
                    ok;
                false ->
                    {error, not_owner}
            end;
        Err -> Err
    end.
