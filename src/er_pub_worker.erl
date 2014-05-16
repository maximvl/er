%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 13 May 2014 by  <>
%%%-------------------------------------------------------------------
-module(er_pub_worker).

-behaviour(gen_server).

%% API
-export([start_link/0, new/2, new_or_get/2,
         new_or_replace/2, delete/1, get_tab/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(META, er_pub_meta).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new(Name, Opts) ->
  case proplists:get_value(named_table, Opts, false) andalso
    not is_atom(Name) of
    true ->
      throw(badarg);
    _ ->
      ets:member(?META, Name) andalso throw(already_exist),
      gen_server:call(?SERVER, {new, Name, Opts})
  end.

new_or_replace(Name, Opts) ->
  case proplists:get_value(named_table, Opts, false) andalso
    not is_atom(Name) of
    true ->
      throw(badarg);
    _ ->
      gen_server:call(?SERVER, {new, Name, Opts})
  end.

new_or_get(Name, Opts) ->
  case ets:lookup(?META, Name) of
    [{Name, Tab}] -> Tab;
    _ ->
      case new(Name, Opts) of
        {ok, Tab} -> Tab;
        {E, R} -> erlang:E(R)
      end
  end.

get_tab(Name) ->
  case ets:lookup(?META, Name) of
    [{Name, Tab}] -> Tab;
    _ -> throw(not_found)
  end.

delete(Name) ->
  gen_server:call(?SERVER, {del, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  ets:new(?META, [protected, named_table]),
  {ok, #state{}}.

handle_call({new, Id, Opts}, _From, State) ->
  del_tab(Id),
  Name = if is_atom(Id) -> Id;
            true -> er_pub
         end,
  try
    Tab = ets:new(Name, Opts),
    ets:insert(?META, {Id, Tab}),
    {reply, {ok, Tab}, State}
  catch E:R ->
      {reply, {E, R}, State}
  end;

handle_call({del, Name}, _From, State) ->
  del_tab(Name),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
del_tab(Name) ->
  case ets:lookup(?META, Name) of
    [{Name, Tab}] ->
      ets:info(Tab) /= undefined andalso ets:delete(Tab),
      ets:delete(?META, Name);
    _ -> true
  end.
