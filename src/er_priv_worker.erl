%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 15 May 2014 by  <>
%%%-------------------------------------------------------------------
-module(er_priv_worker).

-behaviour(gen_server).

%% API
-export([start_link/0, get_tab/1, new_or_get/2,
        new/2, new_or_replace/2, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(META, er_priv_meta).

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
    [{Name, Tab}] ->
      gen_server:call(?SERVER, {get_tab, Name, Tab, self()});
    _ ->
      case new(Name, Opts) of
        {ok, Tab} -> Tab;
        {E, R} -> erlang:E(R)
      end
  end.

delete(Name) ->
  gen_server:call(?SERVER, {del, Name}).

get_tab(Name) ->
  case ets:lookup(?META, Name) of
    [{Name, Tab}] ->
      gen_server:call(?SERVER, {get_tab, Name, Tab, self()});
    _ -> throw(not_found)
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  ets:new(?META, [protected, named_table]),
  {ok, #state{}}.

handle_call({get_tab, Name, Tab, Pid}, _From, State) ->
  Repl = case ets:info(Tab, owner) == self() of
           true ->
             ets:give_away(Tab, Pid, {er_priv, Name}),
             Tab;
           false ->
             {error, not_owner}
         end,
  %% ets:delete(?META, Name),
  {reply, Repl, State};

handle_call({new, Id, Opts}, _From, State) ->
  del_tab(Id),
  Name = if is_atom(Id) -> Id;
            true -> er_priv
         end,
  try
    Tab = ets:new(Name, Opts),
    ets:setopts(Tab, {heir, self(), {er_priv, Id}}),
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

handle_info({'ETS-TRANSFER', Tab, _FromPid, {er_priv, Name}}, State) ->
  case ets:lookup(?META, Name) of
    [{Name, Tab2}] when Tab /= Tab2 ->
      error_logger:error_msg("~p: new table is already registered as ~p",
                             [?MODULE, Name]);
    _ -> ok
  end,
  %% del_tab(Name),  % deletes tab contents too
  ets:insert(?META, {Name, Tab}),
  {noreply, State};

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
      ets:info(Tab, owner) == self() andalso ets:delete(Tab),
      ets:delete(?META, Name);
    _ -> true
  end.
