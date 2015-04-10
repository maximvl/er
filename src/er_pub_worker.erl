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
-export([start_link/0, new/2, get_or_new/2,
         replace_or_new/2, delete/1, get_tab/1]).

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

-spec new(any(), list()) -> {ok, ets:table()} | {error, atom()}.
new(Name, Opts) ->
  case proplists:get_value(named_table, Opts, false) andalso
    not is_atom(Name) of
    true ->
      throw(badarg);
    _ ->
      case ets:member(?META, Name) of
        false -> gen_server:call(?SERVER, {new, Name, Opts});
        true -> {error, already_exist}
      end
  end.

-spec replace_or_new(any(), list()) -> {ok, ets:table()} | {error, atom()}.
replace_or_new(Name, Opts) ->
  case proplists:get_value(named_table, Opts, false) andalso
    not is_atom(Name) of
    true ->
      throw(badarg);
    _ ->
      gen_server:call(?SERVER, {new, Name, Opts})
  end.

-spec get_or_new(any(), list()) -> {ok, ets:table()} | {error, atom()}.
get_or_new(Name, Opts) ->
  case get_tab(Name) of
    {ok, Tab} -> {ok, Tab};
    {error, not_found} -> new(Name, Opts)
  end.

-spec get_tab(any()) -> {ok, ets:table()} | {error, not_found}.
get_tab(Name) ->
  case ets:lookup(?META, Name) of
    [{Name, Tab}] ->
      case ets:info(Tab, type) of
        undefined ->
          gen_server:cast(?SERVER, {del, Name}),
          {error, not_found};
        _ -> {ok, Tab}
      end;
    _ -> {error, not_found}
  end.

-spec delete(any()) -> ok.
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

handle_cast({del, Name}, State) ->
  ets:delete(?META, Name),
  {noreply, State};

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
-spec del_tab(any()) -> true.
del_tab(Name) ->
  case ets:lookup(?META, Name) of
    [{Name, Tab}] ->
      ets:info(Tab, type) /= undefined andalso ets:delete(Tab),
      ets:delete(?META, Name);
    _ -> true
  end.
