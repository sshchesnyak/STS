%%%-------------------------------------------------------------------
%%% @author sshch
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rss_process_new).
-author("sshch").
-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").
-include("logging.hrl").
-define(RECEIVE_INTERVAL, 10000).
-define(TIMEOUT, 10000).
-import(rss_queue_new,[start/1, server/1, get_record_state/2, add_feed/2, add_item/2, get_all/1]).
-import(rss_parse,[is_rss2_feed/1, get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).

-behaviour(gen_server).
-record(systemP,{}).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init([]) -> process_flag(trap_exit,true), {ok, #systemP{}}.

handle_call(_Request = {start_server, QState}, _From, State = #systemP{}) -> io:format("We are in process!~n"),
  {ReqState, Result} = gen_server:start_link(rss_queue_new, [], []),
  if
    ReqState == ok ->
      if
        QState == [] -> ?INFO("Starting base queue process ~p in system process.~n",[Result]), {reply, {ReqState, Result}, State};
        true -> [URL] = QState, ?INFO("Starting queue process ~p with URL ~p in system process.~n",[Result, URL]),
          {_WebState, _WebResult} = gen_server:start(rss_reader_new, [URL, Result], []), {reply, {ReqState, Result}, State}
      end;
    true -> {reply, {ReqState, Result}, State}
  end.

handle_cast(_Msg = Value, State) -> io:format("Some value in system process ~p~n",[Value]), {noreply, State}.

handle_info(_Info = {'EXIT', PID, _Reason}, State) ->
  ?ERROR("Process ~p died from ~p with reason~n",[PID, self()]),
  {noreply, State}.

terminate(_Reason, _State) -> ?INFO("System process ~p is shutting down with reason.~n",[self()]), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
