%%%-------------------------------------------------------------------
%%% @author sshch
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. апр. 2024 2:56
%%%-------------------------------------------------------------------
-module(rss_reader).
-author("sshch").
-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").
-include("logging.hrl").
-define(RECEIVE_INTERVAL, 10000).
-define(TIMEOUT, 10000).
-import(rss_queue,[start/1, server/1, get_record_state/2, add_feed/2, add_item/2, get_all/1]).
-import(rss_parse,[is_rss2_feed/1, get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).

%% API
-export([start/2, system_process/0, web_server/2, web_result/0, loop/1]).

web_server(URL, QPid) -> link(QPid),
  receive after ?RECEIVE_INTERVAL ->
    try
      {Status, Result} = httpc:request(URL),
      if
        Status == ok ->
          {StatusLine, _Header, Response} = Result,
          {_Version, Code, Deciphered} = StatusLine,
          if
            Code == 200 ->
              {Feed, _} = xmerl_scan:string(Response),
              Valid = is_rss2_feed(Feed),
              if
                Valid == true ->
                  FeedItems = get_feed_items(Feed, []),
                  add_feed(QPid, FeedItems),
                  web_server(URL, QPid)
              end;
            true -> throw({connect_error, Deciphered})
          end;
        true -> throw({httpc_error, Result})
      end
    catch
      throw:{httpc_error, Msg} ->
        ?ERROR("Error connecting to resource ~p: ~p.~nThe web server will now exit...~n",[URL, Msg]),
        exit(httpc_error);
      throw:{connect_error, Msg} ->
        ?ERROR("Error getting response from resource ~p: ~p.~nThe web server will now exit...~n",[URL, Msg]),
        exit(connect_error);
      throw:validation_error ->
        ?ERROR("The RSS feed received from ~p is not a valid RSS 2.0 feed.~nThe web server will now exit...",[URL]),
        exit(validation_error)
    end

  end.

start(URL, PID) -> PID ! {start_server, [URL], self()},
  receive
    Value -> {ok, Value}
  after ?TIMEOUT -> {error, timeout}
  end.

system_process() ->
  process_flag(trap_exit, true),
  receive
    {start_server, State, ReqID} -> Q=[], SubList = sets:new(),
      QPid = spawn_link(rss_queue,server,[Q, SubList]),
      if
        State == [] -> ?INFO("Starting base queue process ~p in system process.~n",[QPid]),
          ReqID ! QPid, system_process();
        true -> [URL] = State, ?INFO("Starting queue process ~p with URL ~p in system process.~n",[QPid, URL]),
          spawn(?MODULE, web_server, [URL, QPid]), ReqID ! QPid, system_process()
      end;
    {'EXIT', PID, Reason} ->
      ?WARN("Process ~p terminated.~nThe system process ~p will now be terminating...~n",[PID, self()]),
      exit(Reason);
    _Value -> io:format("Some value in system process ~p~n",[_Value]), system_process()
  end.

loop(BasePID) ->
  receive after 5000 ->
    {NewState, NewList} = get_all(BasePID),
    if
      NewState == ok -> display_items(NewList), loop(BasePID);
      true -> ?ERROR("Error obtaining base queue items list~n",[])
    end
  end.

web_result() -> inets:start(), ssl:start(),
  SysPID = spawn(?MODULE, system_process, []),
  {StatusCNN, PidCNN} = start("http://rss.cnn.com/rss/cnn_topstories.rss", SysPID),
  {StatusNYT, PidNYT} = start("https://rss.nytimes.com/services/xml/rss/nyt/World.xml", SysPID),
  {StatusYahoo, PidYahoo} = start("https://www.yahoo.com/news/rss", SysPID),
  {StatusBase, BasePID} = start(SysPID),
  if
    StatusCNN == error -> ?ERROR("Error creating CNN process~n", []);
    true ->
      if
        StatusNYT == error -> ?ERROR("Error creating New York Times process~n", []);
        true ->
          if
            StatusYahoo == error -> ?ERROR("Error creating Yahoo process~n", []);
            true ->
              if
                StatusBase == error -> ?ERROR("Error creating base process~n", []);
                true ->
                  PidCNN ! {subscribe, BasePID},
                  PidNYT ! {subscribe, BasePID},
                  PidYahoo ! {subscribe, BasePID},
                  loop(BasePID)
              end
          end
      end
  end.
