%%%-------------------------------------------------------------------
%%% @author sshch
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. март 2024 12:32
%%%-------------------------------------------------------------------
% @doc This module is designed for holding and managing RSS feed items. Every queue works with a single RSS feed
-module(rss_queue).
-author("sshch").
-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").
-include("logging.hrl").
-define(TIMEOUT,10000).
-import(rss_parse,[get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).
-import(rss_reader,[start/2, system_process/0, web_server/2]).

%% API
-export([result/0, server/2, start/1, add_item/2, add_feed/2, get_all/1, get_record_state/2]).

% @doc This is the queue process that stores all RSSItems in a list for a quick return.
%      It keeps all entries in order of publication date by sorting it after appending a new element.

server(List, SubList) -> process_flag(trap_exit, true),
  receive
    {add_item, RSSItem} ->
      if
        is_record(RSSItem, xmlElement) ->
          if
            List == [] -> server([RSSItem], SubList);
            true -> {State, Item} = get_record_state(List, RSSItem),
              FilterFunc = fun(Item1,Item2) -> get_item_time(Item1) < get_item_time(Item2) end,
              case State of
                same -> server(List, SubList);
                updated ->
                  RemovedList = lists:delete(Item, List),
                  NewList = lists:sort(FilterFunc, [RSSItem|RemovedList]),
                  SubSet = sets:to_list(SubList),
                  lists:foreach(fun(QPid) -> add_item(QPid, RSSItem) end, SubSet),
                  server(NewList, SubList);
                different ->
                  NewList = lists:sort(FilterFunc, [RSSItem|List]),
                  SubSet = sets:to_list(SubList),
                  lists:foreach(fun(QPid) -> add_item(QPid, RSSItem) end, SubSet),
                  server(NewList, SubList)
              end
          end;
        true -> server(List, SubList)
      end;
    {get_all, ReqID} -> ReqID ! List, server(List, SubList);
    {subscribe, QPid} -> Validate = sets:is_element(QPid, SubList),
      if
        Validate == true -> server(List, SubList);
        true ->
          NewSubList = sets:add_element(QPid, SubList),
          ?INFO("Queue ~p subscribed to base queue~n",[QPid]),
          monitor(process,QPid),
          add_feed(QPid, List),
          server(List, NewSubList)
      end;
    {unsubscribe, QPid} -> Validate = sets:is_element(QPid, SubList),
      if
        Validate == false -> server(List, SubList);
        true ->
          NewSubList = sets:del_element(QPid, SubList),
          ?INFO("Queue ~p unsubscribed from base queue~n",[QPid]),
          server(List, NewSubList)
      end;
    {'DOWN', _Ref, process, QPid, Reason} -> ?WARN("Base queue terminated with reason ~p",[Reason]),
      NewSubList = sets:del_element(QPid,SubList),
      server(List, NewSubList);
    {'EXIT', PID, Reason} -> Validate = sets:is_element(PID, SubList),
      if Validate == true ->
        ?WARN("Base queue process terminated with reason ~p.~nThe queue process ~p will now be terminating...~n",[Reason, self()]),
        NewSubList = sets:del_element(PID,SubList),
        server(List, NewSubList);
        true ->
          ?WARN("Process ~p terminated with reason ~p.~nThe queue process ~p will now be terminating...~n",[PID, Reason, self()]),
          exit(Reason)
      end;
    _Value -> io:format("Some value ~p~n",[_Value]), server(List, SubList)
  end.

get_record_state(List, RSSItem) ->
  SameList = lists:filter(fun(Elem) -> compare_feed_items(Elem,RSSItem) == same end, List),
  UpdatedList = lists:filter(fun(Elem) -> compare_feed_items(Elem,RSSItem) == updated end, List),
  if
    length(SameList) > 0 -> {same, lists:nth(1,SameList)};
    true -> if
              length(UpdatedList) > 0 -> {updated, lists:nth(1,UpdatedList)};
              true -> {different, undefined}
            end
  end.

start(PID) -> PID ! {start_server, [], self()},
  receive
    Value -> {ok, Value}
  after ?TIMEOUT -> {error, timeout}
  end.

add_item(QPid, Item) -> QPid ! {add_item, Item}, ok.

add_feed(QPid, Feed) -> lists:foreach(fun(Item) -> add_item(QPid, Item) end, Feed), ok.

get_all(QPid) -> QPid ! {get_all, self()},
  receive
    Value -> {ok, Value}
  after ?TIMEOUT -> {error, timeout}
  end.

result() -> SysPID = spawn(rss_reader, system_process, []), {Status, PID} = start(SysPID),
  if
    Status == ok ->
      {Feed1,_} = xmerl_scan:file("digg-science-rss1.xml"), {Feed2,_} = xmerl_scan:file("digg-science-rss2.xml"),
      Feed1Items = get_feed_items(Feed1, []), Feed2Items = get_feed_items(Feed2, []),
      add_feed(PID, Feed1Items),
      {State, List} = get_all(PID),
      if
        State == ok -> display_items(List);
        true -> io:format("Error obtaining items list~n")
      end,
      add_feed(PID, Feed2Items),
      {NewState, NewList} = get_all(PID),
      if
        NewState == ok -> display_items(NewList);
        true -> io:format("Error obtaining second items list~n")
      end,
      io:format("About to call process ~p to exit~n", [PID]),
      exit(PID, normal);
    true -> ?ERROR("Error creating process~n", [])
  end.
