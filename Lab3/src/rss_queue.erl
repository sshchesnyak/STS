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
-define(TIMEOUT,10000).
-import(rss_parse,[get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).

%% API
-export([result/0, server/1, start/0, add_item/2, add_feed/2, get_all/1]).

% @doc This is the queue process that stores all RSSItems in a list for a quick return.
%      It keeps all entries in order of publication date by sorting it after appending a new element.

server(List) ->
  receive
    {add_item, RSSItem} ->
      if
        is_record(RSSItem, xmlElement) ->
          if
            List == [] -> server([RSSItem]);
            true -> {State, Item} = get_record_state(List, RSSItem),
              FilterFunc = fun(Item1,Item2) -> get_item_time(Item1) < get_item_time(Item2) end,
              case State of
                same -> server(List);
                updated ->
                  RemovedList = lists:delete(Item, List),
                  NewList = lists:sort(FilterFunc, [RSSItem|RemovedList]),
                  server(NewList);
                different ->
                  NewList = lists:sort(FilterFunc, [RSSItem|List]),
                  server(NewList)
              end
          end;
        true -> server(List)
      end;
    {get_all, ReqID} -> ReqID ! List, server(List);
    _Value -> io:format("Some value ~p~n",[_Value])
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

start() -> Q=[], spawn(?MODULE,server,[Q]).

add_item(QPid, Item) -> QPid ! {add_item, Item}, ok.

add_feed(QPid, Feed) -> lists:foreach(fun(Item) -> add_item(QPid, Item) end, Feed), ok.

get_all(QPid) -> QPid ! {get_all, self()},
  receive
    Value -> {ok, Value}
  after ?TIMEOUT -> {error, timeout}
  end.

result() -> PID = start(),
  {Feed1,_} = xmerl_scan:file("digg-science-rss1.xml"), {Feed2,_} = xmerl_scan:file("digg-science-rss2.xml"),
  Feed1Items = get_feed_items(Feed1, []), Feed2Items = get_feed_items(Feed2, []),
  add_feed(PID, Feed1Items),
  {State, List} = get_all(PID),
  if
    State == ok -> display_items(List);
    true -> io:format("Error obtaining items list")
  end,
  add_feed(PID, Feed2Items),
  {NewState, NewList} = get_all(PID),
  if
    NewState == ok -> display_items(NewList);
    true -> io:format("Error obtaining second items list")
  end,
  exit(PID, normal).
