%%%-------------------------------------------------------------------
%%% @author sshch
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. февр. 2024 12:55
%%%-------------------------------------------------------------------
-module(rss_parse).
-author("sshch").
-include("C:/Program Files/Erlang OTP/lib/xmerl-1.3.34/include/xmerl.hrl").
-include("C:/Program Files/Erlang OTP/lib/inets-9.1/include/httpd.hrl").

%% API
-export([result/0, is_rss2_feed/1, get_feed_items/2, get_item_time/1, compare_feed_items/2, display_items/1]).

is_rss2_feed(Data) ->
  if
    is_record(Data, xmlElement) -> Func = fun({_, Name, _, _, _, _, _, _, Value, _}) -> Name == version andalso Value == "2.0" end,
      Result = lists:filter(Func, Data#xmlElement.attributes),
      if
        length(Result) == 1 -> true;
        true -> false
      end;
    true -> false
  end.

get_feed_items(Record, List) -> if
                                  is_record(Record, xmlElement) ->
                                    if
                                      Record#xmlElement.name == item -> [Record|List];
                                      true -> lists:foldl(fun rss_parse:get_feed_items/2, List, Record#xmlElement.content)
                                    end;
                                  true -> List
                                end.

get_item_string(Item) -> Title = get_item_parameter(Item, title),
  Link = get_item_parameter(Item, link),
  GUID = get_item_parameter(Item, guid),
  Time = get_item_parameter(Item, pubDate),
  {GUID, Title, Link, Time}.

get_items_string(Feed) -> lists:foldl(fun(Item,List) -> [get_item_string(Item)|List] end, [], Feed).

get_item_parameter(Item, Parameter) -> L = lists:filter(fun(Elem) -> element(1,Elem) == xmlElement andalso element(2,Elem) == Parameter end, Item#xmlElement.content),
  if
    length(L) > 0 -> Container = lists:nth(1,L), Text = lists:nth(1, Container#xmlElement.content), Text#xmlText.value;
    true -> false
  end.

get_item_time(Item) -> Value = get_item_parameter(Item, pubDate),
  if
    Value == false -> bad_date;
    true -> RequestDate = httpd_util:convert_request_date(Value),
      if
        RequestDate == bad_date -> RequestDate;
        true -> calendar:datetime_to_gregorian_seconds(RequestDate)
      end
  end.

compare_feed_items(OldItem, NewItem) -> OldString = get_item_string(OldItem), NewString = get_item_string(NewItem),
  if
    OldString == NewString -> same;
    true -> OldGUID = get_item_parameter(OldItem, guid), NewGUID = get_item_parameter(NewItem, guid),
      ValidGUID = OldGUID /= false andalso NewGUID /= false,
      if
        ValidGUID andalso OldGUID == NewGUID -> updated;
        true -> OldTitle = get_item_parameter(OldItem, title), NewTitle = get_item_parameter(NewItem, title),
          ValidTitle = OldTitle /= false andalso NewTitle /= false,
          if
            ValidTitle andalso OldTitle == NewTitle -> updated;
            true -> OldLink = get_item_parameter(OldItem, link), NewLink = get_item_parameter(NewItem, link),
              ValidLink = OldLink /= false andalso NewLink /= false,
              if
                ValidLink andalso OldLink == NewLink -> updated;
                true -> different
              end
          end
      end
  end.

build_unique_feed(Feed1Items, Feed2Items) ->
  Func1 = fun(Item, List) -> L = lists:filter(fun(Elem) -> compare_feed_items(Elem,Item) /= different end, Feed2Items),
    if
      length(L)>0 -> [Item] ++ List;
      true -> List
    end
          end,
  L1 = lists:foldl(Func1,[],Feed1Items),
  L1U = Feed1Items -- L1,
  lists:sort(fun(Item1,Item2) -> get_item_time(Item1) < get_item_time(Item2) end, L1U ++ Feed2Items).

display_items(RSSItems) -> DisplayItems = get_items_string(RSSItems),
  io:format("Articles ~p:~n~p~n~n~n~n~n~n~n~n~n~n",[length(DisplayItems),DisplayItems]).

result() -> RSS1 = xmerl_scan:file("digg-science-rss1.xml"), RSS2 = xmerl_scan:file("digg-science-rss2.xml"),
  Valid1 = is_rss2_feed(RSS1), Valid2 = is_rss2_feed(RSS2),
  io:format("Are both of the feeds valid? ~p and ~p~n", [Valid1, Valid2]),
  {Feed1,_} = RSS1, {Feed2,_} = RSS2,
  Feed1Items = get_feed_items(Feed1, []), Feed2Items = get_feed_items(Feed2, []),
  DisplayFeed1 = get_items_string(Feed1Items),
  io:format("The first news feed (~p articles):~n~p~n~n~n~n~n~n~n~n~n~n",[length(DisplayFeed1), DisplayFeed1]),
  DisplayFeed2 = get_items_string(Feed2Items),
  io:format("The second news feed (~p articles):~n~p~n~n~n~n~n~n~n~n~n~n",[length(DisplayFeed2), DisplayFeed2]),
  DisplayUniqueFeed = get_items_string(build_unique_feed(Feed1Items, Feed2Items)),
  io:format("Unique news feed (~p articles):~n~p~n",[length(DisplayUniqueFeed), DisplayUniqueFeed]).
