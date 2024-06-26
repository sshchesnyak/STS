%%%-------------------------------------------------------------------
%%% @author sshch
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. апр. 2024 4:36
%%%-------------------------------------------------------------------
-author("sshch").

% Helper functions for logging to the error-logger, or printing trace messages
% to the console.

-define(TRACE(Format, Data),
  io:format("[TRACE] ~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).

-define(INFO(Format, Data),
  error_logger:info_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).

-define(WARN(Format, Data),
  error_logger:warning_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).

-define(ERROR(Format, Data),
  error_logger:error_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).

