%%%-------------------------------------------------------------------
%%% @author sshch
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. февр. 2024 19:23
%%%-------------------------------------------------------------------
-module(fib).
-author("sshch").

%% API
-export([fib_p/1, fib_g/1, result/0, tail_fib/1]).

fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) -> fib_p(N-1)+fib_p(N-2).

fib_g(N) when N < 2 -> N;
fib_g(N) -> fib_g(N-1)+fib_g(N-2).

% S(0) = (fib(0), fib(1))
% S(n) = succ(fib(n-1), fib(n)) = (fib(n), fib(n+1)) = (fib(n), fib(n-1)+fib(n))
% Iter - номер итерации, Result - fib(n), Next - fib(n+1).
tail_fib(0, Result, _Next) -> Result;
tail_fib(Iter, Result, Next) when Iter > 0 -> tail_fib(Iter-1, Next, Result+Next).
tail_fib(N) -> tail_fib(N, 0, 1).


result() -> Start = os:timestamp(),
  io:format("~w~n",[tail_fib(10000)]),
  io:format("total time taken ~f seconds~n", [timer:now_diff(os:timestamp(), Start) / 1000000]).
