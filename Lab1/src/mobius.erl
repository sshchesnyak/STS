%%%-------------------------------------------------------------------
%%% @author sshch
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. февр. 2024 22:13
%%%-------------------------------------------------------------------
-module(mobius).
-author("sshch").

%% API
-export([result/0, is_prime/1, prime_factors/1, soe/1, precomp_prime_factors/1, is_square_multiple/1, find_square_multiples/2]).

is_prime(N, M) when M*M > N -> true;
is_prime(N, M) -> Remainder = N rem M,
  if
    Remainder == 0 -> false;
    true -> is_prime(N, M+1)
  end.
is_prime(1) -> false;
is_prime(N) when N > 1 -> is_prime(N, 2).

prime_factors(N, Rest, M) when M*M > N -> [N|Rest];
prime_factors(N, Rest, M) -> ChkPrime = is_prime(M),
  if
    ChkPrime == true, (N rem M) == 0 -> prime_factors(N div M, [M|Rest], M);
    M == 2 -> prime_factors(N, Rest, 3);
    true -> prime_factors(N, Rest, M+2)
  end.
prime_factors(N) -> lists:reverse(prime_factors(N, [], 2)).

% let's make an alternate list creation method using the Sieve of Eratosthenes
soe(Prime, Max, Primes, Integers) when Prime > Max -> lists:reverse([Prime|Primes]) ++ Integers;
soe(Prime, Max, Primes, Integers) ->
  [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
  soe(NewPrime, Max, [Prime|Primes], NewIntegers).
soe(1) -> [];
soe(N) -> soe(2, round(math:sqrt(N)), [], lists:seq(3,N,2)).

precomp_prime_factors(_N, Rest, []) -> Rest;
precomp_prime_factors(N, Rest, [Prime|Primes]) ->
  if
    N rem Prime == 0 -> precomp_prime_factors(N div Prime, [Prime|Rest], [Prime|Primes]);
    true -> precomp_prime_factors(N, Rest, Primes)
  end.
precomp_prime_factors(N) -> lists:reverse(precomp_prime_factors(N, [], soe(N))).

is_square_multiple(N) -> List = prime_factors(N),
  ListSize = erlang:length(List), SetSize = sets:size(sets:from_list(List)),
  if
    ListSize == SetSize -> false;
    true -> true
  end.

find_square_multiples(_N, _MaxN, List, Len, Count) when Len == Count -> NewList = lists:reverse(List), lists:nth(1, NewList);
find_square_multiples(N, MaxN, _List, _Len, _Count) when N > MaxN -> fail;
find_square_multiples(N, MaxN, List, Len, Count) -> ChkMultiple = is_square_multiple(N),
  if
    ChkMultiple == true -> find_square_multiples(N+1, MaxN, [N|List], Len+1, Count);
    true -> find_square_multiples(N+1, MaxN, [], 0, Count)
  end.
find_square_multiples(Count, MaxN) -> find_square_multiples(2, MaxN, [], 0, Count).
result() -> Start = os:timestamp(),
  io:format("~w~n", [find_square_multiples(6, 30000)]),
  io:format("total time taken ~f seconds~n", [timer:now_diff(os:timestamp(), Start) / 1000000]).
