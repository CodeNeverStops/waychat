-module(server_util).
-compile([export_all]).

md5(S) ->
    MD5_bin = erlang:md5(S),
    MD5_list = binary_to_list(MD5_bin),
    lists:flatten(list_to_hex(MD5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> ini_to_hex(X) end, L).

ini_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N < 16 ->
    $a + (N - 10).
    
