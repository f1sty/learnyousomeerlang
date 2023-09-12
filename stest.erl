-module(stest).
-include_lib("eunit/include/eunit.hrl").

reverse_test() -> [1, 3] = lists:reverse([3, 1]).
