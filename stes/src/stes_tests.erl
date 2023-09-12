-module(stes_tests).
-include_lib("eunit/include/eunit.hrl").

reverse_test_() -> ?_assert([2, 3] =:= lists:reverse([3, 2])).
