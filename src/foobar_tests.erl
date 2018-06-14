-module(foobar_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assert(foobar:foo(4) =:= 5).
