-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-define(PQ_OPTS,         [long_result, {numtests, 100}]).
-define(PQ_NUM(__Times), [long_result, {numtests, __Times}]).
-define(PQ(__Fn),        proper:quickcheck(__Fn, ?PQ_OPTS)).

-type config() :: proplists:proplist().
-spec init_per_suite(config()) -> config().
-spec end_per_suite(config()) -> config().
-spec all() -> [atom()].
