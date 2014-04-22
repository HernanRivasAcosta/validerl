-type validerl_ext_call()   :: atom() | {atom(), atom()} |
                               {atom(), atom(), [any()]}.
-type validerl_type()       :: boolean | string | integer | {integer, bounds()} |
                               integer_list | {integer_list, bounds()} |
                               validerl_ext_call().
-type validerl_error()      :: {error, {atom(), any()}}.

% Return types
-type validerl_bool()       :: {ok, boolean()}.
-type invaliderl_bool()     :: {error, {invalid_boolean, any()}}.
-type validerl_binary()     :: {ok, binary()}.
-type invaliderl_binary()   :: {error, {invalid_binary, any()}}.
-type validerl_int()        :: {ok, integer()}.
-type invaliderl_int()      :: {error, {invalid_integer, any()}}.
-type bounds()              :: {undefined | integer(), undefined | integer()}.
-type out_of_range()        :: {error, {out_of_valid_range, integer()}}.
-type validerl_string()     :: {ok, string()}.
-type invaliderl_string()   :: {error, {invalid_string, any()}}.
-type validerl_int_list()   :: {ok, [integer()]}.
-type invaliderl_int_list() :: {error, {invalid_integer_list, any()}}.
-type validerl_atom()       :: {ok, atom()}.
-type invaliderl_atom()     :: {error, {invalid_atom, any()}}.