validerl
========

Erlang input validation and normalization utilities


## Usage

Simply call the funtions in the validerl module, you can validate multiple types

* Boolean validation (accepts 1, 0 or lowercase strings and binaries)
  
  ```erlang
  validerl:get_value(<<"true">>, boolean). % {ok, true}
  validerl:get_value(0, boolean). % {ok, false}
  validerl:get_value("garbage", boolean). % {error, {invalid_boolean, "garbage"}}
  ```

* Binary validation (accepts lists, binaries and atoms, does not handle UTF8)
  
  ```erlang
  validerl:get_value(<<"valid bin">>, binary). % {ok, <<"valid bin">>}
  validerl:get_value(some_atom, binary). % {ok, <<"some_atom">>}
  validerl:get_value("a string", binary). % {ok, <<"a string">>}
  validerl:get_value({a, tuple}, binary). % {error, {invalid_binary, {a, tuple}}}
  ```

* String validation (same as binary validation but returns strings, does not allow UTF8)
  
  ```erlang
  validerl:get_value(<<"valid bin">>, string). % {ok, "valid bin"}
  validerl:get_value(some_atom, string). % {ok, "some_atom"}
  validerl:get_value("a string", string). % {ok, "a string"}
  validerl:get_value({a, tuple}, string). % {error, {invalid_string, {a, tuple}}}
  ```

* Integer validation (accepts integers and both strings and binaries containing a valid integer)
  
  ```erlang
  validerl:get_value(<<"322">>, integer). % {ok, 322}
  validerl:get_value(123, integer). % {ok, 123}
  validerl:get_value("6422", integer). % {ok, 6422}
  validerl:get_value("not valid", integer). % {error, {invalid_integer, "not valid"}}
  ```

* Integer validation with range (same as the integer validation, but checks bounds, interprets 'undefined' as no bounds)

  ```erlang
  validerl:get_value(123, {integer, {0, 1000}}). % {ok, 123}
  validerl:get_value(1232, {integer, {0, 1000}}). % {error, {out_of_valid_range, 1232}}
  validerl:get_value(123, {integer, {0, undefined}}). % {ok, 123}
  validerl:get_value(1232, {integer, {0, undefined}}). % {ok, 1232}
  validerl:get_value(-10, {integer, {0, undefined}}). % {error, {out_of_valid_range, -10}}
  ```

* Atom validation (same as the string or binary validation)

  ```erlang
  validerl:get_value(<<"valid bin">>, atom). % {ok, 'valid bin'}
  validerl:get_value(some_atom, atom). % {ok, some_atom}
  validerl:get_value("string", atom). % {ok, string}
  validerl:get_value({a, tuple}, atom). % {error, {invalid_atom, {a, tuple}}}
  ```

* Tuple validation (validates a N-uple of any valid types)

  ```erlang
  validerl:get_value({321, "true"}, {integer, boolean}). % {ok, {321, true}}
  validerl:get_value({321, "true", other}, {integer, boolean}). % {error, {bad_arity, 3}}
  ```

* List validation (validates a list of items of any other type)

  ```erlang
  validerl:get_value([1, <<"2">>, 3, "4"], [integer]). % {ok, [1, 2, 3, 4]}
  validerl:get_value([1, 2, 3, other], [integer]). % {error, {error, {invalid_integer, other}}
  ```

* Function validation (uses a function to validate the value)

  ```erlang
  validerl:get_value(custom_value, {module, function}).
  % Assumes the function called is 'validate/1'
  validerl:get_value(custom_value, module).
  % The function will be called as Args ++ [custom_value]
  validerl:get_value(custom_value, {module, function, Args}).
  validerl:get_value(custom_value, SomeFun).
  ```