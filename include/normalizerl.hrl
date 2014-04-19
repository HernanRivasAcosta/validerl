-include("validerl.hrl").

-type normalizerl_constraint() :: {matches, any() | [any()]}.

-type normalizerl_item() :: {any(), validerl_type(),
                             required | optional | {default, any()}} |
                            {any(), validerl_type(),
                             required | optional | {default, any()},
                             normalizerl_constraint()}.

-type proplist_schema() :: [normalizerl_item()].