-module(presence_exchange_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-export([all_tests/0]).
-import(presence_exchange, [description/0, serialize_event/0]).

%% Ensure description returns what we configured
description_test() ->
    ?assertEqual(
       presence_exchange:description(),
       [{name, <<"x-presence">>},
        {description, <<"Experimental Presence exchange">>}]

      ),
    ok.

%% Ensure serialize_events() always return false
serialise_events_test() ->
    ?assertEqual(presence_exchange:serialise_events(), false).

% announce_initial_bindings_no_binding_test() ->
%     presence_exchange:announce_initial_bindings([]),
%     ok.

add_binding_skip_initial_announcement_test() ->

    %% Given an exchange and an incoming binding from a LISTENER,
    %% notice the empty key: "", also notice that we're explicitly
    %% disabling the initial announcement with the
    %% "x-presence-exchange-summary" key set to false.
    Exchange = #exchange{name = <<"my-exchange">>},
    Binding = #binding{
      key = <<"">>,
      destination = <<"">>,
      args = [{<<"x-presence-exchange-summary">>, bool, false}]},
    Transaction = none,

    %% When I try to add a new binding to my-exchange
    AddBinding = presence_exchange:add_binding(Transaction, Exchange, Binding),

    %% Then I see it should just return ok without calling the
    %% function announce_initial_bindings()
    ?assertEqual(AddBinding, ok),

    ok. %% Have a nice day!

add_binding_test() ->

    %% Given an exchange and an incoming binding from a LISTENER,
    %% notice the empty key: ""
    Exchange = #exchange{name = <<"my-exchange">>},
    Binding = #binding{
      key = <<"">>,
      destination = <<"destination">>,
      args = [{<<"x-presence-exchange-summary">>, bool, true}]},
    Transaction = none,

    %% And a mock to the function that does the initial announcement
    meck:new(presence_exchange, [passthrough]),
    meck:expect(presence_exchange, announce_initial_bindings,
                fun(XName, Dest) ->
                        ?assertEqual(XName, <<"my-exchange">>),
                        ?assertEqual(Dest, <<"destination">>),
                        {ok, XName, Dest}
                end),

    %% When I try to add a new binding to my-exchange
    AddBinding = presence_exchange:add_binding(Transaction, Exchange, Binding),

    %% Then I see it should just return ok without calling the
    %% function announce_initial_bindings()
    ?assert(meck:validate(presence_exchange)),
    ?assertEqual(AddBinding, ok),

    meck:unload(presence_exchange),
    ok. %% Have a nice day!

all_tests() ->
    % ok = announce_initial_bindings_no_binding_test(),
    ok = description_test(),
    ok = serialise_events_test(),
    ok = add_binding_skip_initial_announcement_test(),
    ok = add_binding_test(),
    ok.
