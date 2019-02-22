%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(presence_exchange).
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-define(EXCHANGE_TYPE_BIN, <<"x-presence">>).
-define(LISTENER_KEY, <<"">>).

-behaviour(rabbit_exchange_type).

-rabbit_boot_step({?MODULE,
                   [{description, "exchange type x-presence"},
		    {mfa,         {rabbit_registry, register,
				   [exchange, ?EXCHANGE_TYPE_BIN, ?MODULE]}},
		    {cleanup,     {rabbit_registry, unregister,
				   [exchange, ?EXCHANGE_TYPE_BIN]}},
                    {requires,    rabbit_registry},
                    {enables,     kernel_ready}]}).

-export([info/1, info/2, description/0, serialise_events/0, route/2]).
-export([validate/1, validate_binding/2, create/2, delete/3,
         policy_changed/2,
         add_binding/3, remove_bindings/3, assert_args_equivalence/2]).
-export([announce_initial_bindings/2, announce_initial_bindings/3]).

encode_binding_delivery(DeliveryXName,
                        Action,
                        #binding{source = #resource{name = XName},
                                 key = BindingKey,
                                 destination = #resource{name = QName}}) ->
    Headers = [{<<"action">>, longstr, list_to_binary(atom_to_list(Action))},
               {<<"exchange">>, longstr, XName},
               {<<"queue">>, longstr, QName},
               {<<"key">>, longstr, BindingKey},
               {<<"timestamp_microsec">>, long, get_timestamp()}],
    Properties = #'P_basic'{headers = Headers},
    Message = rabbit_basic:message(DeliveryXName, ?LISTENER_KEY, Properties, <<>>),
    rabbit_basic:delivery(
      false,    %% mandatory?
      true,     %% should confirm publication?
      Message,  %% message itself
      undefined %% message sequence number
     ).

info(_X) -> [].
info(_X, _) -> [].

description() ->
    [{name, ?EXCHANGE_TYPE_BIN},
     {description, <<"Experimental Presence exchange">>}].

%% Returns the timestamp in microseconds
get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    ((Mega * 1000000 + Sec) * 1000000 + Micro).

serialise_events() -> false.

route(_Exchange, _Delivery) ->
    [].

validate(_X) -> ok.
validate_binding(_X, _B) -> ok.
create(_Tx, _X) -> ok.
delete(_Tx, _X, _Bs) -> ok.
policy_changed(_X1, _X2) -> ok.

%% This code is based on the publish/2 code in rabbit_basic.erl, with
%% the middle step of going through the exchange-type's route/2
%% callback (which in our case always returns []) eliminated.
%%
%% Essentially we're avoiding publishing through ourselves "from the
%% outside", and instead emitting messages as if they originated
%% within the exchange.
deliver(Delivery = #delivery{message = #basic_message{exchange_name = XName,
                                                      routing_keys = RoutingKeys}}) ->
    QueueNames = rabbit_router:match_routing_key(XName, RoutingKeys),
    Queues = rabbit_amqqueue:lookup(QueueNames),
    rabbit_amqqueue:deliver(Queues, Delivery).

%% 1. Receives the exchange name & the destination queue name
%% 2. Looks up mnesia to find the queue with its name
%% 3. List all the bindings the `XName` exchange has and
%%    calls announce_initial_bindings/3
announce_initial_bindings(XName, Dest) ->
    {ok, DestQueue} = rabbit_amqqueue:lookup(Dest),
    announce_initial_bindings(rabbit_binding:list_for_source(XName), XName, DestQueue).

%% No bindings
announce_initial_bindings([], _XName, _Dest) ->
    ok;
announce_initial_bindings([#binding{key = <<>>} | Bs], XName, Dest) ->
    announce_initial_bindings(Bs, XName, Dest);
announce_initial_bindings([B | Bs], XName, Dest) ->
    Delivery = encode_binding_delivery(XName, bind, B),
    rabbit_amqqueue:deliver([Dest], Delivery),
    announce_initial_bindings(Bs, XName, Dest).


%% Code that gets executed when listeners bind to our exchange. We
%% just have to send the list of bindings that have a non-empty
%% key. Which is done by the function announce_initial_bindings().
add_binding(none, #exchange{name = XName}, #binding{key = ?LISTENER_KEY,
                                                    destination = Dest,
                                                    args = ArgsTable}) ->
    case rabbit_misc:table_lookup(ArgsTable, <<"x-presence-exchange-summary">>) of
        {bool, false} -> ok;
        {_, 0}        -> ok;
        %% either undefined or anything non-false and non-zero
        _             -> presence_exchange:announce_initial_bindings(XName, Dest)
    end,
    ok;
add_binding(none, #exchange{name = XName}, B) ->
    deliver(encode_binding_delivery(XName, bind, B)),
    ok;
add_binding(transaction, _Exchange, _Binding) ->
    ok.

remove_bindings(Tx, X, Bs) ->
    [ok = remove_binding(Tx, X, B) || B <- Bs],
    ok.

remove_binding(none, _X, #binding{key = ?LISTENER_KEY}) ->
    ok;
remove_binding(none, #exchange{name = XName}, B) ->
    deliver(encode_binding_delivery(XName, unbind, B)),
    ok;
remove_binding(transaction, _X, _B) ->
    ok.

assert_args_equivalence(X, Args) ->
    rabbit_exchange:assert_args_equivalence(X, Args).
