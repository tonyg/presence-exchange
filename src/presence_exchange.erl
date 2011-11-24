%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is presence-exchange by Tony Garnock-Jones.
%%
%% The Initial Developers of the Original Code are Rabbit Technologies
%% Ltd and Tony Garnock-Jones.
%%
-module(presence_exchange).
-include_lib("rabbit_common/include/rabbit.hrl").

-define(EXCHANGE_TYPE_BIN, <<"x-presence">>).
-define(LISTENER_KEY, <<"">>).

-behaviour(rabbit_exchange_type).

-rabbit_boot_step({?MODULE,
                   [{description, "exchange type x-presence"},
		    {mfa,         {rabbit_registry, register,
				   [exchange, ?EXCHANGE_TYPE_BIN, ?MODULE]}},
                    {requires,    rabbit_registry},
                    {enables,     kernel_ready}]}).

-export([description/0, serialise_events/0, route/2]).
-export([validate/1, create/2, recover/2, delete/3, add_binding/3,
	 remove_bindings/3, assert_args_equivalence/2]).

encode_binding_delivery(DeliveryXName,
                        Action,
                        #binding{source = #resource{name = XName},
                                 key = BindingKey,
                                 destination = #resource{name = QName}}) ->
    Headers = [{<<"action">>, longstr, atom_to_list(Action)},
               {<<"exchange">>, longstr, XName},
               {<<"queue">>, longstr, QName},
               {<<"key">>, longstr, BindingKey}],
    rabbit_basic:delivery(false, false,
                          rabbit_basic:message(
			    DeliveryXName, ?LISTENER_KEY,
			    [{headers, Headers}], <<>>),
			  undefined).

description() ->
    [{name, ?EXCHANGE_TYPE_BIN},
     {description, <<"Experimental Presence exchange">>}].

serialise_events() -> false.

route(_Exchange, _Delivery) ->
    [].

validate(_X) -> ok.
create(_Tx, _X) -> ok.
recover(_X, _Bs) -> ok.
delete(_Tx, _X, _Bs) -> ok.

%% This code is based on the publish/2 code in rabbit_basic.erl, with
%% the middle step of going through the exchange-type's route/2
%% callback (which in our case always returns []) eliminated.
%%
%% Essentially we're avoiding publishing through ourselves "from the
%% outside", and instead emitting messages as if they originated
%% within the exchange.
deliver(Delivery = #delivery{message = #basic_message{exchange_name = XName,
                                                      routing_keys = RoutingKeys}}) ->
    Queues = rabbit_router:match_routing_key(XName, RoutingKeys),
    rabbit_router:deliver(Queues, Delivery).

announce_initial_bindings(XName, Dest) ->
    announce_initial_bindings(rabbit_binding:list_for_source(XName), XName, Dest).

announce_initial_bindings([], _XName, _Dest) ->
    ok;
announce_initial_bindings([#binding{key = <<>>} | Bs], XName, Dest) ->
    announce_initial_bindings(Bs, XName, Dest);
announce_initial_bindings([B | Bs], XName, Dest) ->
    Delivery = encode_binding_delivery(XName, bind, B),
    rabbit_router:deliver([Dest], Delivery),
    announce_initial_bindings(Bs, XName, Dest).

add_binding(none, #exchange{name = XName}, #binding{key = ?LISTENER_KEY,
                                                    destination = Dest,
                                                    args = ArgsTable}) ->
    case rabbit_misc:table_lookup(ArgsTable, <<"x-presence-exchange-summary">>) of
        {bool, false} -> ok;
        {_, 0} -> ok;
        _ -> %% either undefined or anything non-false and non-zero
            announce_initial_bindings(XName, Dest)
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
