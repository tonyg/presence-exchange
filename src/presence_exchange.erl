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
%% The Original Code is presence-exchange.
%%
%% The Initial Developers of the Original Code are Rabbit Technologies
%% Ltd and Tony Garnock-Jones.
%%
%% Portions created by Rabbit Technologies Ltd or by Tony Garnock-Jones
%% are Copyright (C) 2010 Rabbit Technologies Ltd and Tony Garnock-Jones.
%%
%% All Rights Reserved.
%%
%% Contributor(s): ______________________________________.
-module(presence_exchange).
-include_lib("rabbit_common/include/rabbit.hrl").

-define(EXCHANGE_TYPE_BIN, <<"x-presence">>).

-rabbit_boot_step({?MODULE,
                   [{mfa, {rabbit_exchange_type_registry, register, [?EXCHANGE_TYPE_BIN, ?MODULE]}},
                    {requires, rabbit_exchange_type_registry},
                    {enables, exchange_recovery}]}).

-behaviour(rabbit_exchange_type).

-export([description/0, publish/2]).
-export([validate/1, create/1, recover/2, delete/2, add_binding/2, remove_bindings/2]).

encode_binding_delivery(DeliveryXName,
                        Action,
                        #binding{exchange_name = #resource{name = XName},
                                 key = BindingKey,
                                 queue_name = #resource{name = QName}}) ->
    Headers = [{<<"action">>, longstr, atom_to_list(Action)},
               {<<"exchange">>, longstr, XName},
               {<<"queue">>, longstr, QName},
               {<<"key">>, longstr, BindingKey}],
    rabbit_basic:delivery(false, false, none,
                          rabbit_basic:message(DeliveryXName, <<>>, [{headers, Headers}], <<>>)).

description() ->
    [{description, <<"Experimental Presence exchange">>}].

publish(_Exchange, _Delivery) ->
    [].

validate(_X) -> ok.
create(_X) -> ok.
recover(_X, _Bs) -> ok.
delete(_X, _Bs) -> ok.

add_binding(X = #exchange{name = XName}, B) ->
    _ = rabbit_exchange_type_fanout:publish(X, encode_binding_delivery(XName, bind, B)),
    ok.

remove_bindings(X = #exchange{name = XName}, Bs) ->
    _ = [rabbit_exchange_type_fanout:publish(X, encode_binding_delivery(XName, unbind, B))
         || B <- Bs],
    ok.
