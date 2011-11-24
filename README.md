# RabbitMQ "Presence Exchange" Plugin

Extends RabbitMQ Server with support for a new experimental exchange
type, `x-presence`. An exchange of type `x-presence` notifies queues
that are bound to it when other bindings appear and disappear.

Messages are sent out from an `x-presence` exchange to queues attached
with a binding key of the empty string when a new binding appears or
an existing binding disappears. The message sent out has an empty
body, with all the interesting information in the `headers` property
of the message headers:

    Key       Type  Description
    -----------------------------------------------------------------
    action    str   Either "bind" or "unbind"
    exchange  str   The name of the exchange being bound/unbound
    queue     str   The name of the queue being bound/unbound
    key	      str   The binding key supplied at the time of binding

No messages are published for bindings with a key of the empty
string. That means there are two categories of bindings:

 - bindings made with the empty string `""` as their binding key,
   which receive presence messages, but do not produce them; and

 - bindings made with any other binding key, which produce presence
   messages but do not receive them.

Do not publish messages to an `x-presence` exchange. Right now,
messages published to `x-presence` exchanges are dropped, rather than
sent through to bound queues, but this behavior may change in the
future.

## Initial summary of present bindings

When a new presence listener is bound to a presence exchange (i.e. a
new binding is created with `binding_key` being the empty string), an
initial summary of the available bindings is sent to the newly-bound
queue.

The summary is sent as a sequence of messages, each formatted as a
`bind` message as described above.

The initial summary transmission can be disabled by binding with a
special argument in the `args` table of the `Queue.Bind`
command. Supplying an argument named `x-presence-exchange-summary`
with a value of `0` or `false` will disable the initial summary
transmission; if no such argument is present, or if it has any other
value than a numeric value or boolean false, the initial summary
transmission will be enabled.

## Why aren't messages published to the exchange passed on?

Because it would let arbitrary clients forge presence messages.

Messages *could* be forwarded if they could be marked by the exchange
in a way that made it clear that the passed-on messages didn't
originate from within the exchange itself. One way to do that might be
to have the exchange rewrite messages to be similar to presence
messages, but with an action of `message` and a body containing the
encapsulated properties and body of the relayed message. At the
moment, the RabbitMQ pluggable exchange API makes it a bit tricky to
rewrite messages on their way through an exchange, so for now messages
published to presence exchanges are simply dropped.

## Licensing

This plugin is licensed under the MPL. The full license text is
included with the source code for the package. If you have any
questions regarding licensing, please contact us at
<info@rabbitmq.com>.
