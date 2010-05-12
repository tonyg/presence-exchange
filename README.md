# RabbitMQ "Presence Exchange" Plugin

Extends RabbitMQ Server with support for a new experimental exchange
type, `x-presence`. An exchange of type `x-presence` notifies queues
that are bound to it when other bindings appear and disappear.

Messages are only ever sent out from an `x-presence` exchange to
attached queues when a new binding appears or an existing binding
disappears. The message sent out is (pseudo-)ASCII, of the form

    action: add
    exchange: the-exchange-name
    queue: the-queue-name
    key: the-binding-key-that-was-used

where `action` can be either `add` or `remove`, and the `exchange`,
`queue` and `key` are the raw bytes that were sent in the `Queue.Bind`
command. Each line is terminated by a single newline character (`\n`,
ASCII 10).

Messages published to `x-presence` exchanges are dropped, rather than
sent through to bound queues.

## Licensing

This plugin is licensed under the MPL. The full license text is
included with the source code for the package. If you have any
questions regarding licensing, please contact us at
<info@rabbitmq.com>.
