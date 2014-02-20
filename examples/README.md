# Presence Exchange Examples

These examples use [Pika](http://github.com/pika/pika) to communicate
with the broker. Ensure that the presence exchange plugin is installed
and enabled in your broker before running them.

## Simple chat with presence

See `chat.py`. To run,

    $ ./chat.py username

Run multiple instances in separate windows with different usernames
for maximum benefit.

The code is heavily commented. Key methods are `on_queue_declared` and
`on_message`.

A nasty polling hack is used to monitor `stdin` for input at the same
time as we wait for messages from the broker. See the `poll_stdin`
method for details.
