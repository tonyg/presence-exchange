#!/usr/bin/env python2
import sys
import pika

###########################################################################
#
# This is an example of the use of the RabbitMQ presence_exchange
# plugin, which adds a new 'x-presence' exchange type to the system.
#
# Two exchanges are created:
#
#  - 'presence.demo.chat.presence', which is an x-presence exchange
#    used to track who is "in the chat room"; and
#
#  - 'presence.demo.chat.messages', which is a fanout exchange used
#    for relaying messages between the current members of the "room".
#
# See ChatSession.on_queue_declared for the fine detail of how to use
# the exchanges for this kind of application.
#
# If the broker is shut down, this program attempts to reconnect. If a
# connection attempt fails, it sleeps for one second and tries again.
#
###########################################################################

## UGH SelectConnection's SelectPoller.TIMEOUT is hardcoded initially to 1 second!
import pika.adapters.select_connection
pika.adapters.select_connection.SelectPoller.TIMEOUT = 0.1

PRESENCE_EXCHANGE = 'presence.demo.chat.presence'
MESSAGES_EXCHANGE = 'presence.demo.chat.messages'

class ChatSession(object):
    def __init__(self, username, params):
        self.username = username
        self.params = params
        self.connection = pika.SelectConnection(parameters = params,
                                                on_open_callback = self.on_connection_open,
                                                on_open_error_callback = self.on_connection_error,
                                                on_close_callback = self.on_connection_close)
        self.channel = None
        self.private_queue = None

    def run(self):
        try:
            self.connection.ioloop.start()
        except KeyboardInterrupt:
            self.connection.close()

    def on_connection_open(self, _connection):
        self.connection.channel(self.on_channel_open)

    def on_connection_error(self, _connection):
        print 'Connection problem; sleeping and retrying'
        import time
        time.sleep(1)
        self._reboot()

    def on_connection_close(self, _connection, code, message):
        print 'Broker shutdown: code', code, 'message', repr(message)
        self._reboot()

    def _reboot(self):
        ## Reboot session
        ChatSession(self.username, self.params).run()

    def on_channel_open(self, c):
        self.channel = c

        #
        # Here we declare the two application-specific exchanges, and
        # a private per-connection autodelete queue.
        #
        # Once the queue is created, we continue with
        # on_queue_declared.
        #

        self.channel.exchange_declare(exchange = PRESENCE_EXCHANGE,
                                      exchange_type = 'x-presence')
        self.channel.exchange_declare(exchange = MESSAGES_EXCHANGE,
                                      exchange_type = 'fanout')
        self.channel.queue_declare(self.on_queue_declared, queue = '', auto_delete = True)

    def on_queue_declared(self, reply):

        #
        # This is the most interesting part of this program. Here, we
        # bind *twice* to the x-presence exchange:
        #
        #  - once with routing_key '', in order to express our interest in HEARING
        #    about bindings coming and going; and
        #
        #  - once with routing_key self.username, in order to actually
        #    be present in the room, and cause a signal to be emitted
        #    along all peer bindings that happen to have routing_key
        #    ''.
        #
        # Either of these alone would have only half the desired
        # effect: just the '' binding means we'd hear about others
        # coming and going, but wouldn't let anyone else know about
        # our own movements; and just the self.username binding means
        # we'd signal our movements but wouldn't be told about others
        # coming and going.
        #
        # Finally, we bind to the fanout exchange, in order to
        # actually send messages to other peers.
        #

        self.private_queue = reply.method.queue
        self.channel.queue_bind(None, self.private_queue, PRESENCE_EXCHANGE, username)
        self.channel.queue_bind(None, self.private_queue, PRESENCE_EXCHANGE, '')
        self.channel.queue_bind(None, self.private_queue, MESSAGES_EXCHANGE)
        self.channel.basic_consume(self.on_message, self.private_queue)

        # This kicks off the polling loop (ugh!) which monitors stdin
        # for input lines to send to our peers.
        self.poll_stdin()

    def poll_stdin(self):
        import select
        while select.select([0], [], [], 0)[0]:
            line = sys.stdin.readline().strip()
            self.channel.basic_publish(MESSAGES_EXCHANGE, self.username, line)

        ## Unfortunately SelectConnection's ioloop isn't set up to
        ## permit us to register for I/O events on any other file
        ## descriptor than the one connecting us to the broker, so
        ## instead we *ugh* poll here.
        self.connection.ioloop.add_timeout(0.1, self.poll_stdin)

    def on_message(self, channel, method_frame, header_frame, body):
        self.channel.basic_ack(delivery_tag=method_frame.delivery_tag)

        #
        # Here we examine the message that was delivered to us.
        #
        # If it came from the x-presence exchange, it will relate to
        # either the arrival or departure of a peer.
        #
        # If it came from the fanout exchange, it will be a message
        # that some other peer injected: something that someone
        # "said".
        #

        exchange = method_frame.exchange
        if exchange == PRESENCE_EXCHANGE:
            action = header_frame.headers['action']
            who = header_frame.headers['key']
            if action == 'bind':
                print 'User %s entered the room.' % (who,)
            elif action == 'unbind':
                print 'User %s left the room.' % (who,)
        elif exchange == MESSAGES_EXCHANGE:
            who = method_frame.routing_key
            print '%s: %s' % (who, body)

if len(sys.argv) < 2:
    print 'Usage: chat.py <username> [<brokerurl]'
    print 'where brokerurl defaults to amqp://guest:guest@localhost/'
    sys.exit(1)

username = sys.argv[1]
params = URLParameters(sys.argv[2]) if len(sys.argv) > 2 else None
ChatSession(username, params).run()
