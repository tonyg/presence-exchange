{application, rabbit_presence_exchange,
 [{description, "RabbitMQ Presence Exchange Plugin"},
  {vsn, "0.01"},
  {modules, [presence_exchange]},
  {registered, []},
  {applications, [kernel, stdlib, rabbit, mnesia]}]}.
