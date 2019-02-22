
ERLC_OPTS += -Wall -Werror -v

PROJECT = rabbit_presence_exchange
PROJECT_DESCRIPTION = RabbitMQ Presence Exchange Plugin
PROJECT_MOD = presence_exchange_app

#define PROJECT_ENV
#[
#	    {exchange, <<"rabbit_presence_exchange">>}
#	  ]
#endef

DEPS = rabbit_common rabbit amqp_client
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
