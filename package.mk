PACKAGE_NAME:=rabbit_presence_exchange
APP_NAME:=rabbit_presence_exchange
DEPS:=rabbitmq-erlang-client meck-wrapper
STANDALONE_TEST_COMMANDS:=presence_exchange_test:all_tests()
