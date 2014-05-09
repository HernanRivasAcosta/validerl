PROJECT = validerl

TEST_DEPS = proper
dep_proper = pkg://proper v1.1

CT_OPTS = -cover test/validerl.coverspec
CT_SUITES = validerl

include erlang.mk
