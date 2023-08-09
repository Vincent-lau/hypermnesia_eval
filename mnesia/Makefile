ERL=erl
DOCKER=docker
APP_NAME=mnesia-bench
DOCKER_USER=cuso4
EFLAGS=-pa ebin
VSN=0.1

all: compile

k8s: build
	kubectl delete -f deploy/erl-cluster.yaml && \
	kubectl apply -f deploy/erl-cluster.yaml

build: compile
	$(DOCKER) build -t $(APP_NAME) . && \
	$(DOCKER) tag $(APP_NAME) $(DOCKER_USER)/$(APP_NAME):latest && \
	$(DOCKER) push $(DOCKER_USER)/$(APP_NAME):latest

compile: src/*.erl
	test -d ebin || mkdir ebin
	$(ERL) $(EFLAGS) -make
	# cp $(APP_NAME).app ebin

doc:	
	$(ERL) $(EFLAGS) -noshell \
	-run edoc_run application "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	test ! -d doc || rm -rfv doc
	rm -rfv erl_crash.dump ebin
	find . -name "*~" -exec rm -fv {} \;

# for testing purpose
run: compile
	$(ERL) $(EFLAGS)  -eval 'application:start($(APP_NAME)).'