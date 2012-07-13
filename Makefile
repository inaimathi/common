### Erlang shortcuts
ERL = erl -pa ebin -pa include -pa priv

erl_start = -eval 'lists:map(fun (App) -> application:load(App), application:start(App) end, [sasl, common]).'

### Rules
all: 
	erlc -Wf -o ebin/ src/*erl
	cp src/*app ebin/

install:
	apt-get install screen erlang

start: 
	screen -S common $(ERL) -name common@127.0.1.1 $(erl_start)

attach:
	screen -r common

clean:
	rm ebin/* include/* priv/* 