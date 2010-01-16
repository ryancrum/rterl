all: rterl

rterl:
	erlc -o ebin src/*.erl

clean:
	rm ebin/*.beam
