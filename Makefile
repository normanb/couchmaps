COUCH_HOME=~/projects/couchdb
ERLC=erlc
EBIN=ebin

COUCH_INCLUDE= ~/projects/couchdb/couchdb/dependencies/couchdb/src/couchdb/

EFLAGS=-o ${EBIN} -I ${COUCH_INCLUDE}

SRCS = ${wildcard src/*.erl}

all: clean compile
   
compile:
	${ERLC} ${EFLAGS} ${SRCS}

clean:
	rm -rf ${EBIN}/*.beam
