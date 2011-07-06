INCLUDES=-I util
CFLAGS=-cflags "-I +ocamlgraph"
LFLAGS=-lflags "-I +ocamlgraph"
LIBS=-lib graph
TARGETS=run.byte

all:
	ocamlbuild $(INCLUDES) $(LIBS) $(TARGETS) $(CFLAGS) $(LFLAGS)

clean:
	ocamlbuild -clean
