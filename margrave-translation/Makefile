INCLUDES=-I lang -I lang/policy -I lang/vocab -I lang/vocab/compile -I util
CFLAGS=-cflags "-I +ocamlgraph"
LFLAGS=-lflags "-I +ocamlgraph"
LIBS=-lib graph
TARGETS=mc.byte

all:
	ocamlbuild $(INCLUDES) $(LIBS) $(TARGETS) $(CFLAGS) $(LFLAGS)

clean:
	ocamlbuild -clean
