INCLUDES=-I util
CFLAGS=-cflags "-I +ocamlgraph"
LFLAGS=-lflags "-I +ocamlgraph"
LIBS=-lib graph
TARGETS=Fol.cmo 

all:
	ocamlbuild $(INCLUDES) $(LIBS) $(TARGETS) $(CFLAGS) $(LFLAGS)

clean:
	ocamlbuild -clean
