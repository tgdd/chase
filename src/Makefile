INCLUDES=-I fol \
         -I fol-extra \
		 -I util \
         -I margrave \
		 -I margrave \
         -I margrave/policy \
		 -I margrave/vocab \
		 -I margrave/vocab/compile
CFLAGS=-cflags "-thread"
LFLAGS=-lflags "-thread"
TARGETS=mc.byte example.byte

all:
	ocamlbuild -use-ocamlfind $(INCLUDES) $(LIBS) $(TARGETS) $(CFLAGS) $(LFLAGS)

clean:
	ocamlbuild -clean
