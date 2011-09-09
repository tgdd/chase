INCLUDES=-I fol \
         -I mpc \
		 -I mpc/vocab \
		 -I mpc/policy \
		 -I tests/fol \
		 -I tests/mpc \
		 -I tests/mpc/vocab \
		 -I tests/mpc/policy \
		 -I tests \
		 -I util
CFLAGS=-cflags "-thread -g"
LFLAGS=-lflags "-thread -g"
TARGETS=mpc.byte tests.byte

all:
	ocamlbuild -use-ocamlfind $(INCLUDES) $(LIBS) $(TARGETS) $(CFLAGS) $(LFLAGS)
	ocamlrun -b ./tests.byte

clean:
	ocamlbuild -clean
