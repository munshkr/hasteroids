MAIN := Hasteroids.hs
BIN := hasteroids

all: $(BIN)

$(BIN):
	ghc -O2 -o $(BIN) $(MAIN)
	strip $(BIN)

clean:
	rm -f *.hi *.o $(BIN)
