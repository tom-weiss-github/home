CC=g++
CFLAGS=-std=c++11 -c -Wall
BIN=bin
OBJ=obj
.PHONY : clean all trans

all: $(BIN)/trans

trans: $(BIN)/trans

$(BIN)/trans: $(OBJ)/trans.o
	mkdir $(BIN)
	$(CC) -o $@ $<

$(OBJ)/trans.o: trans.cpp
	mkdir $(OBJ)
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -rf $(BIN)
	rm -rf $(OBJ)

# $@ The full name of the dependency.
# $< The name of the first dependency in the list.