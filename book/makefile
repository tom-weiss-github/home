CC=g++
# CFLAGS=-std=c++11 -c -Wall -Wextra -Werror -ggdb -O0
CFLAGS=-c -Wall -Wextra -Werror -ggdb -O0
BIN=bin
OBJ=obj
MKDIR=mkdir -p
.PHONY : clean all trans rvalues crash types function

all: $(BIN)/trans $(BIN)/rvalues

trans: $(BIN)/trans
$(BIN)/trans: $(OBJ)/trans.o
	$(MKDIR) $(BIN)
	$(CC) -o $@ $<
$(OBJ)/trans.o: trans.cpp
	$(MKDIR) $(OBJ)
	$(CC) $(CFLAGS) -o $@ $<


rvalues: $(BIN)/rvalues
$(BIN)/rvalues: $(OBJ)/rvalues.o
	$(MKDIR) $(BIN)
	$(CC) -o $@ $<
$(OBJ)/rvalues.o: rvalues.cpp
	$(MKDIR) $(OBJ)
	$(CC) $(CFLAGS) -o $@ $<


crash: $(BIN)/crash
$(BIN)/crash: $(OBJ)/crash.o
	$(MKDIR) $(BIN)
	$(CC) -o $@ $<
$(OBJ)/crash.o: crash.cpp
	$(MKDIR) $(OBJ)
	$(CC) $(CFLAGS) -o $@ $<


types: $(BIN)/types
$(BIN)/types: $(OBJ)/types.o
	$(MKDIR) $(BIN)
	$(CC) -o $@ $<
$(OBJ)/types.o: types.cpp
	$(MKDIR) $(OBJ)
	$(CC) $(CFLAGS) -o $@ $<


function: $(BIN)/function
$(BIN)/function: $(OBJ)/function.o
	$(MKDIR) $(BIN)
	$(CC) -o $@ $<
$(OBJ)/function.o: function.cpp
	$(MKDIR) $(OBJ)
	$(CC) $(CFLAGS) -o $@ $<


clean:
	rm -rf $(BIN)
	rm -rf $(OBJ)

# $@ The full name of the dependency.
# $< The name of the first dependency in the list.