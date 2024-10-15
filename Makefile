

F_SOURCE = src/fortran
F_BUILD  = build/fortran
F_LIB = build/fortran/lib
F_INCLUDE_DIRS = -I$(F_BUILD)/ldpc


.PHONY: all clean

all: build/fortran/test/test_decoder_t build/fortran/test/test_create_checknode_buffer



%/:
	mkdir -p $@


$(F_BUILD)/%.o $(F_BUILD)/%.mod $(F_BUILD)/%.smod : $(F_SOURCE)/%.f90
	gfortran -fPIC -c $< -o $(@D)/$(*F).o -I$(<D) -I$(@D) -J$(@D)

################################
# Dependancies for LDPC module #
################################
LDPC_BUILD = $(F_BUILD)/ldpc
$(LDPC_BUILD)/%.o: $(LDPC_BUILD)
LDPC_SUBMODULES_SRC = $(wildcard $(F_SOURCE)/ldpc/ldpc_*.f90)
LDPC_SUBMODULES_OBJ = $(patsubst $(F_SOURCE)/ldpc/%.f90,$(LDPC_BUILD)/%.o, $(LDPC_SUBMODULES_SRC))
$(LDPC_SUBMODULES_OBJ) : $(LDPC_BUILD)/ldpc.smod
$(F_LIB)/libldpc.a : $(LDPC_BUILD)/ldpc.o $(LDPC_SUBMODULES_OBJ)


$(F_BUILD)/test/% : $(F_SOURCE)/test/%.f90 $(F_SOURCE)/test/%.f90 $(F_BUILD)/test/ $(F_LIB)/libldpc.a
	gfortran $< -o $@ -L$(F_LIB) -I$(F_BUILD)/ldpc -lldpc






$(F_LIB)/lib%.a : $(F_LIB)/
	ar r $@ $(wildcard $(F_BUILD)/$(*F)/*.o)




clean:
	rm -rf build/fortran/ldpc/*
	rm -rf build/fortran/test/*
	rm -rf build/fortran/lib/*
