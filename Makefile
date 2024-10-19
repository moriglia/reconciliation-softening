
F_SOURCE = src/fortran
F_BUILD  = build/fortran

F_LIB = build/fortran/lib
STD_LIB = $(HOME)/.local/lib
LIBNAMES = ldpc simtools	
F_INCLUDE_DIRS = $(patsubst %,-I$(F_BUILD)/%, $(LIBNAMES)) -I$(HOME)/.local/include/fortran_stdlib/GNU-11.4.0/
LIBFILES = $(patsubst %,$(F_LIB)/lib%.a, $(LIBNAMES))

GFFLAGS = -fPIC -O3 -fcoarray=lib -lcaf_openmpi -lfortran_stdlib $(F_INCLUDE_DIRS) -L$(F_LIB) -L$(STD_LIB)


.PHONY: all test libs resoften clean cleanf cleancy

all: test libs resoften

test: build/fortran/test/test_decoder_t build/fortran/test/test_create_checknode_buffer
libs: $(LIBFILES)


%/:
	mkdir -p $@


$(F_BUILD)/%.o $(F_BUILD)/%.mod $(F_BUILD)/%.smod : $(F_SOURCE)/%.f90
	gfortran -c $< -o $(@D)/$(*F).o -I$(@D) -J$(@D) $(GFFLAGS)

################################
# Dependancies for LDPC module #
################################
LDPC_BUILD = $(F_BUILD)/ldpc
$(LDPC_BUILD)/ldpc.smod: $(LDPC_BUILD)/
LDPC_SUBMODULES_SRC = $(wildcard $(F_SOURCE)/ldpc/ldpc_*.f90)
LDPC_SUBMODULES_OBJ = $(patsubst $(F_SOURCE)/ldpc/%.f90,$(LDPC_BUILD)/%.o, $(LDPC_SUBMODULES_SRC))
$(LDPC_SUBMODULES_OBJ) : $(LDPC_BUILD)/ldpc.smod
$(F_LIB)/libldpc.a : $(LDPC_BUILD)/ldpc.o $(LDPC_SUBMODULES_OBJ)


####################################
# Dependancies for SIMTOOLS module #
####################################
SIMTOOLS_BUILD = $(F_BUILD)/simtools
SIMTOOLS_SRC   = $(F_SOURCE)/simtools
$(SIMTOOLS_BUILD)/simtools.smod: $(SIMTOOLS_BUILD)/
SIMTOOLS_SUBMODULES_SRC = $(wildcard $(SIMTOOLS_SRC)/simtools_*.f90)
SIMTOOLS_SUBMODULES_OBJ = $(patsubst $(SIMTOOLS_SRC)/%.f90,$(SIMTOOLS_BUILD)/%.o, $(SIMTOOLS_SUBMODULES_SRC))
$(SIMTOOLS_SUBMODULES_OBJ) : $(SIMTOOLS_BUILD)/simtools.smod
$(F_LIB)/libsimtools.a : $(SIMTOOLS_BUILD)/simtools.o $(SIMTOOLS_SUBMODULES_OBJ)
$(SIMTOOLS_BUILD)/simtools.o $(SIMTOOLS_SUBMODULES_OBJ) : $(F_LIB)/libldpc.a



$(F_BUILD)/test/% : $(F_SOURCE)/test/%.f90 $(F_BUILD)/test/ $(F_LIB)/libldpc.a
	gfortran $< -o $@ -L$(F_LIB) -I$(F_BUILD)/ldpc -lldpc






$(F_LIB)/lib%.a : $(F_LIB)/
	ar r $@ $(wildcard $(F_BUILD)/$(*F)/*.o)




CY_SRC = src/cython
SOEXT = $(shell python3 -c "import distutils; print(distutils.sysconfig.get_config_var('EXT_SUFFIX'))")
CY_LIST = simtools
SO_LIST = $(patsubst %, resoften/%$(SOEXT), $(CY_LIST))
resoften/%$(SOEXT) : $(CY_SRC)/%.pyx $(LIBFILES)
resoften/%$(SOEXT) : $(CY_SRC)/%.pyx $(CY_SRC)/%.pxd $(LIBFILES)
resoften/%$(SOEXT) :
	python3 setup.py build_ext -b . --only $(*F)

resoften : $(SO_LIST) resoften/




clean: cleanf cleancy

cleanf:
	rm -rf $(patsubst %, $(F_BUILD)/%/**, ldpc simtools test lib)

cleancy:
	rm -rf $(SO_LIST) $(wildcard build/lib.*/$(CY_SRC)/* build/temp.*/$(CY_SRC)/*)
