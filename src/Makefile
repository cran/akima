LIBNAME=akima
#LD=ld
#FFLAGS=-g
#SHLIBLDFLAGS=-shared

OBJS=	idbvip.o idgrid.o idpdrv.o idsfft.o idxchg.o idcldp.o \
	idlctn.o idptip.o  idtang.o idptli.o akima.new.o tripack.subset.o


$(LIBNAME):	$(OBJS)
	@$(LD) $(SHLIBLDFLAGS) -o $(LIBNAME).so $(OBJS)  # -lf2c

clean:
	@rm -f *.o *.so ttidbs driver fort.2 WFSD3P

realclean:
	@rm -f Makefile *.o *.so

# Test programs from original Fortran distribution:
test: ttidbs driver
 
ttidbs: ttidbs.f $(OBJS)
	$(F77) $(FFLAGS) ttidbs.f idbvip.o idgrid.o idpdrv.o idsfft.o idxchg.o \
           idcldp.o idlctn.o idptip.o  idtang.o idptli.o -o ttidbs

driver: driver.f akima.new.o
	$(F77) $(FFLAGS) driver.f akima.new.o tripack.subset.o -o driver


# compilation with f77 -g -fpic fails on Linux with g77 0.5.19.1!
# (assembler complains about unknown i386 instructions, so it seems to be
#  an g77 internal problem)
# But it works when the PICFLAG is omitted:
#%.o: %.f
#	$(F77) -g -c $< -o $@
# compilation with f77 -O2 -fpic works, but now interpp() (it calls idbvip)
# crashes R!

