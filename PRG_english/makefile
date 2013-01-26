CC=gcc
CFLAGS = -O
.c.o:  ; $(CC) -c $(CFLAGS) $<
OBJ = init.o \
      boundary.o \
      uvp.o \
      visual.o \
      surface.o \
      main.o 

run: $(OBJ)
	$(CC) $(CFLAGS) -o run $(OBJ) -lm


init.o       : datadef.h
boundary.o   : datadef.h
uvp.o        : init.h datadef.h
visual.o     : visual.h datadef.h
surface.o    : surface.h visual.h datadef.h
main.o       : init.h boundary.h uvp.h datadef.h surface.h visual.h
