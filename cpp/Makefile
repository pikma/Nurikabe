# This program requires the glog library, which can be found at
# http://google.com/p/google-glog/.

#  CC=g++
CC=clang++

SRCS=$(wildcard *.cc)
OBJS=$(patsubst %.cc,%.o,$(SRCS))
DEPS=$(patsubst %.cc,%.d,$(SRCS))

all: main

LDFLAGS=-lglog

main: $(OBJS)
	$(CC) -o main $^ $(LDFLAGS)

CFLAGS=-Wall -std=c++11 -O2
#  CFLAGS=-Wall -std=c++11 -pg  # profile
#  CFLAGS=-Wall -std=c++11 -g   # debug

$(OBJS): %.o: %.cc
	$(CC) -c -MMD -MP $(CFLAGS) $< -o $@
	@sed -i -e '1s,\($*\)\.o[ :]*,\1.o $*.d: ,' $*.d

-include $(DEPS)

clean:
	rm -f *.o *.d gmon.out
