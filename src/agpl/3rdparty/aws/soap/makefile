
# $Id: makefile,v 1.1 2003/10/05 19:59:48 Jano Exp $

.SILENT: all build build_ssl build_std distrib ssl_mode std_mode clean

GARGS	= -q -u $(GFLAGS) -I../ssl -I../include -I../src $(INCLUDES)

SOURCES	= soap.ads soap-types.adb soap-parameters.adb soap-message.adb \
	soap-message-xml.adb soap-message-response.adb \
	soap-message-response-error.adb soap-message-payload.adb \
	soap-client.adb soap-utils.adb soap-message-reader.adb

UNITS = $(sort $(basename $(SOURCES)))

force:

$(UNITS): force
	echo Building $@;
	gnatmake -u $(GARGS) $@;

build: $(UNITS)

clean:
	-rm -f *.o *.ali b~*.ad* *.~*.*.~
