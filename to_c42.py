#!/usr/bin/env python
import serial

s = serial.Serial("/dev/tty.usbserial-A100TLCU", baudrate=600)

# send the command to open the file.
s.write("\x01")

byte=1
with open("Handyterm.prg", "r") as f:
    while f:
        ret = -1
        data = f.read(1)
        if len(data) == 0:
            break
        s.write("\x02") # send data command
        while ret != data:
            print "sending byte:      ", byte
            s.write(data)
            print "sedning: ", ord(data)
            ret = s.read(1)
            print "rec'ed:  ", ord(ret)
            if ret != data:
                s.write("\x03")
            else:
                byte+=1
                break
    s.write("\x04")

