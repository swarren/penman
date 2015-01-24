#!/usr/bin/env python3

import argparse
import serial
import sys
import time

class Log(object):
    def __init__(self, f=sys.stdout):
        self._f = f
        self._last_dir_out = True
        self._f.write('>>')

    def sending(self, data):
        if not self._last_dir_out:
            self._f.write('<<\n>>')
        self._last_dir_out = True
        self._write_data(data)

    def received(self, data):
        if self._last_dir_out:
            self._f.write('>>\n<<')
        self._last_dir_out = False
        self._write_data(data)

    def done(self):
        if self._last_dir_out:
            self._f.write('>>\n')
        else:
            self._f.write('<<\n')

    def _write_data(self, data):
        for c in data:
            if isinstance(c, int):
                c = chr(c)
            s = repr(c)
            if s[0] == 'b':
                s = s[1:]
            if s[0] == "'":
                s = s[1:]
            if s[-1] == "'":
                s = s[:-1]
            self._f.write(s)
        self._f.flush()

class Penman(object):
    def __init__(self, port, tx_only, log=None):
        self._tx_only = tx_only
        self._log = log
        self._ready = True
        self._s = serial.Serial(port, baudrate=9600, timeout=0)

    def send(self, cmd):
        self._ready = False
        for i in range(len(cmd)):
            self._drain_rx(False)
            c = cmd[i]
            if self._log:
                self._log.sending(c)
            self._s.write(bytearray(c, 'ascii'))
            # We need this sleep, since the SW flow-control from the device
            # takes a long time to respond. The 10ms delay below is about 10
            # character times which is hopefully plenty; I'd guess that 1ms
            # would be fine, but this is fast enough and seems to work.
            time.sleep(0.01)
        self._drain_rx(True)

    def _drain_rx(self, wait_for_ready):
        if self._tx_only:
            if wait_for_ready:
                if self._log:
                    self._log.received("wait for ready")
            return

        wait = False
        while True:
            c = self._s.read(1)
            if not c:
                if wait or (wait_for_ready and not self._ready):
                    continue
                else:
                    break
            if self._log:
                self._log.received(c)
            if c == b'!':
                self._ready = True
            if c == b'\x11':
                wait = False
            elif c == b'\x13':
                wait = True

def split_commands(l):
    cmds = []
    while l:
        is_label_cmd = (l[0] == 'L')
        if is_label_cmd:
            cmds.append(l.strip() + chr(13))
            l = []
            break
        for i in range(1, len(l)):
            is_cmd_char = (l[i] >= 'A') and (l[i] <= 'Z')
            if is_cmd_char:
                cmds.append(l[0:i].strip() + ' ')
                l = l[i:]
                break
        else:
            if l:
                cmds.append(l.strip() + ' ')
                break
    return cmds

parser = argparse.ArgumentParser(description='Send a script to a Penman')
parser.add_argument('--tx-only', action='store_true', default=False, help='Only TX characters; don''t wait for RX responses')
parser.add_argument('--log', action='store_true', default=False, help='Log all serial I/O')
parser.add_argument('port', default='/dev/ttyUSB0', nargs='?', help='The port to communicate on')
args = parser.parse_args()

if args.log:
    log = Log()
else:
    log = None
penman = Penman(args.port, args.tx_only, log)

for l in sys.stdin.readlines():
    l = l.strip()
    if not l:
        continue
    for cmd in split_commands(l):
        penman.send(cmd)
