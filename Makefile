debug : disk
	# generate a monitor file from the symbol file
	awk -F' |=\\$$' '{print "al " $$3 " ." $$2}' primes.sym > primes.mon

	# Read the source file, and add any 'vars' to  the monitor file
	grep "^.var.*\\$$" primes.s | awk -F' | = \\$$' '{print "al " $$3 " ." $$2} ' >> primes.mon

	# Add a break line so that basic doesn't fubar my zeropage.
	#echo "break .start" >> primes.mon
	echo "break .break" >> primes.mon
	echo "break .break2" >> primes.mon
	#echo "break .end_prg" >> primes.mon
	x64.app/Contents/MacOS/x64 -ntsc -warp -moncommands primes.mon primes.d64
	#x64.app/Contents/MacOS/x64 -ntsc +warp -moncommands primes.mon primes.d64

disk : primes
	x64.app/Contents/MacOS/c1541 -format blah,00 d64 primes.d64 -attach primes.d64 -write primes.prg primes

primes : primes.s
	java -jar kick/KickAss.jar primes.s




