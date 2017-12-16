
hevm exec  \
	--code "$(cat out.vm)" \
	--calldata 00 \
	--gas 1000000 \
	--value 5 \
	--debug

