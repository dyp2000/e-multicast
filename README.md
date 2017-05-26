# Сборка

	make get-deps
	make

# Запуск сервера

	./emc debug -infile </path/to/file> -mcgrp 224.2.2.4 -speed 5000000

# Запуск клиента

	erl -s emc_cli -url http://ip.address:8088/get_meta
	
