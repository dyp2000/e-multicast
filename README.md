## Start server:
	
	$ ./emc debug -infile /path/to/file -mcgrp 224.2.2.4 -speed 500000

* `-infile <имя_файла>` - имя файла для передачи с полным путем
* `-mcgrp <ip>` - ip адрес мультикаст группы
* `-speed <байты_в_сек>` - скорость передачи в байтах в секунду

## Start client

	$ erl -s emc_cli -url http://host_adress:8088/data/get_meta

