(mk-tower
 'p_eastpass "Eastpass"
	(list
		".. {4 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		".. {4 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		".. .. {5 {{ {{ {{ {{ xx ,, xx {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		".. .. {4 {{ {{ {{ {{ w+ ,, w+ {5 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ "
		".. .. .. {1 {1 {1 {1 xx ,, xx .. {1 {5 {{ ^^ ^^ ^^ ^^ ^^ "
		".. .. .. .. .. xx xx xx ,, xx xx xx {4 {{ {{ ^^ ^^ ^^ ^^ "
		".. .. .. .. .. w+ ,, ,, ,, ,, ,, w+ .. {5 {{ {{ ^^ ^^ ^^ "
		".. .. .. .. .. x! ,, ,, ,, ,, ,, x! .. .. {5 {{ {{ ^^ ^^ "
		".. .. .. .. .. w+ ,, ,, ,, ,, ,, w+ .. .. .. {5 {{ {{ ^^ "
		".. .. .. .. .. ,, ,, ,, ,, ,, ,, ,, .. .. .. .. {d {{ ^^ "
		".. .. .. .. .. w+ ,, ,, ,, ,, ,, w+ .. .. .. {c {{ {{ ^^ "
		".. .. .. .. .. x! ,, ,, ,, ,, ,, x! .. .. {4 {{ {{ ^^ ^^ "
		".. .. .. .. .. w+ ,, ,, ,, ,, ,, w+ .. .. {c {{ ^^ ^^ ^^ "
		".. .. .. .. .. xx xx xx ,, xx xx xx .. {c {{ {{ ^^ ^^ ^^ "
		".. .. .. .. .. .. .. xx ,, xx .. .. {c {{ {{ ^^ ^^ ^^ ^^ "
		".. .. .. {8 {8 {8 {8 w+ ,, w+ {8 {c {{ {{ ^^ ^^ ^^ ^^ ^^ "
		".. .. {c {{ {{ {{ {{ xx ,, xx {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ "
		".. {4 {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		".. {4 {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
	)
	nil
 (put (mk-ladder-down 'p_westpass 4 14) 14 9)
 (put (mk-windowed-door) 11 9)
 (put (mk-windowed-door)  5 9)

 (put (guard-pt 'knight)  7 8)
 (put (guard-pt 'knight)  9 10)
 (put (guard-pt 'squire)  7 10)
 (put (guard-pt 'squire)  9 8)

 (put (spawn-pt 'bandit) 3 8)
 (put (spawn-pt 'bandit) 4 9)
 )

(mk-place-music p_eastpass 'ml-castle)

(kern-place-set-gob
 p_eastpass
 (tbl-build
  'description "a guarded passage under the mountains"
  ))
