lochistory(Ketchup, kitchen).
lochistory(Ketchup, spa).
lochistory(Ketchup, theatre).

lochistory(Victim, theatre).
lochistory(Victim, observatory).

victim(loc):-write('Victim was killed in theatre').
victim(weapon):-write('Victim was killed with a rope').
touchhistory(Ketchup, rope).


suggest(Ketchup, theatre, rope):-write('correct').
suggest(Ketchup, X, Y):-write('not enough proof!').