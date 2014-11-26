
begin:-
	write('Welcome to clue helper bot'), nl, nl,
	roomsetters,nl,
	weaponsetters, nl,
	suspectsetters, nl,
	numsetters, nl,
	turnsetters, nl,
	mycardsetters, nl,
	write('The game has started'),nl,nl,
	games.



roomsetters:-
	write('which rooms are used in this version of the game?'),nl,
	read([H|T]),assertz(room(H)),saveroom(T).

weaponsetters:-
	write('which weapons are used in this version of the game?'),nl,
	read([H|T]),assertz(weapon(H)),saveweapon(T).

suspectsetters:-
	write('which weapons are used in this version of the game?'),nl,
	read([H|T]), assertz(suspect(H)),savesuspect(T).

numsetters:-
	write('how many players are in this game?'),nl,
	read(Nump), savenumber(Nump).

turnsetters:-
	write('when is your turnsetters in number?'),nl,
	read(Numt), saveturn(Numt).
mycardsetters:-
	write('what cards are you holding?'),nl,
	read(Cards), savecards(Cards).
saveroom([]).
saveroom([H|T]):-( not(alreadyindb(room,H))->assertz(room(H)),saveroom(T);
					saveroom(T)).

saveweapon([]).
saveweapon([H|T]):-( not(alreadyindb(weapon,H))->assertz(weapon(H)),saveweapon(T);
					saveweapon(T)).
savesuspect([]).
savesuspect([H|T]):-( not(alreadyindb(suspect,H))->assertz(suspect(H)),savesuspect(T);
					savesuspect(T)).

savenumber(X):-assertz(nump(X)).

saveturn(X):-assertz(turn(X)).

savecards([]).
savecards([H|T]):- assertz(mycards(H)), retractcard(H), savecards(T).


alreadyindb(X,Y):-(X==room -> room(Y);
					X == weapon -> weapon(Y);
					suspect(Y)
					).



retractcard(X):-(alreadyindb(room, X)->  retract(room(X));
				alreadyindb(suspect,X) ->retract(suspect(X));
				retract(weapon(X))
				).

games:-
	write('Choose the following option'),nl,
	write('1 ) make a suggestion'), nl,
	write('2 ) view suggestioni history'), nl,
	write('3 ) see what I learne,'), nl,
	read(Choice), ( Choice  == 1 -> write('suggestion chosen'), nl, makesuggestion;
					Choice == 2 -> write('view suggestion'); 
					write('learning history choice')).

makesuggestion:-
	write('write suggestion in following manner [person, place, weapon]'),nl,
	read([A,B,C]), write('you suggested that '), write(A), write(' killed in '), write(B), write(' with a '), write(C),
	assertz(suggestion(A,B,C)), nl, write('Did you discover anything? '),nl,
	write('1 ) yes'), nl,
	write('2 ) no'), nl,
	read(Choice), (Choice == 1 -> write('what card did you see?'), read(Card),  retractcard(Card),

					(foundanswer-> room(D),weapon(E),suspect(F),write('All that left is criminal ')
						,write(F), write(' in room '),write(D), write(' using weapon '), write(E);
						games
						)
					;

					games
					).	

foundanswer:- countnumbers(room,1),countnumbers(weapon,1),countnumbers(suspect, 1).

countnumbers(X,Y) :-
	(X==room -> findall(N, room(N), Ns);
		X==weapon->	findall(N, weapon(N), Ns);
	findall(N, suspect(N), Ns)
	),
    length(Ns, Y).



