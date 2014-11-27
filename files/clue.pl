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
	write('which suspects are used in this version of the game?'),nl,
	read([H|T]), assertz(suspect(H)),savesuspect(T).

numsetters:-
	write('how many players are in this game?'),nl,
	read(Nump), savenumber(Nump).

turnsetters:-
	write('when is your turn in number?'),nl,
	read(Numt), saveturn(Numt).
mycardsetters:-
	write('what cards are you holding?'),nl,
	read(Cards), savecards(Cards).
saveroom([]).
saveroom([H|T]):-( not(alreadyindb(room,H))->assertz(room(H)),saveroom(T),addinfer(room,H);
					saveroom(T)).

saveweapon([]).
saveweapon([H|T]):-( not(alreadyindb(weapon,H))->assertz(weapon(H)),saveweapon(T),addinfer(weapon,H);
					saveweapon(T)).
savesuspect([]).
savesuspect([H|T]):-( not(alreadyindb(suspect,H))->assertz(suspect(H)),savesuspect(T),addinfer(suspect,H);
					savesuspect(T)).

savenumber(X):-assertz(nump(X)).

saveturn(X):-assertz(turn(X)).

savecards([]).
savecards([H|T]):- assertz(mycards(H)), retractcard(H), savecards(T).


alreadyindb(X,Y):-(X==room -> room(Y);
					X == weapon -> weapon(Y);
					X == deletedroom -> deletedroom(Y);
					X == deletedsuspect -> deletedsuspect(Y);
					X == deletedweapon -> deletedweapon(Y);
					suspect(Y)
					).

retractcard(X):-(alreadyindb(room, X)->  retract(room(X)),assertz(deletedroom(X)),retractinfer(room,X);
				alreadyindb(suspect,X) ->retract(suspect(X)),assertz(deletedsuspect(X)),retractinfer(suspect,X);
				retract(weapon(X)),assertz(deletedweapon(X)),retractinfer(weapon,X)
				).

games:-
	write('Choose the following option'),nl,
	write('1 ) make a suggestion'), nl,
	write('2 ) view suggestion history'), nl,
	write('3 ) see possible suspects/weapons/places'), nl,
	write('4 ) adding a suspicious card'),nl,
	write('5 ) delete a suspicious card'),nl,
	write('6 ) make me a suggestion'),nl,
	write('7 ) infer from other people\'s suggestion'),nl,
	write('8 ) make me a suggestion that will trick other players'),nl,
	write('0 ) quit'),nl,nl,
	read(Choice), ( Choice == 1 -> nl, makesuggestion;
					Choice == 2 -> nl, getsuggestions, nl,games; 
					Choice == 3 -> nl, printpossibility, nl, games;
					Choice == 4 -> nl, addcard, nl;
					Choice == 5 -> nl, deletecard, nl;
					Choice == 7 -> nl, inference, nl, games;
					Choice == 8 -> nl, makewrongsuggestion, games;
					Choice == 0 -> write('good bye');
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
					(foundcomb(A,B,C)->write('The Criminal is '),write(A),write(' who killed in '),write(B),write(' using '),
						write(C)

						;

						games

						)
					).	

foundanswer:- countnumbers(room,1),countnumbers(weapon,1),countnumbers(suspect, 1).

countnumbers(X,Y) :-
	(X==room -> findall(N, room(N), Ns);
		X==weapon->	findall(N, weapon(N), Ns);
	findall(N, suspect(N), Ns)
	),
    length(Ns, Y).


foundcomb(A,B,C):-alreadyindb(suspect,A),alreadyindb(room,B),alreadyindb(weapon,C).


getsuggestions:-
	findall((A,B,C),suggestion(A,B,C),Ns),printsuggestion(Ns).

printsuggestion([]).
printsuggestion([(A,B,C)|T]):-write('you suggested that '), write(A), write(' killed in '), write(B),
 write(' using '), write(C), nl, printsuggestion(T).

printpossibility:-
	printsuspects,printplaces,printweapons.

printsuspects:-
	write('Possible suspects that are left are '), findall(A,suspect(A),Ns), write(Ns), nl.

printplaces:-
	write('Possible room that are left are '), findall(A,room(A),Ns), write(Ns), nl.	
printweapons:-
	write('Possible weapons that are left are '), findall(A,weapon(A),Ns), write(Ns), nl.

addcard:-
	write('What is the type of the card?'),nl,
	read(Card),
	(
		Card == room-> addindb(room);
		Card == weapon-> addindb(weapon);
		addindb(suspect)
	),(foundanswer-> room(D),weapon(E),suspect(F),write('All that left is criminal ')
						,write(F), write(' in room '),write(D), write(' using weapon '), write(E);
						games
						).

addindb(X):-
	write('What is the value of the card?'), nl, read(Value),
	(X == room->  assertz(room(Value));
		X == weapon->assertz(weapon(Value));
				assertz(suspect(Value))
		).


deletecard:-
	write('What is the type of the card?'),nl,
	read(Card),
	(
		Card == room-> deleteindb(room);
		Card == weapon-> deleteindb(weapon);
		deleteindb(suspect)
	).

deleteindb(X):-
	write('What is the value of the card?'), nl, read(Value),
	(X == room->  retract(room(Value)),assertz(deletedroom(Value));
		X == weapon->retract(weapon(Value)),assertz(deletedweapon(Value));
				retract(suspect(Value)),assertz(deletedsuspect(Value))
		),(foundanswer-> room(D),weapon(E),suspect(F),write('All that left is criminal ')
						,write(F), write(' in room '),write(D), write(' using weapon '), write(E);
						games
						).

makewrongsuggestion:-
	findall(L, deletedroom(L), Ls),findall(M, deletedweapon(M), Ms),findall(N, deletedsuspect(N), Ns),
    length(Ls,X),length(Ms, Y),length(Ns,Z),
    random(0,X,Xr),random(0,Y,Yr),random(0,Z,Zr),
    random_member(Lx, Ls),random_member(Mx, Ms),random_member(Nx, Ns),
    write('try tricking them with suspect '), write(Nx), write(' with weapon '), write(Mx), write(' in '), write(Lx),nl.

inference:-
	write('Who was the suspect in other people\'s suggestion?'), nl,
	read(Suspect),nl,
	write('What was the weapon in other people\'s suggestion?'), nl,
	read(Weapon),nl,
	write('What was the room in other people\'s suggestion?'), nl,
	read(Room),nl,
	write('Did they find a card? (1 for yes, 0 for no)'),nl,
	read(Yesorno),nl,
	(Yesorno==1->
	(alreadyindb(deletedroom,Room),alreadyindb(deletedsuspect, Suspect),not(alreadyindb(deletedweapon,Weapon))->
		write('It can be inferred that '), write(Weapon), write(' can be removed'), retractcard(Weapon);
	not(alreadyindb(deletedroom,Room)),alreadyindb(deletedsuspect, Suspect),alreadyindb(deletedweapon,Weapon)->
		write('It can be inferred that '), write(Room), write(' can be removed'), retractcard(Room);
	alreadyindb(deletedroom,Room),not(alreadyindb(deletedsuspect, Suspect)),alreadyindb(deletedweapon,Weapon)->
		write('It can be inferred that '), write(Suspect), write(' can be removed'), retractcard(Suspect);
		write('Nothing can be inferred at this point')
		);
		write('Nothing can be inferred at this point')
	).


makeguess:-
	write('Choose the following option'),nl,
	write('1 ) make a random possible guess'), nl,
	write('2 ) make a possible guess based on chance'), nl,
	write('3 ) see possible suspects/weapons/places'), nl,
	write('4 ) adding a suspicious card'),nl,
	write('5 ) delete a suspicious card'),nl,
	write('6 ) make me a suggestion'),nl,
	write('7 ) infer from other people\'s suggestion'),nl,
	write('8 ) make me a suggestion that will trick other players'),nl,
	write('0 ) go back'),nl,nl,
	read(Choice), ( Choice == 1 -> nl, randomguess,games;
					Choice == 2 -> nl, getsuggestions, nl,games; 
					Choice == 3 -> nl, printpossibility, nl, games;
					Choice == 4 -> nl, addcard, nl;
					Choice == 5 -> nl, deletecard, nl;
					Choice == 7 -> nl, inference, nl, games;
					Choice == 8 -> nl, makewrongsuggestion, games;
					games).

randomguess:-
	findall(X,room(X),Xs),findall(Y,suspect(Y),Ys),findall(Z,weapon(Z),Zs),
	random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
	write('The randome guess is that the murderer is '), write(Yx), write(' in room '),
	write(Xx), write(' using weapon '), write(Zx).
addinfer(X,Y):-
	(
		X==room->assertz(inferroom(Y));
		X==weapon->assertz(inferweapon(Y));
		assertz(infersuspect(Y))
		).

retractinfer(X,Y):-
	(
		X==room = retractall(inferroom(Y));
		X==weapon = retractall(inferweapon(Y));
		retractall(infersuspect(Y))
		).

geteducatedguess(X):-
	(X==room->findall(X,inferroom(X),Xs),findall(Y,deletedweapon(Y),Ys),findall(Z,deletedsuspect(Z),Zs),
		random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
		write('The educated guess on suspect domain is that the murderer is '), write(Zx), write(' in room '),
		write(Xx), write(' using weapon '), write(Yx);
		X==weapon->findall(X,deletedroom(X),Xs),findall(Y,inferweapon(Y),Ys),findall(Z,deletedsuspect(Z),Zs),
		random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
		write('The educated guess on suspect domain is that the murderer is '), write(Zx), write(' in room '),
		write(Xx), write(' using weapon '), write(Yx);
		X==suspect->findall(X,deletedroom(X),Xs),findall(Y,deletedweapon(Y),Ys),findall(Z,infersuspect(Z),Zs),
		random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
		write('The educated guess on suspect domain is that the murderer is '), write(Zx), write(' in room '),
		write(Xx), write(' using weapon '), write(Yx);
		findall(X,inferroom(X),Xs),findall(Y,inferweapon(Y),Ys),findall(Z,infersuspect(Z),Zs),
		random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
		write('The educated guess on every domain is that the murderer is '), write(Zx), write(' in room '),
		write(Xx), write(' using weapon '), write(Yx)
		).

