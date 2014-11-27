% CPSC 312, Project 2
% Authors
% Name				Student number		CS id
% Yoonsung Ahn 		29838091			r4j8
% Ki Bum Kim		64650088			c4i7



%
%	This program starts with begin. This program uses probability to guess which combination is the correct combination
%	This program allows you to make a suggestion, see your past suggestions, track turn numbers, see possible options for you, make a guess for you
%	with variety of options, and also make educated from other people's suggestions.
%

%
%	This program tries to obtain information from other people's guesses and uses probability to determine the card combination that is likely
%	to be a correct combination. If it is inferred that the colonel Mustard has higher chance of being a card murderer than Mrs. White, the guess
%	that the program makes has higher chance to give Col. Mustard. However, since this is based on probability, there is also a chance Mrs. White is chosen in the guess
% 	given by the program. The program also can retract card from a database from hearing other people's suggestion if it thinks that the card is clearly
%	not in the correct combination.


% A start method, user can add inputs to start the game

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


% game variable setters

roomsetters:-
	write('Please give me rooms are used in this version of the game in list(example: [dining, theatre])'),nl,
	read([H|T]),assertz(room(H)),saveroom(T).

weaponsetters:-
	write('Please give me weapons that are used in this game in list(example: [knife,rope])'),nl,
	read([H|T]),assertz(weapon(H)),saveweapon(T).

suspectsetters:-
	write('Please give me suspects that are used in this game in list(example: [mustard])'),nl,
	read([H|T]), assertz(suspect(H)),savesuspect(T).

numsetters:-
	write('How many players are in this game?'),nl,
	read(Nump), savenumber(Nump).

turnsetters:-
	write('When is your turn in number?'),nl,
	read(Numt), saveturn(Numt).

mycardsetters:-
	write('What cards are you holding?'),nl,
	read(Cards), savecards(Cards).

%	Game variable setters helper functions.
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

saveturn(X):-assertz(myturn(X)), assertz(turn(1)).

savecards([]).
savecards([H|T]):- assertz(mycards(H)), retractcard(H), savecards(T).

% Checks through database to find if it already exists.

alreadyindb(X,Y):-(X==room -> room(Y);
					X == weapon -> weapon(Y);
					X == deletedroom -> deletedroom(Y);
					X == deletedsuspect -> deletedsuspect(Y);
					X == deletedweapon -> deletedweapon(Y);
					suspect(Y)
					).

% Delete from database.

retractcard(X):-(alreadyindb(room, X)->  retract(room(X)),assertz(deletedroom(X)),retractinfer(room,X);
				alreadyindb(suspect,X) ->retract(suspect(X)),assertz(deletedsuspect(X)),retractinfer(suspect,X);
				retract(weapon(X)),assertz(deletedweapon(X)),retractinfer(weapon,X)
				).

% The game function, this allows user to navigate through different options to make best use of program
% The player can type 0~8 to navigate through program. 

games:-
	getturn,
	write('Choose the following option'),nl,
	write('1 ) Make a suggestion'), nl,
	write('2 ) View suggestion history'), nl,
	write('3 ) See possible suspects/weapons/places'), nl,
	write('4 ) Add a suspicious card'),nl,
	write('5 ) Delete a suspicious card'),nl,
	write('6 ) Make me a suggestion'),nl,
	write('7 ) Infer from other people\'s suggestion'),nl,
	write('8 ) End current turn'),nl,	
	write('0 ) Quit'),nl,
	read(Choice), ( Choice == 1 -> nl, makesuggestion;
					Choice == 2 -> nl, getsuggestions, nl,games; 
					Choice == 3 -> nl, printpossibility, nl, games;
					Choice == 4 -> nl, addcard, nl;
					Choice == 5 -> nl, deletecard, nl;
					Choice == 6 -> nl, makeguess, nl, games;
					Choice == 7 -> nl, inference, nl, games;
					Choice == 8 -> nl, nextturn, nl, games;
					write('good bye')).


% Add a suggestion in database. If a user has discovered a new information, update the databse.

makesuggestion:-
	write('Write suggestion in following manner [person, place, weapon]'),nl,
	read([A,B,C]), write('You suggested that '), write(A), write(' killed in '), write(B), write(' with a '), write(C),
	assertz(suggestion(A,B,C)), nl, write('Did you discover anything? '),nl,
	write('1 ) Yes'), nl,
	write('2 ) No'), nl,
	read(Choice), (Choice == 1 -> write('What card did you see?'), nl,read(Card),  retractcard(Card),

					(foundanswer-> room(D),weapon(E),suspect(F),write('All that left is criminal ')
						,write(F), write(' in room '),write(D), write(' using weapon '), write(E),nl,games;
						nl,games
						)
					;
					(foundcomb(A,B,C)->write('The Criminal is '),write(A),write(' who killed in '),write(B),write(' using '),
						write(C),nl,games

						;

						nl,games

						)
					).	

% true, if win condition is met.

foundanswer:- countnumbers(room,1),countnumbers(weapon,1),countnumbers(suspect, 1).

% Count the number of elements in the list

countnumbers(X,Y) :-
	(X==room -> findall(N, room(N), Ns);
		X==weapon->	findall(N, weapon(N), Ns);
	findall(N, suspect(N), Ns)
	),
    length(Ns, Y).

% Used to return the answer.

foundcomb(A,B,C):-alreadyindb(suspect,A),alreadyindb(room,B),alreadyindb(weapon,C).

% Used to print out the history of suggestions

getsuggestions:-
	findall((A,B,C),suggestion(A,B,C),Ns),printsuggestion(Ns).

printsuggestion([]).
printsuggestion([(A,B,C)|T]):-write('You suggested that '), write(A), write(' killed in '), write(B),
 write(' using '), write(C), nl, printsuggestion(T).

% Used to view lists of possible suspects, weapons, and rooms

printpossibility:-
	printsuspects,printplaces,printweapons.

printsuspects:-
	write('Possible suspects that are left are '), findall(A,suspect(A),Ns), write(Ns), nl.

printplaces:-
	write('Possible room that are left are '), findall(A,room(A),Ns), write(Ns), nl.	
printweapons:-
	write('Possible weapons that are left are '), findall(A,weapon(A),Ns), write(Ns), nl.

% Used to manually add card, if user wants

addcard:-
	write('What is the type of the card?'),nl,
	read(Card),
	(
		Card == room-> addindb(room);
		Card == weapon-> addindb(weapon);
		addindb(suspect)
	),(foundanswer-> room(D),weapon(E),suspect(F),write('All that left is criminal ')
						,write(F), write(' in room '),write(D), write(' using weapon '), write(E), nl,games;
						nl,games
						).

addindb(X):-
	write('What is the value of the card?'), nl, read(Value),
	(X == room->  assertz(room(Value));
		X == weapon->assertz(weapon(Value));
				assertz(suspect(Value))
		).

% Used to manually delete card, if user wants.

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
						,write(F), write(' in room '),write(D), write(' using weapon '), write(E),nl,games;
						nl,games
						).

% Used to create suggestion that is wrong, if user needs it.

makewrongsuggestion:-
	findall(L, deletedroom(L), Ls),findall(M, deletedweapon(M), Ms),findall(N, deletedsuspect(N), Ns),
    length(Ls,X),length(Ms, Y),length(Ns,Z),
    random(0,X,Xr),random(0,Y,Yr),random(0,Z,Zr),
    random_member(Lx, Ls),random_member(Mx, Ms),random_member(Nx, Ns),
    write('try tricking them with suspect '), write(Nx), write(' with weapon '), write(Mx), write(' in '), write(Lx),nl.

% Used to learn from other people's suggestioins

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
		write('It can be inferred that '), write(Weapon), write(' can be removed'), nl,retractcard(Weapon),
		(foundanswer->
		foundcomb(A,B,C)->write('The Criminal is '),write(A),write(' who killed in '),write(B),write(' using '),
						write(C),nl; 	nl);
	not(alreadyindb(deletedroom,Room)),alreadyindb(deletedsuspect, Suspect),alreadyindb(deletedweapon,Weapon)->
		write('It can be inferred that '), write(Room), write(' can be removed'), retractcard(Room), nl,
		(foundanswer->
		foundcomb(A,B,C)->write('The Criminal is '),write(A),write(' who killed in '),write(B),write(' using '),
						write(C),nl; 	nl);
	alreadyindb(deletedroom,Room),not(alreadyindb(deletedsuspect, Suspect)),alreadyindb(deletedweapon,Weapon)->
		write('It can be inferred that '), write(Suspect), write(' can be removed'), retractcard(Suspect), nl,
		(foundanswer->
		foundcomb(A,B,C)->write('The Criminal is '),write(A),write(' who killed in '),write(B),write(' using '),
						write(C),nl; 	nl);
	alreadyindb(deletedroom,Room),not(alreadyindb(deletedsuspect, Suspect)),not(alreadyindb(deletedweapon,Weapon))->
		write('It can be inferred that '), write(Suspect), write(' and '),write(Weapon),write(' are not likely'),
		findall(X,suspect(X),Xs),findall(Y,weapon(Y),Ys), 
		inferother(suspect, Suspect, Xs),inferother(weapon, Weapon, Ys);
	not(alreadyindb(deletedroom,Room)),alreadyindb(deletedsuspect, Suspect),not(alreadyindb(deletedweapon,Weapon))->
		write('It can be inferred that '), write(Room), write(' and '),write(Weapon),write(' are not likely'),
		findall(X,room(X),Xs),findall(Y,weapon(Y),Ys), 
		inferother(room, Room, Xs),inferother(weapon, Weapon, Ys);
	not(alreadyindb(deletedroom,Room)),not(alreadyindb(deletedsuspect, Suspect)),alreadyindb(deletedweapon,Weapon)->
		write('It can be inferred that '), write(Suspect), write(' and '),write(Room),write(' are not likely'),
		findall(X,suspect(X),Xs),findall(Y,room(Y),Ys), 
		inferother(suspect, Suspect, Xs),inferother(room, Room, Ys);
	not(alreadyindb(deletedroom,Room)),not(alreadyindb(deletedsuspect, Suspect)),not(alreadyindb(deletedweapon,Weapon))->
		write('It can be inferred that '), write(Suspect), write(' and '),write(Room),write(' and '),write(Weapon),write(' are not likely'),
		findall(X,suspect(X),Xs),findall(Y,room(Y),Ys), findall(Z,weapon(Z),Zs),
		inferother(suspect, Suspect, Xs),inferother(room, Room, Ys),inferother(weapon,Weapon,Zs);
		write('Nothing can be inferred at this point')
		);
		write('Nothing can be inferred at this point')
	).

% Used to display possible suggestions
makeguess:-
	write('Choose the following option'),nl,
	write('1 ) Make a random possible guess'), nl,
	write('2 ) Make an educated guess on every domain'), nl,
	write('3 ) Make an educated guess for suspect'), nl,
	write('4 ) Make an educated guess for room'),nl,
	write('5 ) Make an educated guess for weapon'),nl,
	write('6 ) Make a wrong guess to throw other players off'),nl,
	write('0 ) Go back'),nl,
	read(Choice), ( Choice == 1 -> nl, randomguess,nl;
					Choice == 2 -> nl, geteducatedguess(all),nl; 
					Choice == 3 -> nl, geteducatedguess(suspect), nl;
					Choice == 4 -> nl, geteducatedguess(room), nl;
					Choice == 5 -> nl, geteducatedguess(weapon), nl;
					Choice == 6 -> nl, makewrongsuggestion, nl;
					games).

% Create purely random possible combination
randomguess:-
	findall(X,room(X),Xs),findall(Y,suspect(Y),Ys),findall(Z,weapon(Z),Zs),
	random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
	write('The randome guess is that the murderer is '), write(Yx), write(' in room '),
	write(Xx), write(' using weapon '), write(Zx).

% Scoring functions on each value for domain
addinfer(X,Y):-
	(
		X==room->assertz(inferroom(Y));
		X==weapon->assertz(inferweapon(Y));
		assertz(infersuspect(Y))
		).
inferother(X,Y,[H|T]):-
	(
		not(H==Y),T==[]->addinfer(X,H),true;
		H==Y,T==[]->true;
		H==Y->inferother(X,Y,T);
		addinfer(X,H),inferother(X,Y,T)
		).

retractinfer(X,Y):-
	(
		X==room->retractall(inferroom(Y));
		X==weapon->retractall(inferweapon(Y));
		retractall(infersuspect(Y))
		).

% Make educated guess.

geteducatedguess(Type):-
	(Type==room->
		findall(X,inferroom(X),Xs),findall(Y,deletedweapon(Y),Ys),findall(Z,deletedsuspect(Z),Zs), 
		random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
		write('The educated guess on suspect domain is that the murderer is '), write(Zx), write(' in room '),
		write(Xx), write(' using weapon '), write(Yx);
		Type==weapon->
		findall(X,deletedroom(X),Xs),findall(Y,inferweapon(Y),Ys),findall(Z,deletedsuspect(Z),Zs),
		random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
		write('The educated guess on suspect domain is that the murderer is '), write(Zx), write(' in room '),
		write(Xx), write(' using weapon '), write(Yx);
		Type==suspect->
		findall(X,deletedroom(X),Xs),findall(Y,deletedweapon(Y),Ys),findall(Z,infersuspect(Z),Zs),
		random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
		write('The educated guess on suspect domain is that the murderer is '), write(Zx), write(' in room '),
		write(Xx), write(' using weapon '), write(Yx);
		findall(X,inferroom(X),Xs),findall(Y,inferweapon(Y),Ys),findall(Z,infersuspect(Z),Zs),
		random_member(Xx,Xs),random_member(Yx,Ys),random_member(Zx,Zs),
		write('The educated guess on every domain is that the murderer is '), write(Zx), write(' in room '),
		write(Xx), write(' using weapon '), write(Yx)
		).

% Get the next turn

nextturn:-
	turn(X), X1 is X + 1, write(X1),retract(turn(X)),nump(Y),
	(
		X>Y->assertz(turn(1));
		assertz(turn(X1))
		).

% Get the current turn and display

getturn:-
	turn(X),myturn(Y),
	(
		X==Y-> write('It is now your turn');
		write('It is now player '),write(X),write('\'s turn')
		),nl.