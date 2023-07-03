parseString(String,ParsedString):-downcase_atom(String,LowercaseString),
                                  atom_codes(LowercaseString,AsciiList),
				  filtrarSimbolos(AsciiList,AsciiNoSymbolList),
                                  parsearPalabras(AsciiNoSymbolList,NoSymbolString),
				  filtraVacia(NoSymbolString,ParsedString), !.

filtrarSimbolos([], []).
filtrarSimbolos([X|Xs],Ys) :- esSimbolo(X), filtrarSimbolos(Xs, Ys).
filtrarSimbolos([X|Xs],[X|Ys]) :- filtrarSimbolos(Xs, Ys).

esSimbolo(X) :- between(33,64, X); between(91,96,X); between(123,191,X).

parsearPalabras([], []).
parsearPalabras(Str, [X|Xs]) :- parseaPalabra(Str, Y, NuevaStr), atom_codes(X, Y),
	parsearPalabras(NuevaStr, Xs).

parseaPalabra([], [], []).
parseaPalabra([32|Xs], [], Xs).
parseaPalabra([X|Xs], [X|Ys], NuevaS) :- \+ X = 32, parseaPalabra(Xs, Ys, NuevaS).

filtraVacia([],[]).
filtraVacia([''|Xs],Y) :- filtraVacia(Xs,Y).
filtraVacia([X|Xs],Y) :- \+ X = '', enlace(X), filtraVacia(Xs, Y).
filtraVacia([X|Xs], [X|Ys]) :- \+ X = '', \+ enlace(X), filtraVacia(Xs, Ys).

respuestas(Fichero, [], _) :- at_end_of_stream(Fichero).
respuestas(Fichero,[], 4). 
respuestas(Fichero, [ParsedY|Ys], C) :- \+ at_end_of_stream(Fichero),
						read(Fichero, Y),
						parseString(Y,ParsedY),
						D is C+1,
						respuestas(Fichero, Ys, D).

respuestasLiteral(Fichero, [], _) :- at_end_of_stream(Fichero).
respuestasLiteral(Fichero,[], 4). 
respuestasLiteral(Fichero, [Y|Ys], C) :- \+ at_end_of_stream(Fichero),
						read(Fichero, Y),
						D is C+1,
						respuestasLiteral(Fichero, Ys, D).

% funciones auxiliares para lista de palabras clave
enLista(_, []) :- false.
enLista(X,[Y|_]):-sinon(X,Y), !.
enLista(X,[Y|Xs]) :- \+ sinon(X,Y), enLista(X, Xs).

quitarLista(C, [X|Xs], Xs) :- sinon(C,X), !.
quitarLista(C, [X|Xs], [X|Ys]) :- \+ sinon(C,X), quitarLista(C, Xs, Ys).

% true iff ambas preguntas son la misma
matchPregunta([], []).
matchPregunta([], [_|_]).
matchPregunta([_|_],[]).
matchPregunta([Pa|Pas], P) :- enLista(Pa, P), quitarLista(Pa, P, Ps),
		 matchPregunta(Pas, Ps), !.

% true iff una pregunta es la otra negada
matchNoPregunta([],N):- enLista('no',N).
matchNoPregunta(['no'],_).
matchNoPregunta([Pa|Pas], P) :- enLista(Pa, P), quitarLista(Pa, P, Ps), matchNoPregunta(Pas, Ps).

% palabras enlace
enlace('nos').
enlace('la'). enlace('las'). enlace('el'). enlace('los'). enlace('lo').
enlace('un'). enlace('una').
enlace('y'). enlace('de'). enlace('en'). enlace('a'). enlace('para').
enlace('al'). enlace('se'). enlace('que'). enlace('cuáles').
enlace('qué').
enlace('endoffile').
enlace('b'). enlace('c'). enlace('d').
enlace('es'). enlace('son'). enlace(satisfacer).

sinon(X,X).
sinon('principales','primarias').
sinon('cosa','algo').
sinon('necesario', 'necesita').
sinon(dignamente, digna).
sinon(vivir, vida).
%sinon('','').

first([X|_],X).

cogerPregunta(P,[X|_],X) :- X = [Y|_], matchPregunta(P,Y), !.
cogerPregunta(P,[X|Xs],R) :- X = [Y|_],\+ matchPregunta(P,Y), 
		cogerPregunta(P,Xs,R).

cogerCorrecta([_,_,_,_,X],X).

encontrarRespuesta(X,[Y|_],Y) :- parseString(Y,ParsedY), matchPregunta(X,ParsedY), !.
encontrarRespuesta(X,[Y|Ys],R) :- parseString(Y,ParsedY), \+ matchPregunta(X,ParsedY),
	 encontrarRespuesta(X,Ys,R).

respuestaTest(Fichero, [])  :- at_end_of_stream(Fichero).
respuestaTest(Fichero, [[X|Y]|L])  :- \+ at_end_of_stream(Fichero),
	read(Fichero,X),
	respuestasLiteral(Fichero,Y,0),
	respuestaTest(Fichero, L), !.

pregunta(Fichero, [])  :- at_end_of_stream(Fichero).
pregunta(Fichero, [[ParsedX|Y]|L])  :- \+ at_end_of_stream(Fichero),
	read(Fichero,X),
	parseString(X,ParsedX),
	atom_codes(X,Z),
	respuestas(Fichero,Y,0),
	pregunta(Fichero, L), !.

resolverTest(_,[]).
resolverTest(Z,[X|Xs]) :- resolverPregunta(X,Z,P,Ans),
		     write('Pregunta: '), write(P), nl,
		     write('Respuesta: '), write(Ans), nl,
		     resolverTest(Z,Xs).

resolverPregunta(Pregunta,Z,P,Ans) :- first(Pregunta, X),
					       parseString(X,ParsedX),
				               cogerPregunta(ParsedX, Z, R),
				               cogerCorrecta(R, C),
				               Pregunta = [P|Respuestas],
				               encontrarRespuesta(C, Respuestas, Ans).

main :- open('./test.txt', read, Test),
	open('./train.txt', read, Train),
	respuestaTest(Test, T),
	pregunta(Train, Z),
	resolverTest(Z, T),
	close(Test),
	close(Train).
