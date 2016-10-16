/*
   Programacao Logica - Prof. Alexandre G. Silva - 30set2015
   RECOMENDACOES:
   - O nome deste arquivo deve ser 'programa.pl'
   - O nome do banco de dados deve ser 'desenhos.pl'
   - Dicas de uso podem ser obtidas na execucação:
     ?- menu.
   - Exemplo de uso:
     ?- load.
     ?- searchAll(id1).
*/

% Apaga os predicados 'xy' da memoria e carrega os desenhos a partir de um arquivo de banco de dados
load :-
    retractall(xy(_,_,_)),
    open('desenhos.pl', read, Stream),
    repeat,
        read(Stream, Data),
        (Data == end_of_file -> true ; assert(Data), fail),
        !,
        close(Stream).

% Ponto de deslocamento, se <Id> existente
new(Id,X,Y) :-
    xy(Id,_,_),
    assertz(xy(Id,X,Y)),
    !.

% Ponto inicial, caso contrario
new(Id,X,Y) :-
    asserta(xy(Id,X,Y)),
    !.

% Exibe opcoes de busca
search :-
    write('searchAll(Id).     -> Ponto inicial e todos os deslocamentos de <Id>'), nl,
    write('searchFirst(Id,N). -> Ponto inicial e os <N-1> primeiros deslocamentos de <Id>'), nl,
    write('searchLast(Id,N).  -> Lista os <N> ultimos deslocamentos de <Id>').

searchAll(Id) :-
    listing(xy(Id,_,_)).

% Exibe opcoes de alteracao
change :-
    write('change(Id,X,Y,Xnew,Ynew).  -> Altera um ponto de <Id>'), nl,
    write('changeFirst(Id,Xnew,Ynew). -> Altera o ponto inicial de <Id>'), nl,
    write('changeLast(Id,Xnew,Ynew).  -> Altera o deslocamento final de <Id>').

% Grava os desenhos da memoria em arquivo
commit :-
    open('desenhos.pl', write, Stream),
    telling(Screen),
    tell(Stream),
    listing(xy),
    %listing(list),
    tell(Screen),
    close(Stream).

% Exibe menu principal
menu :-
    write('load.        -> Carrega todos os desenhos do banco de dados para a memoria'), nl,
    write('new(Id,X,Y). -> Insere um deslocamento no desenho com identificador <Id>'), nl,
    write('                (se primeira insercao, trata-se de um ponto inicial)'), nl,
    write('search.      -> Consulta pontos dos desenhos'), nl,
    write('change.      -> Modifica um deslocamento existente do desenho'), nl,
    write('remove.      -> Remove um determinado deslocamento existente do desenho'), nl,
    write('undo.        -> Remove o deslocamento inserido mais recentemente'), nl,
    write('commit.      -> Grava alteracoes de todos dos desenhos no banco de dados').

remove :- 
    write('remove(Id,x,y)  -> passa o id, o x e o y do ponto e o remove'), nl,
    write('removeFirst(Id) -> remove o primeiro deslocamento com o id passado'), nl,
    write('removeAll(Id)   -> remove todos os deslocamentos com o id Id'),nl,
    write('removeAll       -> remove TODOS os deslocamentos'), nl.


%Mostra elementos da lista dado um range
showRangeList(List, Begin, End) :-
                                    between(Begin,End,Index),
                                    nth1(Index,List,Elem),
                                    write(Elem),nl.

searchFirst(Id, N) :-
    findall(V, (xy(Id, X, Y), append([Id], [X], L), append(L, [Y], V)), List),
    showRangeList(List, 1, N),
    false.


%calcula o range a ser mostrado sabendo que o primeiro elemento precisa ser TamanhoList - N
searchLast(Id, N) :-
    findall(V, (xy(Id, X, Y), append([Id], [X], L), append(L, [Y], V)), ListPoints),
    length(ListPoints, Len),
    Init is Len - N + 1,
    showRangeList(ListPoints,Init,Len),
    false.

%%Escreve uma lista de deslocamentos no arquivo, formato da lista: [[id1,X1,Y],[id2,X2,Y2],...]
writeListInfile(List) :- 
    length(List,Size),
    between(0,Size,K),
    nth0(K, List, P),
    nth0(0, P, Id),
    nth0(1, P, XP),
    nth0(2, P, YP),
    new(Id, XP, YP).


% o change1 altera só a primeira ocorrencia do ponto, por exemplo, se no banco existe [[id1,10,10],[id1,10,10]],
% o change1 só irá alterar a primeira ocorrencia, ficando o banco assim: [[id1,Ynew,Xnew],[id1,10,10]]
% o change altera todas as ocorrencias
change1(Id, X,Y,Xnew, Ynew) :- 
    findall(V, (xy(I, A, B), append([I], [A], L), append(L, [B], V)), All),
    length(All, Size),
    between(0,Size,K),
    nth0(K, All, P),
    nth0(0, P, IdP),
    nth0(1, P, XP),
    nth0(2, P, YP),
    IdP = Id, XP = X, YP = Y -> replace(All,K,Xnew,Ynew,List),
    removeAll,
    retractall(list(_, _, _)),
    writeListInfile(List),
    false.

%muda as coordenadas de um dado deslocamento
change(Id, X, Y, Xnew, Ynew) :-
    retract(xy(Id,X,Y)), !,
    assert(xy(Id,Xnew,Ynew)).
    
changeFirst(Id, Xnew, Ynew) :-
    remove(Id, _, _),
    !,
    asserta(xy(Id, Xnew, Ynew)),
    assertz(list(Id, Xnew, Ynew)).

% L é uma lista de listas, X é a linha e Y a coluna, Z é valor a ser alterado e
% R é a lista alterada
replaceAux(L,X,Y,Z,R) :-
  append(RowPfx,[Row|RowSfx],L),
  length(RowPfx,X),
  append(ColPfx,[_|ColSfx],Row),
  length(ColPfx,Y),
  append(ColPfx,[Z|ColSfx],RowNew),
  append(RowPfx,[RowNew|RowSfx],R).

% Usa replaceAux para listas do tipo [[id0,X0,Y0],[id1,X1,Y1],...]
% Pos é o indice do deslocamento, Xnew e Ynew sao os valores a serem alterados, NewList é a lista alterada
replace(List,Pos,Xnew,Ynew, NewList) :-
                replaceAux(List,Pos,1,Xnew,Aux),
                replaceAux(Aux,Pos,2,Ynew,NewList).

changeLast(Id, Xnew, Ynew) :-
    findall(V, (xy(Id, X, Y), append([Id], [X], L), append(L, [Y], V)), All),
    last(All, Last),
    nth0(0, Last, IdLast),
    nth0(1, Last, XLast),
    nth0(2, Last, YLast),
    remove(IdLast, XLast, YLast),
    assertz(xy(Id, Xnew, Ynew)),
    asserta(list(Id, Xnew, Ynew)),
    !.

remove(Id, X, Y) :-
    retract(xy(Id, X, Y)),
    !.

removeFirst(Id) :- xy(Id, A, B), retract(xy(Id,A,B)), !.

removeAll(Id) :-  retractall((xy(Id,_,_))).

removeAll     :- retractall(xy(_,_,_)).

undo :-
    findall(V, (xy(Id, X, Y), append([Id], [X], L), append(L, [Y], V)), DeslocList),
    length(DeslocList,Size),
    nth1(Size,DeslocList,El),
    nth0(0,El,Id),
    nth0(1,El,X),
    nth0(2,El,Y),
    retract(xy(Id,X,Y)),
    write(El).
    
retangulo(Id, X, Y, Lado1, Lado2) :-
    NegLado1 is (-1) * Lado1,
    new(Id,X,Y),
    new(Id,0,Lado1),
    new(Id,Lado2, 0),
    new(Id, 0, NegLado1).

quadrado(Id, X, Y, Lado) :- retangulo(Id,X,Y,Lado,Lado).

% Base: tamanho da base do triangulo
triangulo(Id, X, Y, Base) :-
    BaseDiv2 is div(Base, 2),
    NBaseDiv2 is (-1) * BaseDiv2,
    new(Id, X, Y),
    new(Id, Base, 0),
    new(Id,NBaseDiv2, NBaseDiv2),
    false.

replica(Id, N, Dx, Dy) :-
    between(1, N, T),
    (findall(V, (xy(Id, X, Y), append([Id], [X], L), append(L, [Y], V)), DeslocList),
     length(DeslocList, S),
     between(0, S, K),
     nth0(K, DeslocList, M),
     nth0(0, M, IdM),
     nth0(1, M, XM),
     nth0(2, M, YM),
     atom_concat(IdM, '_', Temp),
     atom_concat(Temp, T, NewId),
     NewX is XM + (Dx*T),
     NewY is YM + (Dy*T),
     ((K =:= 0) -> new(NewId, NewX, NewY);
     new(NewId, XM, YM))),
     false.

% desenha um circulo de acordo com o resultado: cada ponto p de um circulo é igual a (x,y) = (r * cos(i), r * cos(i))
% i esta em graus e varia de 0 a 360. Usa a elipse como base com a excentricidade igual a 0
%
circulo(Id, X, Y, R) :- elipse(Id, X, Y, R, 0).

% Cria uma elipse 
% A excentricidade aqui não é precisa
% Dois pontos de um circulo ou elipse tem uma distancia h, h é a hipotenusa de um triangulo retangulo formado por esses dois pontos
% Toda vez que o cateto adjacente aumenta e o oposto dimunui, o circulo se deforma e vira, cada vez mais, uma elipse,
% por isso há a multiplicação do componente X e a divisão do componente Y de cada ponto
% Poderia dividir por E simplesmente, mas a excentricidade do circulo é 0, então E + 1, é necessaria
elipse(Id, X, Y, R, E) :- 
            new(Id, X, Y),
            loop(Id, 0, R, E).

% Faz o loop e calcula os pontos do circulo ou elipse
loop(Id,N, R, E) :-
  N < 360,
  N0 is N + 1,
  degToRad(N, Aux1),
  degToRad(N, Aux2),
  X is (cos(Aux1) * R)*(E + 1),
  Y is (sin(Aux2) * R)/(E + 1),
  new(Id,X,Y),
  loop(Id,N0,R,E).

degToRad(Deg, Rad) :- X is Deg * 3.14159265, Rad is X / 180.0.

% Desenha um rosto

figura(Id,X,Y) :- circulo(Id, X, Y, 4).
figura(Id,X,Y) :- X1 is X - 50,
                  Y1 is Y + 100,
                  atom_concat(Id, '_olhoEsquerdo', RealId),
                  circulo(RealId,X1,Y1,0.5).
figura(Id,X,Y) :- X2 is X + 100,
                  Y2 is Y + 100,
                  atom_concat(Id, '_olhoDireito', RealId),
                  circulo(RealId,X2,Y2,0.5).
figura(Id,X,Y) :- Y3 is Y + 250,
                  X3 is X,
                  atom_concat(Id, '_nariz', RealId),
                  triangulo(RealId,X3,Y3,50).
figura(Id,X,Y) :- Y4 is Y + 350,
                  atom_concat(Id, '_boca', RealId),
                  elipse(RealId,X,Y4,1,1).
