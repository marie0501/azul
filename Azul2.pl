%objetos definidos dinamicamente
:-dynamic wall/1.
:-dynamic player_state/5. 

%lolaa():- best_move([[azul,rojo,rojo,verde],[init_tile]],[[[azul,[0]],[azul,floor],[rojo,[0,0]],[rojo,floor],[verde,[0]],[verde,floor]],[]], [0,0,0,0,0],[F,C,P],_).
lolaa():-new_game(4).
c(X):- initialize_player_state(2),update_player_state(1,[[azul,rojo,verde,negro,cyan],[0,0,0,0,0],[azul,rojo,verde,negro,cyan],[0,0,0,0,0],[0,0,0,0,0]],_,_,10),
update_player_state(2,[[azul,0,verde,negro,cyan],[azul,0,0,0,0],[azul,rojo,verde,negro,cyan],[azul,0,0,0,0],[azul,0,0,0,0]],_,_,10),aditional_score([1,2],S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TABLERO
color_list([red,blue,cyan,black,yellow]).
game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,State).
%Inicializacion del Muro del jugador
initialize_wall(N,W):- my_concat([0,0,0,0,0],W,N).

%Inicializacion del Patron del jugador
initialize_pattern([[0],[0,0],[0,0,0],[0,0,0,0],[0,0,0,0,0]]).

%Inicializacion del Piso del jugador
initialize_floor([0,0,0,0,0,0,0]).

%Piso del tablero
floor_values([-1,-1,-2,-2,-2,-3,-3]). 

%Muro del tablero
build_wall(1,[[yellow,red,black,cyan,blue]]):-!.
build_wall(Row,P):- 
    Next_row is Row - 1, build_wall(Next_row,[[First_tile|R]|Q]),
    concat_list(R,[First_tile],New_row), concat_list([New_row],[[First_tile|R]|Q],P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BOLSA
initialize_bag(Bag):- 
    insert_tile_bag(20,blue,[],Bag1), 
    insert_tile_bag(20,red, Bag1,Bag2),
    insert_tile_bag(20,yellow,Bag2,Bag3),
    insert_tile_bag(20,black,Bag3,Bag4),
    insert_tile_bag(20,cyan,Bag4,Bag).

%Inicializar bolsa con Cantidad de Azulejos de un Color
insert_tile_bag(0,_,Bag,Bag):-!.
insert_tile_bag(Count,Color,Bag,New_bag):- Next_count is Count - 1, insert_element(Bag,Color,Temp_bag), insert_tile_bag(Next_count,Color,Temp_bag,New_bag).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FABRICAS

% regla que define el numero de fabrica a crear, dada la cantidad de
% jugadores N, se deben crear K fabricas
num_of_factory(2,5).
num_of_factory(3,7).
num_of_factory(4,9).

%inicializacion de fabricas
initialize_factories(_,[],[],[]):-!.
initialize_factories(0,Bag,[],Bag):-!.
initialize_factories(N,Bag,[L|R],Bag2):- initialize_one_factory(4,Bag,L,Bagx), P is N - 1, initialize_factories(P,Bagx,R,Bag2).

% regla que inicializa una fabrica con N azulejos y actualiza la bolsa,
% quitando de ella los azulejos que se pusieron en la fabrica

initialize_one_factory(_,[],[],[]):- !.
initialize_one_factory(1,Bag,[L],Bag2):-  random_select(L,Bag,Bag2),!.
initialize_one_factory(N,Bag,Factory,Bag2):- random_select(L,Bag,Bagx), P is N-1, initialize_one_factory(P,Bagx,K,Bag2), concat_list(K,[L],Factory).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JUGADORES
reset([]):-!.
reset([P|Players]):- retract(player_state(P,_,_,_,_)), reset(Players).
%Determinar el siguiente jugador
next_player(1,_,2).
next_player(2,2,1):-!.
next_player(2,N,3):-N>2.
next_player(3,3,1):-!.
next_player(3,N,4):-N>3.
next_player(4,4,1).

%Obtener una lista con los indices de losjugadores
player_list(2,[1,2]).
player_list(3,[1,2,3]).
player_list(4,[1,2,3,4]).

%inicializacion de jugadores
initialize_player([],Wall,Pattern,Floor,Score):-!.
initialize_player([P|R],Wall,Pattern,Floor,Score):- assertz(player_state(P,Wall,Pattern,Floor,Score)), initialize_player(R,Wall,Pattern,Floor,Score).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ESTADO DEL JUEGO Y DEL JUGADOR

%inicializaciones de estados
initialize_game_state(N,game_state(N,NewBag,Factories,Center,[],Initial,Initial,initialized)):- 
    initialize_bag(Bag), 
    initialize_center(Center),
    num_of_factory(N,F), 
    initialize_factories(F,Bag,Factories,NewBag),
    Initial is random(N) + 1.

initialize_player_state(N):- 
    initialize_wall(5,Wall),
    initialize_pattern(Pattern),
    initialize_floor(Floor),
    player_list(N,List),
    initialize_player(List,Wall,Pattern,Floor,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%inicializar centro
initialize_center([init_tile]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SALVAR CAMBIOS DE ESTADO
update_game_state(N,Bag,Factories,Center,Stack,InitialPlayer,CurrentPlayer,State):- retract(game_state(N,_,_,_,_,_,_,_)), asserta(game_state(N,Bag,Factories,Center,Stack,InitialPlayer,CurrentPlayer,State)).
update_player_state(Player,Wall,Pattern,Floor,Score):- retract(player_state(Player,_,_,_,_)), asserta(player_state(Player,Wall,Pattern,Floor,Score)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JUGADAS VALIDAS PARA UN JUGADOR 

%Obtener todas las jugadas factibles dado un estado de juego(fabricas y centro) y el estado del jugador(Patron y Muro)
list_factories_moves([F],Pattern,Wall,[M]):- delete_all(init_tile,F,F1), from_list_to_set(F1,F1s), list_moves(F1s,Pattern,Wall,M),!.
list_factories_moves([F|Fr],Pattern,Wall,[M|Mr]):- from_list_to_set(F,Fs), list_moves(Fs,Pattern,Wall,M), list_factories_moves(Fr,Pattern,Wall,Mr).

    list_moves([],_,_,[]):-!.
    list_moves([Color|C],Pattern,Wall,M):- list_move_2(Color,Pattern,Wall,K), list_moves(C,Pattern,Wall,Moves), concat_list(K,Moves,M).

        list_move_2(Color,[],_,[[Color,floor]]):-!.
        list_move_2(Color,[P|Pr],[W|Wr],[[Color,P]|M]):- valid_move(Color,P,W), list_move_2(Color,Pr,Wr,M),!.
        list_move_2(Color,[P|Pr],[W|Wr],M):- not(valid_move(Color,P,W)),list_move_2(Color,Pr,Wr,M).

valid_move(Color,PatternRow,WallRow):- 
    not(member(Color, WallRow)), 
    not(full(PatternRow)),
    (empty(PatternRow); member(Color, PatternRow)).

%Obtener jugada optima dentro de las factibles atendiendo al estado del jugador mediante algoritmo greedy
best_move([[Eval|BMove]],BMove,Eval):-!.
best_move([[Eval|BMove]|List],BestMove,Ev):- best_move(List,X,E), best(E,Eval,X,BMove,BestMove,Ev),!.


list_best_factory_move([],_,_,[]):-!.
list_best_factory_move([F|Fr],[FMoves|FrMoves],Floor,[[Eval|BMove]|Rest]):- 
    best_factory_move(F,FMoves,Floor,BMove,Eval), 
    list_best_factory_move(Fr,FrMoves,Floor,Rest).  

    best_factory_move(Factory,[],Floor,[Factory,Color,PRow],-1000):-!.

    best_factory_move(Factory,[[Color,floor]|RMoves],Floor,Move,Eval):- 
        split_for_item(Factory,Color,ColorList,_),
        split_for_item(Floor,0,ZeroList,_), 
        evaluate(ColorList,[],Floor,E1,0),
        best_factory_move(Factory,RMoves,Floor,Move2, E2), 
        best(E1,E2,[Factory,Color,floor],Move2,Move,Eval),!.

    best_factory_move(Factory,[[Color,PatternRow]|RMoves],Floor,Move,Eval):-  PatternRow\=floor,
       split_for_item(Factory,Color,ColorList,_),split_for_item(PatternRow,0,ZeroList,BonList), length(BonList,B), 
       evaluate(ColorList,ZeroList,Floor,E1,B), best_factory_move(Factory,RMoves,Floor,Move2, E2), 
       best(E1,E2,[Factory,Color,PatternRow],Move2,Move,Eval),!.
   
    
    evaluate(Color,ZeroList,Floor,Eval,Bonnus):- length(Color, C), length(ZeroList, Z) , E is C - Z, E > 0, not(full(Floor)), Eval is E * -3,!.
    evaluate(Color,ZeroList,Floor,Z,Bonnus):- length(Color, C), length(ZeroList, Z) , E is C - Z, E=:=0, !.
    evaluate(Color,ZeroList,Floor,Eval,Bonnus):- length(Color, C), length(ZeroList, Z) , E is C - Z, E > 0,full(Floor), Eval is Bonnus - E, !.
    evaluate(Color,ZeroList,Floor,Eval,Bonus):- length(Color, C), length(ZeroList, Z) , E is C - Z, E < 0, Eval is Bonnus + E.

    best(E1,E2,Move1,Move2,Move1,E1):- E1>=E2,!.
    best(E1,E2,Move1,Move2,Move2,E2):- E1<E2,!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PONIENDO AZULEJOS EN LOS PATRONES Y EN EL SUELO

%Poner un conjunto de azulejos en una fila del patron mientras quepan, los restantes se ponen en el suelo

%put_tile_pattern(FilaPatron,ListaDeLozas,Piso,Stack,FilaResultante,PisoREsultante,StackResultante)
put_tile_pattern(Patrn,[],Floor,Stack,Patrn,Floor,Stack):-!.
put_tile_pattern([],[Tile|T],Floor,Stack,[],NewFloor,NewStack):- update_player_floor_and_game_stack(Floor,[Tile|T],NewFloor,Stack,NewStack),!.
put_tile_pattern([0|Pr],[Tile|T],Floor,Stack,[Tile|Prx],NewFloor,NewStack):- put_tile_pattern(Pr,T,Floor,Stack,Prx,NewFloor,NewStack),!. 
put_tile_pattern([Tile|Pr],[Tile|T],Floor,Stack,[Tile|Prx],NewFloor,NewStack):- put_tile_pattern(Pr,[Tile|T],Floor,Stack,Prx,NewFloor,NewStack). 

%poner un conjunto de azulejos en el suelo

%update_player_floor_and_game_stack(Piso,ListaLozas,PisoResultante,Stack,StackResultante)
update_player_floor_and_game_stack(Floor,[Tile],NewFloor,Stack,NewStack):- put_tile_floor_and_stack(Tile,Floor,NewFloor,Stack,NewStack),!. 
update_player_floor_and_game_stack(Floor,[Tile|R],NewFloor,Stack,NewStack) :- 
    put_tile_floor_and_stack(Tile,Floor,NewFloor1,Stack,NewStack1), 
    update_player_floor_and_game_stack(NewFloor1,R,NewFloor,NewStack1,NewStack).

%poner uno en uno azulejos en el suelo, si el suelo se llena se ponen en la tapa de la caja

%put_tile_floor_and_stack(UNALoza,Piso,PisoResultante,Stack,StackResultante)
put_tile_floor_and_stack(init_tile,[],[],Stack,Stack):- !.
put_tile_floor_and_stack(Tile,[],[],Stack,[Tile|Stack]):- Tile\= init_tile,!.
put_tile_floor_and_stack(Tile,[0|X],[Tile|X],Stack,Stack):-!.
put_tile_floor_and_stack(Tile,[X|Y],[X|Z],Stack,Stack1):-X\=0, put_tile_floor_and_stack(Tile,Y,Z,Stack,Stack1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUNTUACION POR REVESTIR EL MURO

score_counter(Wall,TWall,Color,FScore):- 
    horizontal_score(Wall,Color,HScore),
    horizontal_score(TWall,Color,VScore),
    add1(VScore,RVScore),
    FScore is HScore + RVScore + 1,!.
   
add1(0,0):-!.
add1(Score,NScore):- NScore is Score +1.

horizontal_score(Row,Color,Score):-
     adj(Row,Color,S), 
     reverse(Row,Reverse), 
     adj(Reverse,Color,S1), 
     Score is S+ S1.

score_counter_floor(Floor,Score):- 
    floor_values(F),
    scalar_product(Floor,F,Score).

adj([],Col,0):-!.
adj([Col|X],Col,Z):-score(X,Z),!.
adj([X|Y],Col,Z):- X\=Col, adj(Y,Col,Z).

score([],0):-!.
score([0|_],0):-!.   
score([X|Y],Score):- 
    score(Y,PartialScore),
    Score is PartialScore + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REVESTIR MURO
cover_wall([],[],[],[],[],Stack,Stack,Transpose,[],0):-!.
cover_wall([P|Pattern],[W|Wall],[W|NWall],[0|Color],[P|NewP],Stack,NewStack,Transpose,[X|Xr],Score):- 
    not(full(P)),transpose(W,W1),multi_concat(Transpose,W1,UT1),
   cover_wall(Pattern,Wall,NWall,Color,NewP,Stack,NewStack,UT1,Xr,Score),!.

cover_wall([[Col|P]|Pattern],[W|Wall],[NW|NWall],[Col|Y],[NP|NewP],Stack,NewStack,Transpose,[X|Xr],Score):- 
    full([Col|P]),  
    update_wall(Col,W,X,NW),
    concat_list(Stack,P,Stack2),
    replace_all(Col,0,[Col|P],NP),
    transpose(NW,W1), multi_concat(Transpose,W1,UT1),
    matrix_transpose(Wall,TW),multi_concat(UT1,TW,UT2),
    nth0(Index,NW,Col),
    nth0(Index,UT2,TWRow),
    score_counter(NW,TWRow,Col,S1),
    cover_wall(Pattern,Wall,NWall,Y,NewP,Stack2,NewStack,UT1,Xr,S2), Score is S1 + S2.


update_wall(_,[],[],[]):-!.
update_wall(Color,[_|Wr],[Color|Xr],[Color|Wr]):-!.
update_wall(Color,[W|Wr],[X|Xr],[W|Result]):- Color\=X, update_wall(Color,Wr,Xr,Result). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJECUTAR LA JUGADA CORRESPONDIENTE

make_move(P,First,Center,Color,floor,Center,Floor,Stack,P,floor,NCenter,NStack,NFloor):- member(init_tile,Center), delete_all(init_tile,Center,F2),
split_for_item(F2,Color,ColorList,NCenter),update_player_floor_and_game_stack(Floor,[init_tile|ColorList],NFloor,Stack,NStack),!.

make_move(P,First,Center,Color,floor,Center,Floor,Stack,First,floor,NCenter,NStack,NFloor):- not(member(init_tile,Center)), 
split_for_item(Center,Color,ColorList,NCenter),update_player_floor_and_game_stack(Floor,ColorList,NFloor,Stack,NStack),!.

make_move(P,First,Factory,Color,floor,Center,Floor,Stack,First,floor,NCenter,NStack,NFloor):-Factory \= Center, 
split_for_item(Factory,Color,ColorList,Rest), concat_list(Center,Rest,NCenter),
update_player_floor_and_game_stack(Floor,ColorList,NFloor,Stack,NStack),!.

make_move(P,First,Center,Color,Pattern,Center,Floor,Stack,P,NPattern,NCenter,NStack,NFloor):- 
member(init_tile,Center), delete_all(init_tile,Center,F2),
split_for_item(F2,Color,ColorList,NCenter),put_tile_floor_and_stack(init_tile,Floor,NFloor1,Stack,NStack1),reverse(Pattern,Pattern1), 
put_tile_pattern(Pattern1,ColorList,NFloor1,NStack1,NPattern1,NFloor,NStack),reverse(NPattern1,NPattern),!.

make_move(P,First,Center,Color,Pattern,Center,Floor,Stack,First,NPattern,NCenter,NStack,NFloor):- not(member(init_tile,Center)), 
split_for_item(Center,Color,ColorList,NCenter), 
reverse(Pattern,Pattern1), put_tile_pattern(Pattern1,ColorList,Floor,Stack,NPattern1,NFloor,NStack), reverse(NPattern1,NPattern),!.

make_move(P,First,Factory,Color,Pattern,Center,Floor,Stack,First,NPattern,NCenter,NStack,NFloor):-Factory \= Center, 
split_for_item(Factory,Color,ColorList,Rest), concat_list(Center,Rest,NCenter),reverse(Pattern,Pattern1),
put_tile_pattern(Pattern1,ColorList,Floor,Stack,NPattern1,NFloor,NStack),reverse(NPattern1,NPattern).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONDICION DE PARADA

game_over_f2([]):-!,false.
game_over_f2([W|Wall]):- full(W); game_over_f2(Wall). 

over_fase2([]):-!.
over_fase2([P|Players]):- player_state(P,_,Pattern,[0,0,0,0,0,0,0],_), full_pattern(Pattern,0), over_fase2(Players).

gover_fase2([]):-!,false.
gover_fase2([P|Players]):- player_state(P,Wall,_,_,_), game_over_f2(Wall), over_fase2(Players),!.
gover_fase2([P|Players]):- player_state(P,Wall,_,_,_), not(game_over_f2(Wall)), over_fase2([P]), gover_fase2(Players).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_game(N):-build_wall(5,Wall), asserta(wall(Wall)), initialize_game_state(N, game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,initialized)),
   initialize_player_state(N),num_of_factory(N,K), concat_list(Factories,[Center],FactoriesC),print_start(N,K,FactoriesC,Bag,Stack),nl,
   play(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,initialized),game_state(N,FBag,FFactories,FCenter,FStack,FInitialP,FCurrentP,win)),player_list(N,List),reset(List),retract(wall(Wall)).
new_game(N):- player_list(N,List),reset(List),retract(wall(Wall)).

play(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,State),game_state(N,FBag,FFactories,FCenter,FStack,FInitialP,FCurrentP,State2)):- 
    move(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,State),game_state(N,FBag,FFactories,FCenter,FStack,FInitialP,FCurrentP,State2)),!. 

play(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,State),game_state(N,FBag,FFactories,FCenter,FStack,FInitialP,FCurrentP,State2)):- 
    move(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,State),game_state(N,IBag,IFactories,ICenter,IStack,IInitialP,ICurrentP,StateI)),
    play(game_state(N,IBag,IFactories,ICenter,IStack,IInitialP,ICurrentP,StateI),game_state(N,FBag,FFactories,FCenter,FStack,FInitialP,FCurrentP,State2)).


move(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,initialized),game_state(N,Bag,FFactories,FCenter,FStack,FInitialP,FCurrentP,fase1)):- 
    player_state(CurrentP,Wall,Pattern,Floor,Score), 
    concat_list(Factories,[Center],FC),
    list_factories_moves(FC,Pattern,Wall,Moves), 
    list_best_factory_move(FC,Moves, Floor,List),
    best_move(List,[F,C,P],_),
    write('BM OK'),nl,
    make_move(CurrentP,InitialP,F,C,P,Center,Floor,Stack,FInitialP,FPattern,FCenter,FStack,FFloor),  write('Make OK'),nl, write(FCenter),nl,
    my_delete(F, Factories, FFactories), 
    next_player(CurrentP,N,FCurrentP),
    replace_all(P,FPattern, Pattern, FP), 
    update_player_state(CurrentP,Wall,FP,FFloor,Score),!,
    sleep(1),write('Print OK'),nl,print_move_pattern(CurrentP,C,F,P),nl, nl,print_wp_double(CurrentP,Wall,Pattern,Wall,FP),
    nl, print_floor_double(Floor,FFloor,Score,Score),nl,nl. 

move(game_state(N,Bag,[],[init_tile],Stack,InitialP,CurrentP,fase1),game_state(N,Bag,[],[],Stack,InitialP,CurrentP,fase2)):-!,sleep(2), write("Fase 1 --> Fase 2"), nl,nl. 
move(game_state(N,Bag,[],[],Stack,InitialP,CurrentP,fase1),game_state(N,Bag,[],[],Stack,InitialP,CurrentP,fase2)):-!,sleep(2), write("Fase 1 --> Fase 2"), nl,nl. 
move(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,fase1),game_state(N,Bag,FFactories,FCenter,FStack,FInitialP,FCurrentP,fase1)):- 
    player_state(CurrentP,Wall,Pattern,Floor,Score), 
    concat_list(Factories,[Center],FC),
    list_factories_moves(FC,Pattern,Wall,Moves),
    list_best_factory_move(FC,Moves, Floor,List),

    best_move(List,[F,C,P],_),F\=Center, P\=floor, write('BM OK'),nl,
    make_move(CurrentP,InitialP,F,C,P,Center,Floor,Stack,FInitialP,FPattern,FCenter,FStack,FFloor),  write('Make OK'),nl, write(FCenter),nl,
    my_delete(F, Factories, FFactories), 
    next_player(CurrentP,N,FCurrentP),
    replace_all(P,FPattern, Pattern, FP), 
    update_player_state(CurrentP,Wall,FP,FFloor,Score),!, 
    sleep(1),write('Print OK'),nl, print_move_pattern(CurrentP,C,F,P),nl,nl,print_wp_double(CurrentP,Wall,Pattern,Wall,FP),
    nl, print_floor_double(Floor,FFloor,Score,Score),nl,nl.

move(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,fase1),game_state(N,Bag,Factories,FCenter,FStack,FInitialP,FCurrentP,fase1)):- 
    player_state(CurrentP,Wall,Pattern,Floor,Score), 
    concat_list(Factories,[Center],FC),
    list_factories_moves(FC,Pattern,Wall,Moves),
    list_best_factory_move(FC,Moves, Floor,List),
    best_move(List,[Center,C,P],_), P\=floor,
    write('BM OK'),nl,
    make_move(CurrentP,InitialP,Center,C,P,Center,Floor,Stack,FInitialP,FPattern,FCenter,FStack,FFloor),  write('Make OK'),nl, write(FCenter),nl,
    next_player(CurrentP,N,FCurrentP),
    replace_all(P,FPattern, Pattern, FP), 
    update_player_state(CurrentP,Wall,FP,FFloor,Score),!,
    sleep(1),write('Print OK'),nl, print_move_pattern_center(CurrentP,C,Center,P),nl,nl,print_wp_double(CurrentP,Wall,Pattern,Wall,FP),
    nl, print_floor_double(Floor,FFloor,Score,Score),nl,nl.

move(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,fase1),game_state(N,Bag,Factories,FCenter,FStack,FInitialP,FCurrentP,fase1)):- 
    player_state(CurrentP,Wall,Pattern,Floor,Score), 
    concat_list(Factories,[Center],FC),
    list_factories_moves(FC,Pattern,Wall,Moves),
    list_best_factory_move(FC,Moves, Floor,List),
    best_move(List,[Center,C,floor],_), write('BM OK'),nl,
    make_move(CurrentP,InitialP,Center,C,P,Center,Floor,Stack,FInitialP,FPattern,FCenter,FStack,FFloor),  write('Make OK'),nl, write(FCenter),nl,
    next_player(CurrentP,N,FCurrentP),
    replace_all(P,FPattern, Pattern, FP), 
    update_player_state(CurrentP,Wall,FP,FFloor,Score),!,
    sleep(1),write('Print OK'),nl, print_move_floor_center(CurrentP,C,Center,P),nl,nl,print_wp_double(CurrentP,Wall,Pattern,Wall,FP),
    nl, print_floor_double(Floor,FFloor,Score,Score),nl,nl.

move(game_state(N,Bag,Factories,Center,Stack,InitialP,CurrentP,fase1),game_state(N,Bag,FFactories,FCenter,FStack,FInitialP,FCurrentP,fase1)):-
     player_state(CurrentP,Wall,Pattern,Floor,Score), 
        concat_list(Factories,[Center],FC),
        list_factories_moves(FC,Pattern,Wall,Moves), 
        list_best_factory_move(FC,Moves, Floor,List), 
        best_move(List,[F,C,floor],_), F\=Center, write('BM OK'),nl,
        make_move(CurrentP,InitialP,F,C,floor,Center,Floor,Stack,FInitialP,floor,FCenter,FStack,FFloor),  write('Make OK'),nl, write(FCenter),nl,
        my_delete(F, Factories, FFactories), next_player(CurrentP,N,FCurrentP),
        update_player_state(CurrentP,Wall,Pattern,FFloor,Score), 
        sleep(1),write('Print OK'),nl, print_move_floor(CurrentP,C,F,P),nl,nl,print_wp_double(CurrentP,Wall,Pattern,Wall,Pattern),
        nl, print_floor_double(Floor,FFloor,Score,Score),nl,nl.

move(game_state(N,Bag,[],[],Stack,InitialP,CurrentP,fase2),game_state(N,Bag,[],[],Stack,InitialP,CurrentP,over)):-
    player_list(N,List), gover_fase2(List), !, write("Game Over :)"), nl,nl.
 

move(game_state(N,Bag,[],[],Stack,InitialP,CurrentP,fase2),game_state(N,Bag,[],[],Stack,InitialP,CurrentP,fase3)):- 
    player_list(N,List), over_fase2(List),!,sleep(2),write("Fase 2 --> Fase 3"),nl,nl.

move(game_state(N,Bag,[],[],Stack,InitialP,CurrentP,fase2),game_state(N,Bag,[],[],FStack,InitialP,FCurrentP,fase2)):-
    player_state(CurrentP,Wall,Pattern,Floor,Score),wall(StandardW), cover_wall(Pattern,Wall,FWall,Color,FPattern,Stack,FStack1,T,StandardW,S0), 
    score_counter_floor(Floor,S1),FScore is max(Score+S0+S1,0), delete_all(init_tile,Floor,Floor1), split_for_item(Floor1,0,_,Rest),concat_list(FStack1,Rest,FStack),
    initialize_floor(NewFloor),update_player_state(CurrentP,FWall,FPattern,NewFloor,FScore), next_player(CurrentP,N,FCurrentP),
    sleep(1),write("Player "), write(CurrentP), nl,nl,print_wp_double(CurrentP,Wall,Pattern,FWall,FPattern),
    nl, print_floor_double(Floor,NewFloor,Score,FScore),nl,nl.

move(game_state(N,[],[],[],[],InitialP,CurrentP,fase3),game_state(N,NBag,Factories,Center,Stack,InitialP,InitialP,over)):- !, write("Game Over :)"), nl,nl.
move(game_state(N,Bag,[],[],Stack,InitialP,CurrentP,fase3),game_state(N,NBag,Factories,Center,Stack,InitialP,InitialP,fase1)):-
    Bag\= [], num_of_factory(N,K), initialize_factories(K,Bag,Factories,NBag), initialize_center(Center),!,
    
    sleep(1),concat_list(Factories,[Center],F), length(F,R),print_fase1(R,F,NBag,Stack),nl,nl, write("Fase 3 --> Fase 1"),nl ,nl.

move(game_state(N,[],[],[],Stack,InitialP,CurrentP,fase3),game_state(N,NBag,Factories,Center,[],InitialP,InitialP,fase1)):-
    num_of_factory(N,K), initialize_factories(K,Stack,Factories,NBag), initialize_center(Center),
    sleep(1),concat_list(Factories,[Center],F), length(F,R), print_fase1(R,F,NBag,[]),nl ,nl,write("Fase 3 --> Fase 1"),nl,nl.

move(game_state(N,_,[],[],_,_,_,over),game_state(N,_,[],[],_,Winners,_,win)):- player_list(N,List), aditional_score(List,Scores), 
   best_score(Scores,S), best_score_players(List,S,PossibleWinners),winner_full_rows(PossibleWinners,Max),winner(PossibleWinners,Max,Winners),sleep(2),write("Scores: "),nl,print_score(List),nl, print_winners(Winners) . 

aditional_score([],[]):-!.
aditional_score([P|Players],[S|Score]):- 
    player_state(P,Wall,_,_,Sp), 
    full_pattern(Wall,S1), 
    matrix_transpose(Wall,TWall), 
    full_pattern(TWall,S2),
    color_list(C),
    lucky_five(C,Wall,S3),
    S is Sp + S1 * 2 + S2 * 7 + S3 , 
    update_player_state(P,Wall,_,_,S), 
    aditional_score(Players,Score). 

best_score([],0):-!.
best_score([S|Scores],S):- best_score(Scores,S2), S >= S2,!.
best_score([S|Scores],S2):- best_score(Scores,S2), S < S2.

best_score_players([],_,[]):-!.
best_score_players([P|Players],S,[P|Winners]):- player_state(P,_,_,_,S), best_score_players(Players,S,Winners),!.
best_score_players([P|Players],S,Winners):- player_state(P,_,_,_,M), M < S, best_score_players(Players,S,Winners).

winner_full_rows([],0):-!.
winner_full_rows([P|Possible],Max):- player_state(P,Wall,_,_,_), full_pattern(Wall,Max),winner_full_rows(Possible,X), Max >= X,!.
winner_full_rows([P|Possible],Max):-player_state(P,Wall,_,_,_), full_pattern(Wall,X),winner_full_rows(Possible,Max), Max >= X,!.

winner([],Max,[]):-!.
winner([P|Possible],Max,[P|W]):- player_state(P,Wall,_,_,_), full_pattern(Wall,Max), winner(Possible,Max,W),!.
winner([P|Possible],Max,W):- player_state(P,Wall,_,_,_), not(full_pattern(Wall,Max)), winner(Possible,Max,W),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLAUSULAS AUXILIARES

%reglas auxiliares
my_concat(P,[P],1):-!.
my_concat(P,[P|Z],N):- T is N - 1, my_concat(P,Z,T).


my_modify([[L,K]|Y],L,[[L,P]|Y]):-P is K -1, P \= 0, !.
my_modify([[L,K]|Y],L,Y):-!.
my_modify([[K,W]|Y],L,[[K,W]|S]):- K \= L, my_modify(Y,L,S).

concat_list([],X,X):-!.
concat_list(X,[],X):-!.
concat_list([X|R],[P|Q],[X|Z]):- concat_list(R,[P|Q],Z).

insert_element([],L,[L]):-!.
insert_element([X|Y],L,[L,X|Y]):-!.
insert_element([X|Y],L,[X|Z]):- insert_element(Y,L,Z).
    rint_tiles_(W),nl,print_wall_and_pattern(Wall).

count_tiles_([],0,0,0,0,0):-!.
count_tiles_([blue|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue1,Red,Yellow,Cyan,Black), Blue is Blue1 + 1.
count_tiles_([red|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue,Red1,Yellow,Cyan,Black), Red is Red1 + 1.
count_tiles_([yellow|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue,Red,Yellow1,Cyan,Black), Yellow is Yellow1 + 1.
count_tiles_([cyan|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue,Red,Yellow,Cyan1,Black), Cyan is Cyan1 + 1.
count_tiles_([black|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue,Red,Yellow,Cyan,Black1), Black is Black1 + 1.
    
print_bag(Bag):- 
    count_tiles_(Bag,Blue,Red,Yellow,Cyan,Black), 
    write("Bag:"),nl,
    write("-"),write(Blue),write(" "),write("blue tiles."),nl,
    write("-"),write(Red),write(" "),write("red tiles."),nl,
    write("-"),write(Yellow),write(" "),write("yellow tiles."),nl,
    write("-"),write(Cyan),write(" "),write("cyan tiles."),nl,
    write("-"),write(Black),write(" "),write("black tiles."),nl.


print_stack(Stack).
print_winners(P).
print_fase2_wall(Wall,Score).

print_score([]):-!.
print_score([P|Players]):-player_state(P,Wall,_,_,Score),print_wall_and_pattern(Wall),nl,write("Player "),write(P),write("--> "),write(Score),write(" "),write("points").

print_horizontal(0):-!.
print_horizontal(N):-write("--"), M is N-1,print_horizontal(M).

print_vertical(0):-!.
print_vertical(N):-write("|"), M is N-1,print_vertical(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
