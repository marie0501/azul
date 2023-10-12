my_print(M):-ansi_format([bold,fg(M)], M, []).

print_new_line:-nl,!.

num_of_factory(N,K):- K is N * 2 + 1.

print_start(N,K,F,B,S):-write("New Game!"), nl,nl, write("Number of players: "),write(N),nl,nl,print_fase1(K,F,B,S),!.

print_fase1(K,F,B,S):-write("Bag:"),nl,print_bag_and_stack(B),nl,nl,write("Stack:"),nl,print_bag_and_stack(S),nl,nl,write("Factories:"),nl,print_factory(F,K),!.

print_factory([],K):-!.
print_factory([F],K):-write("Center: "),write("["), print_tiles_(F),write("]"),nl,!.
print_factory([F|FactoryList],K):-length(FactoryList,L1),write("Factory"),write(" "), A is K - L1 + 1, write(A), write(": "),write("["), print_tiles_(F),write("]"),nl,print_factory(FactoryList,K).

print_tiles([]):-print_vertical(1),!.
print_tiles([init_tile|Tiles]):-print_vertical(1),write(" "),write("1"),write(" "), print_tiles(Tiles).
print_tiles([0|Tiles]):-print_vertical(1),write(" "),write("X"),write(" "), print_tiles(Tiles).
print_tiles([T|Tiles]):-T\=0,print_vertical(1),write(" "), ansi_format([bold,fg(T)], 'O', []),write(" "), print_tiles(Tiles).

print_tiles_([init_tile]):-write("1"),!.
print_tiles_([0]):-write("X"),!.
print_tiles_([T]):-T\=0, ansi_format([bold,fg(T)], 'O', []),!.
print_tiles_([init_tile|Tiles]):-write("1"),write(" "), print_tiles_(Tiles).
print_tiles_([0|Tiles]):-write("X"),write(" "), print_tiles_(Tiles).
print_tiles_([T|Tiles]):- T\=0, ansi_format([bold,fg(T)], 'O', []),write(" "), print_tiles_(Tiles).

count_column([],0):-!.
count_column([P|Pattern],M):-count_column(Pattern,X), M is X + 1.

count_tiles([],0):-!.
count_tiles([0|Pattern],N):-count_tiles(Pattern,N),!.
count_tiles([P|Pattern],N):-P\=0, count_tiles(Pattern,S), N is S + 1.

print_move_floor_center(P,Color,F,Fl):-count_tiles(Color,N),write("Player "),write(P),write(" "),write("took "), write(Color), write(" "),write("tiles from Center "), write("["), print_tiles_(F),write("]"),write(" "), write("and put them on the Floor").
print_move_floor(P,Color,F,Fl):-write("Player "),write(P),write(" "),write("took "), write(Color), write(" "),write("tiles from Factory "), write("["), print_tiles_(F),write("]"),write(" "), write("and put them on the Floor").

print_move_pattern_center(P,Color,F,Pat):-count_column(Pat,M),count_tiles(Color,N),write("Player "),write(P),write(" "),write("took "),write(N),write(" "), write(Color),write(" "),write("tiles from Center "), write("["), print_tiles_(F),write("]"), write(" "), write("and put them on Pattern Row "),write(M).
print_move_pattern(P,Color,F,Pat):-count_tiles(Color,N),write("Player "),write(P),write(" "),write("took "),write(N),write(" "), write(Color),write(" "),write("tiles from Factory "), write("["), print_tiles_(F),write("]"), write(" "), write("and put them on Pattern Row "),write(M).

print_wall_and_pattern([]):-print_horizontal(5),!.
print_wall_and_pattern([W|Wall]):-length(W,LW),K is (5-LW)*4,print_space(K),print_horizontal(LW),nl, print_space(K),print_tiles(W),nl,print_wall_and_pattern(Wall).

print_wp([],[]):-print_horizontal(5),write("    "),print_horizontal(5),!.
print_wp([W|Wall],[P|Pattern]):-length(P,LP),K is (5-LP)*4,print_space(K),print_horizontal(LP),write("    "),print_horizontal(5),nl,print_space(K),print_tiles(P),write(" >  "),print_tiles(W),nl,print_wp(Wall,Pattern).

print_space(0):-!.
print_space(K):-write(" "),M is K-1, print_space(M).

count_tiles_([],0,0,0,0,0):-!.
count_tiles_([blue|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue1,Red,Yellow,Cyan,Black), Blue is Blue1 + 1.
count_tiles_([red|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue,Red1,Yellow,Cyan,Black), Red is Red1 + 1.
count_tiles_([yellow|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue,Red,Yellow1,Cyan,Black), Yellow is Yellow1 + 1.
count_tiles_([cyan|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue,Red,Yellow,Cyan1,Black), Cyan is Cyan1 + 1.
count_tiles_([black|R],Blue,Red,Yellow,Cyan,Black):-count_tiles_(R,Blue,Red,Yellow,Cyan,Black1), Black is Black1 + 1.
    
print_bag_and_stack(Bag):- 
    count_tiles_(Bag,Blue,Red,Yellow,Cyan,Black), 
    write("-"),write(Blue),write(" "),ansi_format([bold,fg(blue)], 'O', []),nl,
    write("-"),write(Red),write(" "),ansi_format([bold,fg(red)], 'O', []),nl,
    write("-"),write(Yellow),write(" "),ansi_format([bold,fg(yellow)], 'O', []),nl,
    write("-"),write(Cyan),write(" "),ansi_format([bold,fg(cyan)], 'O', []),nl,
    write("-"),write(Black),write(" "),ansi_format([bold,fg(black)], 'O', []).



print_winners(P):-ansi_format([bold,fg(green)], '*WINNER(S)*', []),print_score_winners(P).

print_score_winners([]):-!.
print_score_winners([P|Players]):-player_state(P,Wall,_,_,Score),print_wall_and_pattern(Wall),nl,write("Player "),write(P),write("--> "),write(Score),write(" "),write("points"),print_score_winners([Players]).

print_score([]):-!.
print_score([P|Players]):-player_state(P,_,_,_,Score),write("Player "),write(P),write("--> "),write(Score),write(" "),write("points"),print_score([Players]).

print_horizontal(1):-write("+---+"),!.
print_horizontal(N):-write("+---"), M is N-1,print_horizontal(M).

print_vertical(0):-!.
print_vertical(N):-write("|"), M is N-1,print_vertical(M).

print_player(P,Wall,Pattern,Floor,Score):-write("Player "),write(P),nl,print_wp(Wall,Pattern),nl,nl,print_horizontal(7),nl,print_tiles(Floor),write("   "),write("Score: "),write(Score),nl,print_horizontal(7),!.

print_wp_double(P,[],[],[],[]):-
    print_horizontal(5),
    write("    "),print_horizontal(5),
    write("   ||   "),print_horizontal(5),
    write("    "),print_horizontal(5),!.

print_wp_double(P,[W1|WR1],[P1|PR1],[W2|WR2],[P2|PR2]):-
    length(P1,LP),K is (5-LP)*4,
    print_space(K),print_horizontal(LP),
    write("    "),print_horizontal(5),
    write("   ||   "),
    print_space(K),print_horizontal(LP),
    write("    "),print_horizontal(5),
    nl,
    print_space(K),print_tiles(P1),write(" >  "),
    print_tiles(W1),write("   ||   "),print_space(K),
    print_tiles(P2),write(" >  "),
    print_tiles(W2),nl, print_wp_double(P,WR1,PR1,WR2,PR2).

print_floor_double(F1,F2,S1,S2):- length(F1,FP),print_horizontal(FP),
write("                    ||   "), print_horizontal(FP),nl,print_tiles(F1), write("  Score: "),write(S1),
write("         ||   "),print_tiles(F2), write("  Score: "),write(S2), nl, print_horizontal(FP),
write("                    ||   "), print_horizontal(FP).